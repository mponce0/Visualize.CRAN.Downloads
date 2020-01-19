# downloadsStats-utils.R
#  -- M.Ponce




#################################################################################################
##	Utilities file for the Visualize.CRAN.Downloads package
#
#################################################################################################



### retrieve package data
retrievePckgData <- function(pckg=NULL, t0=lastyear.date(), t1=today()){
#' function to download the data from the CRAN logs for an specific package
#' @param  pckg  is the name of the package to look for the downloads data
#' @param  t0  is the initial date
#' @param  t1  is the final date
#' @return a list composed of the stats from the original time frame and the last month
#'
#' @importFrom cranlogs  cran_downloads
#' @export
#'
#' @examples
#' retrievePckgData("ggplot")
#' retrievePckgData("ggplot","2018-01-01","2019-01-01")

	# to access the logs from CRAN
	loadLibrary("cranlogs")

	# check package name
	if (is.null(pckg)) stop("Need a valid package name to process!")

	# check dates
	dates <- checkDates(t0,t1)
	t0 <- dates[[1]]
	t1 <- dates[[2]]

	# retrieve data
	# TOTAL
	pckg.stats.total <- cran_downloads(pckg, from=t0, to=t1)

	# Last Month
	#pckg.stats.lstmnt <- cran_downloads(pckg, when='last-month')

	# check whether there is indeed available data, ie. an indirect indication of misspelling a package's name 
	if (sum(pckg.stats.total$count) == 0) {
		warning("Package *",pckg,"* yields no data in CRAN logs during the specified period of time (",t0,"...",t1,"); check that you have specified the correct name for the package.")
		return(NULL)
	}

	# identify first not null entry in the set...
	i0t <- which(pckg.stats.total$count>0, arr.ind=TRUE)[1]
	#print(i0t)
	#print(pckg.stats.total)
	if (i0t <= 5) i0t <- 1
	#i0m <- which(pckg.stats.lstmnt$count>0, arr.ind=TRUE)[1]
	#if (i0m <= 5) i0m <- 1

	#return(list(pckg.stats.total[i0t:length(pckg.stats.total$count),],pckg.stats.lstmnt[i0m:length(pckg.stats.lstmnt$count),]))
	return(list(pckg.stats.total[i0t:length(pckg.stats.total$count),]))
}


### main wrapper fn
processPckg <- function(pckg.lst, t0=lastyear.date(), t1=today(), opts=list()) {
#' main function to analyze a list of packages in a given time frame
#' @param  pckg.lst  list of packages to process
#' @param  t0  initial date, begining of the time period
#' @param  t1  final date, ending of the time period
#' @param  opts  a list of different options available for customizing the output 
#'
#' @export
#'
#' @examples
#' processPckg("ehelp")
#' processPckg(c("ehelp","plotly","ggplot"), "2001-01-01")
#' processPckg(c("ehelp","plotly","ggplot"), "2001-01-01", opts="nostatic")
#' processPckg(c("ehelp","plotly","ggplot"), "2001-01-01",
#'		opts=c("nostatic","nocombined","nointeractive"))

	# verify options...
	checkOpts <- function(opts,validOpts){
	# function to check arguments
	# @param  opts  list of options
	# @param  validOpts  list of valid options
	#
	# @keywords internal
	# flag to identify invalid arguments...
		invalid <- FALSE

		# check every argument
		for (opt in tolower(opts)) {
			if (!(opt %in% validOpts)) {
				message('"',opt,'"', " not recognized among valid options for the function, it will be ignored")
				invalid <- TRUE
			}
		}

		# check whether is necessary to remind the user about the possible options for the arguments
		if (invalid) message("Valid options are: ", '"',paste(validOpts,collapse='" "'),'"')
	}


	# check dates
	dates <- checkDates(t0,t1)
	t0 <- dates[[1]]
	t1 <- dates[[2]]

	# check options...
	validOpts <- tolower(c("nostatic","nointeractive", "nocombined", "noMovAvg","noConfBand", "compare"))
	checkOpts(opts,validOpts)


	# initialize lists for storaging info
	pckgDwnlds.lst <- list()
	pckg.stats.lstmnt <- list()


	for (pckg in pckg.lst) {
		# retrieve data
		pckgDwnlds <- retrievePckgData(pckg,t0,t1)

		# get the total data
		pckg.stats.total <- pckgDwnlds[[1]]

		# Last Month data
		#pckg.stats.lstmnt <- pckgDwnlds[[2]]

		# check whether there is indeed available data, ie. an indirect indication of misspelling a package's name
		if ( is.null(pckgDwnlds) || (sum(pckg.stats.total$count) == 0) ) {
			warning("Package *",pckg,"* yields no data in CRAN logs during the specified period of time (",t0,"...",t1,"); check that you have specified the correct name for the package.")
		} else {
			### Plots
			## static plots
			if ('nostatic' %in% tolower(opts)) {
				message("Will skip static plots...")
			} else {
				# set defaults
				cmb <- TRUE; noCBs <- FALSE; noMAvgs <- FALSE

				# alternate depending on options provided by the user
				if ('nocombined' %in% tolower(opts))  cmb <- FALSE
				if ('noconfband' %in% tolower(opts))  noCBs <- TRUE
				if ('nomovavg' %in% tolower(opts))  noMAvgs <- TRUE

				staticPlots(pckg.stats.total, combinePlts=cmb, noMovAvg=noMAvgs, noConfBands=noCBs)
			}

			### interactive plots
			if ( "nointeractive" %in% tolower(opts) ) {
				message("will skip interactive plots...")
			} else {
				interactivePlots(pckg.stats.total)
			}

			### comparison between several packages
			if ( "compare" %in% tolower(opts) ) {
				if (length(pckg.lst) <= 1)
					warning("The 'compare' option is available when more than 1 package is indicated!")
				pckgDwnlds.lst <- c(pckgDwnlds.lst,pckgDwnlds)
			}

			# summaries
			#summaries(pckg.stats.total,pckg.stats.lstmnt)
			summaries(pckg.stats.total)
		}
	}

	if ( "compare" %in% tolower(opts) ) {
		#print(str(pckgDwnlds.lst))
		#lapply(pckgDwnlds.lst,summary)

		nbrPckgs <- length(pckgDwnlds.lst)
		dates.min <- c()
		dates.max <- c()
		counts.min <- c()
		counts.max <- c()
		pckgNames <- c()
		for (j in 1:nbrPckgs) {
			#print(pckgDwnlds.lst[[j]]["date"][[(sapply(pckgDwnlds.lst[[j]]["date"],min))]])
			##dates.min <- c(dates.min, sapply(pckgDwnlds.lst[[j]]["date"],min))
			##dates.max <- c(dates.max, sapply(pckgDwnlds.lst[[j]]["date"],max))
			#dates.min <- c(dates.min, as.character(pckgDwnlds.lst[[j]]["date"][[1]]))
			#dates.max <- c(dates.max, as.character(pckgDwnlds.lst[[j]]["date"][[length(pckgDwnlds.lst[[j]]["date"])]]))
			counts.min <- c(counts.min, sapply(pckgDwnlds.lst[[j]]["count"],min))
			counts.max <- c(counts.max, sapply(pckgDwnlds.lst[[j]]["count"],max))
			#pckgNames <- c(pckgNames, sapply(pckgDwnlds.lst[[j]]["package"],unique))
			pckgNames <- c(pckgNames, pckgDwnlds.lst[[j]][["package"]][[1]])
		}


		min.date <- as.Date(as.character(t0))
		max.date <- as.Date(as.character(t1))
		xrange <- c(min.date,max.date)
		#min.date <- min(dates.min)
		#max.date <- max(dates.max)
		counts.min <- (min(counts.min))
		counts.max <- (max(counts.max)*1.05)
		yrange <- c(counts.min,counts.max)
		#print(xrange)
		#print(yrange)

		fileName <- paste0("DWNLDS_",paste(pckgNames,collapse='-'),".pdf")
		message("Combined plots for packges: ",paste(pckgNames,collapse=' ')," will be saved in ",fileName)
		pdf(fileName)
		par(mar=c(3,2.5,0.85,0.5))
		#plot(pckgDwnlds.lst[[1]]$date, pckgDwnlds.lst[[1]]$count, 'n')
		if (cmb)  plot(xrange,yrange, 'n', xlim=xrange, ylim=yrange, ann=FALSE, axes=FALSE)

		for (j in 1:length(pckgDwnlds.lst)) {
			if (cmb) par(new=TRUE)
			yvar <- (pckgDwnlds.lst[[j]]$count)
			if (!cmb) yrange <- c(min(yvar),max(yvar))
			plot(pckgDwnlds.lst[[j]]$date, yvar, 'o', col=j, lwd=0.35, cex=0.35,
				#label=pckgDwnlds.lst[[j]]$package[[1]],
				xlim=xrange, ylim=yrange,
                        	ann=!cmb, axes=!cmb )

			if (!noCBs) confBand(pckgDwnlds.lst[[j]]$date,pckgDwnlds.lst[[j]]$count,
                                t0,t1, 0,counts.max,
                                ,length(pckgDwnlds.lst[[j]]$count),
                                j,,1.25, filling=FALSE)

			if (!noMAvgs) confBand(pckgDwnlds.lst[[j]]$date,pckgDwnlds.lst[[j]]$count,
				t0,t1, 0,counts.max,
				,,
				j,,0.5, filling=TRUE)

			legend("top", pckgNames,
				cex=0.5,box.lty=0, pch=1,lty=1,pt.cex=0.5,
				col=1:length(pckgDwnlds.lst),border='white')
		}

		#print(min.date)
		axis.Date(1,at = seq(min.date, max.date, by=T.unit))
		axis(2)
		#box()

		dev.off()

		return(invisible(pckgDwnlds.lst))
	}
}




# summaries
summaries <- function(data1, deltaTs=30) {
#' function to display the summary of the data
#' @param  data1  first dataset, eg. total data
#' @param  deltaTs  a numerical (integer) value, indicating the lenght --in days-- for selecting a subset of the original dataset; default value is 1 mont, ie. 30 days
#'
#' @export
#'
#' @examples
#' packageXdownloads <- retrievePckgData("ehelp")[[1]]
#' summaries(packageXdownloads)

	printSummary <- function(data, time.orig="",hdr="--- \n") {


		# some descriptive statistics first,
		# ie. min, max, mean, median, ...

		cat("Stats from ",time.orig, paste0("(",length(data$date)-2," days)"), '\n')
        	cat(paste("period: ",data$date[1],' - ',data$date[length(data$date)]),'\n')
        	cat(hdr)

        	#print(summary(data))
		print(summary(data$count))
		cat('\n')
        	
		indicators <- c("Min.","Median","Mean","Max.")
        	selectedIndicators <- c("Min.","Max.")
        	sum1 <- summary(data$count)

		for (ind in indicators) {
			if (ind %in% selectedIndicators) {
				info <- paste('  ',paste(ind, sum1[ind], "__", data$date[which(data$count == sum1[ind])]),'\n')
			} else {
				info <- paste('  ',paste(ind, round(sum1[ind],2)),'\n')
			}
			cat(info)
		}
	}


	###########################
	##### EXPERIMENTAL   ######
	###########################
	statAnal <- function(data){
		# some statistical tests to check whether the downloads are following
		# a random pattern from a normal/poisson/... distribution

		t.tst <- t.test(data$count)
		shapiro.tst <- shapiro.test(data$count)
		xi2.tst <- chisq.test(data$count)

		#cat(t.tst$method)
		#cat(paste('\t',t.tst$statistic,"  --  ",t.tst$p.value,'\n'))
		print(t.tst)
		print(shapiro.tst)
		print(xi2.tst)
	}
	############################
	############################


	# define some headers and line-breaks...
	hdr <- paste(paste(rep("#",70),collapse=''), '\n')
	hdr2 <- paste(paste(rep("-",35),collapse=''), '\n')
	hdr3 <-  paste(paste(rep("~",60),collapse=''), '\n')

	# display info about the package...
	cat('\n'); cat(hdr)
	line1 <- paste("Processing Package",data1$package[1])
	len.line1 <- nchar(line1)
	p.line1 <- paste(paste(rep("#",(70-len.line1-2)/2),collapse=''))
	cat(p.line1,line1,p.line1,'\n')
	cat(hdr)

	printSummary(data1,"original selected period",hdr2)
	cat(hdr3)

	tot.days <- length(data1$date)

	deltaTs <- time.units(tot.days)
	#print(deltaTs)
	deltas.units <- c("day","week","month","trimester","semester","year","2 years","5 years")

        for (deltaT.ind in seq_along(deltaTs)) {
		deltaT <- deltaTs[deltaT.ind]
		#print(deltaT)
		if (deltaT > 1) {
			cat(hdr2)
			#cat("Stats from last",deltas.units[deltaT.ind], paste0("(",deltaT," days)"), '\n')
			emph.range <- (tot.days-(deltaT+1)):(tot.days)
			#print(emph.range)
			data2 <- data1[emph.range,]

			printSummary(data2,paste("last",deltas.units[deltaT.ind]),hdr2)
		}
	}

	# final line...
	cat(hdr)
	cat('\n\n\n')
}


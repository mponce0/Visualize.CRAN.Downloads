# downloadsStats-utils.R
#  -- M.Ponce


#################################################################################################
##	Utilities file for the Visualize.CRAN.Downloads package
#
#################################################################################################


### date aux fns
today <- function() {
#' function that returns the current date 
#' @keywords internal
	return(Sys.Date())
}

lastyear.date <- function() {
#' function that returns the date from one year ago
#' @keywords internal
	cur.date <- Sys.Date()
	cur.year <- substr(cur.date,1,4)
	lst.year <- as.integer(cur.year) - 1
	t0 <- paste(lst.year,substr(cur.date,5,10),sep="")

	message("Starting date was not specified, will assume a year from now: ",t0)
	return(t0)
}

# load and check needed packages/libraries...
loadLibrary <- function(lib) {
#' function to check and load an specific set of libraries
#' @param  lib  is a list of packages to be loaded
#' @keywords internal
	if (require(lib,character.only = TRUE) == FALSE) stop(lib, " is needed for this package to work but is not installed in your system!")
}



### retrieve package data
retrievePckgData <- function(pckg=NULL, t0=lastyear.date(), t1=today()){
#' function to download the data from the CRAN logs for an specific package
#' @param  pckg  is the name of the package to look for the downloads data
#' @param  t0  is the initial date
#' @param  t1  is the final date
#' @return a list compose of the stats from the original time frame and the last month
#'
#' @importFrom cranlogs  cran_downloads
#' @export
#'
#' @examples
#' retrievePckgData("ggplot")
#' retrievePckgData("ggplot","2018-01-01","2019-01-01")

	# to access the logs from CRAN
	loadLibrary("cranlogs")

	if (is.null(pckg)) stop("Need a valid package name to process!")


	# retrieve data
	# TOTAL
	pckg.stats.total <- cran_downloads(pckg, from=t0, to=t1)

	# Last Month
	pckg.stats.lstmnt <- cran_downloads(pckg, when='last-month')

	# identify first not null entry in the set...
	i0t <- which(pckg.stats.total$count>0, arr.ind=TRUE)[1]
	if (i0t <= 5) i0t <- 1
	i0m <- which(pckg.stats.lstmnt$count>0, arr.ind=TRUE)[1]
	if (i0m <= 5) i0m <- 1

	return(list(pckg.stats.total[i0t:length(pckg.stats.total$count),],pckg.stats.lstmnt[i0m:length(pckg.stats.lstmnt$count),]))
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


	# check dates...
	

	# check options...
	validOpts <- c("nostatic","nointeractive", "nocombined", "compare")
	checkOpts(opts,validOpts)

	# initialize lists for storaging info
	pckgDwnlds <- list()
	pckg.stats.lstmnt <- list()

	for (pckg in pckg.lst) {
		# retrieve data
		pckgDwnlds <- retrievePckgData(pckg,t0,t1)
		
		# get the total data
		pckg.stats.total <- pckgDwnlds[[1]]

		# Last Month data
		pckg.stats.lstmnt <- pckgDwnlds[[2]]

		# check whether there is indeed available data, ie. an indirect indication of misspelling a package's name
		if (sum(pckg.stats.total$count) == 0) {
			warning("Package *",pckg,"* yields no data in CRAN logs during the specified period of time (",t0,"...",t1,"); check that you have specified the correct name for the package.")
		} else {
			### Plots
			## static plots
			if ('nostatic' %in% tolower(opts)) {
				message("Will skip static plots...")
			} else {
				if ('nocombined' %in% tolower(opts)) {
					cmb <- FALSE
				} else {
					cmb <- TRUE
				}
				staticPlots(pckg.stats.total,pckg.stats.lstmnt, combinePlts=cmb)
			}

			### interactive plots
			if ( "nointeractive" %in% tolower(opts) ) {
				message("will skip interactive plots...")
			} else {
				interactivePlots(pckg.stats.total)
			}

			### comparison between several packges


			# summaries
			summaries(pckg.stats.total,pckg.stats.lstmnt)
		}
	}
}


### static plots
staticPlots <- function(pckg.stats.total,pckg.stats.lstmnt,
			fileName=paste0("DWNLDS_",pckg.stats.total$package[1],".pdf"), combinePlts=FALSE){
#' function that generates visual trends of the package downloads logs from CRAN, it will generate 4 plots: two histograms, a pulse plot and the main plot is a plot of the downloads as a function of time
#' @param  pckg.stats.total  total downloads from the package
#' @param  pckg.stats.lstmnt  last month downloads
#' @param  fileName  an optional string argument specifying the name of the file where to save the plots
#' @param  combinePlts a boolean indicating whether the plots generated will be combined into one single figure or not
#'
#' @importFrom grDevices  dev.off pdf
#' @importFrom graphics  abline axis axis.Date hist
#'                       lines par plot points text
#' @importFrom stats  sd
#'
#' @export
#'
#' @examples
#' packageData <- retrievePckgData("ggplot")
#' totalDownloads <- packageData[[1]]
#' lastmonthDownloads <- packageData[[2]]
#' staticPlots(totalDownloads,lastmonthDownloads)
#' staticPlots(totalDownloads,lastmonthDownloads,combinePlts=TRUE)

	# define some useful quantities
	max.downloads <- max(pckg.stats.total$count)
	max.dwlds.date <- pckg.stats.total$date[pckg.stats.total$count == max.downloads]
	fst.date <- pckg.stats.total$date[1]
	tot.days <- length(pckg.stats.total$date)
	mnt.days <- length(pckg.stats.lstmnt$date)
	lst.date <- pckg.stats.total$date[tot.days]
	pckgName <- pckg.stats.total$package[1]

	# reporting package name...
	message("Processing package ",pckgName)

	# informing where the plot is going to be saved
	message("Saving static plots in ",fileName)

	# open PDF file
	pdf(fileName)

	if (combinePlts) {
		par(new=TRUE)
		par(mar=c(4.5,3,1.5,2))
		par(mfrow=c(4,3))
	}
        ### histogram
        # bins in units of weeks
        bins.days <- as.integer(tot.days/7)
	# bins in units of months
	bins.mnt <- as.integer(tot.days/30)
	# bins in units of trimesters
	bins.3mnts <- as.integer(tot.days/120)
	# bins in units of semesters
	bins.6mnts <- as.integer(tot.days/180)

	bins <- bins.mnt

	####### subplots ############

	# subplot #1: histogram per date
        hist(pckg.stats.total$date,pckg.stats.total$count, freq=T, breaks=bins, ann=FALSE,
		col='gray', main="Downloads histogram")

	# subplot #2: histogram per nbr of downloads
	#if (combinePlts) par(mfg=c(2,1))
	hist(pckg.stats.total$count, freq=T, ann=FALSE, col='lightgray', main="")

	# pulse plot
	plot(pckg.stats.total$date,(pckg.stats.total$count), type="S", ann=FALSE)

	#############################

	# reset canvas to 1 plt per page
	par(mfrow=c(1,1))
	if (combinePlts) {
		par(new=TRUE)
		par(mar=c(2.5,1.5,7.5,2.5))
	}

	### plotting downloads per day
	plot(pckg.stats.total$date, (pckg.stats.total$count), 'b',
		xlim=c(fst.date,lst.date),
		ylim=c(0,max.downloads*1.05) ,
		ann=FALSE, axes=FALSE )

	# print some stats in the plot
	text(pckg.stats.total$date[as.integer(tot.days*0.15)],.95*max(pckg.stats.total$count),
		paste0('"',pckgName,'"'," Package",'\n', fst.date," -- ",lst.date,'\n',
			"Total downloads: ",sum(pckg.stats.total$count),'\n',
			"Tot.Avg: ",round(mean(pckg.stats.total$count),digits=2),'\n',
			"Last month: ",sum(pckg.stats.lstmnt$count),'\n',
			"LMAvg: ",round(mean(pckg.stats.lstmnt$count),digits=2)
		)
	)

	# emphasize last month data
	par(new=TRUE)
	plot(pckg.stats.lstmnt$date, (pckg.stats.lstmnt$count),
		'b', col='darkblue', lwd=2,
		ann=FALSE, axes=FALSE,
		xlim=c(fst.date,lst.date),
		ylim=c(0,max.downloads*1.05) )


	# stats from last month
	mean.lstmnt <- mean(pckg.stats.lstmnt$count)
	sd.lstmnt <- sd(pckg.stats.lstmnt$count)
	message(paste("Average downloads last month: ",round(mean.lstmnt,digits=2),"; sd=",round(sd.lstmnt,digits=2)))
	# avergage last month
	lines(pckg.stats.lstmnt$date,rep(mean.lstmnt,mnt.days), type='l', lwd=2, col='darkblue',
		ann=FALSE,
		xlim=c(fst.date,lst.date),
		ylim=c(0,max.downloads*1.05) )
	#text(pckg.stats.lstmnt$date[2],1.075*mean.lstmnt, paste(as.integer(mean.lstmnt)), col='darkblue' )
	text(lst.date,1.075*mean.lstmnt, paste(as.integer(mean.lstmnt)), col='darkblue' )

#        if (combinePlts) {
                #axis(side=1,
		#	at=pckg.stats.total$date[seq_along(pckg.stats.total$date)%%6==0],
		#	labels=as.character.Date(pckg.stats.total$date[seq_along(pckg.stats.total$date)%%6==0], "%d-%m-%y")
		#)
		axis.Date(1, at = seq(min(pckg.stats.total$date), max(pckg.stats.total$date), "weeks"))
                axis(side=4)
#        } else {
#                axis(side=c(1:4))
#        }


	mean.total <- mean(pckg.stats.total$count)
	sd.total <- sd(pckg.stats.total$count)

	# total average
	abline(h=mean.total, lt=2, col='black')
	text(pckg.stats.total$date[as.integer(tot.days*0.05)],1.1*mean.total, paste("avg = ",as.integer(mean.total)) )

	# add maximum download
	points(max.dwlds.date,max.downloads, col='darkred', pch=19)
	text(max.dwlds.date,max.downloads*1.035,max.downloads, col='darkred')


	# Close file
	dev.off()
}



### interactive plots
interactivePlots <- function(downloads.data, mytitle=paste(downloads.data$package[1],"Package downloads counts"),
				nbrPlts = 2, month.ln=31,
				HTMLfile=paste0("Interactive_DWNLDS_",downloads.data$package[1],"stats.html") ) {
#' function that generates interactive plots of the package downloads logs from CRAN
#' @param  downloads.data  total downloads from the package
#' @param  mytitle  optional char argument specifying the title to be displayed
#' @param  nbrPlts  optional numeric argument specifying number of plots to generate
#' @param  month.ln optional numeric argument specifying the lenght of the month in days
#' @param  HTMLfile  an optional string argument specifying the name of the file where to save the plots
#'
#' @importFrom  plotly  %>% add_annotations add_trace as_widget plot_ly subplot layout
#' @importFrom  htmlwidgets  saveWidget
#' @export
#'
#' @examples
#' packageXdownloads <- retrievePckgData("ggplot")[[1]]
#' interactivePlots(packageXdownloads)
       
        loadLibrary("plotly")

	tot.days <- length(downloads.data[,1])

	if (tot.days > month.ln) {
		mnth.rng <- (tot.days-month.ln):tot.days
		lst.mnth <- downloads.data[mnth.rng,]
	} else {
		lst.mnth <- downloads.data
		mnth.rng <- 1:tot.days
		month.ln <- tot.days
	}

        p1 <- plot_ly() %>%
		add_trace(data = downloads.data, x = ~date, y = ~count,
		mode="lines", fill = 'tozeroy', alpha=0.25, name="dwnlds",
                marker = list(size = ~count/max(sqrt(count)),
			color = ~count, text = paste("downloads: ",~count), 
			line = list(color = 'rgba(152, 0, 0, .8)', width = 2))) %>%
		add_trace(x =~date[mnth.rng], y = ~count[mnth.rng], mode="lines+markers", name="LMD", line=list(width=3.5), fill='tozeroy') %>%
		#add_ribbons(x = lst.mnth$date, ymin = lst.mnth$count*0.95, ymax = lst.mnth$count*1.05, color = I("gray95"), name = "last month") %>%
		add_annotations(
                        x=0.15,y=1.00, xref="paper",yref="paper",
                        text = paste("Downloads in the last month: ",'<b>',sum(lst.mnth$count),'</b>'), showarrow = F ) %>%
		add_annotations(
			x=0.15,y=0.950, xref="paper",yref="paper",
			text = paste("Total downloads: ",'<b>',sum(downloads.data$count),'</b>'), showarrow = F ) %>%
		add_annotations(
			x=0.15,y=0.975, xref="paper",yref="paper",
			text = paste("Avg dwnlds: ",'<b>',round(mean(lst.mnth$count),digits=2),'</b>'), showarrow = F ) %>%
		add_annotations(
			x=0.15,y=0.925, xref="paper",yref="paper",
			text = paste("Avg per day: ",'<b>',round(mean(downloads.data$count),digits=2),'</b>'), showarrow = F ) %>%
          layout(title = mytitle,
                 xaxis = list(zeroline = TRUE), #range=c(downloads.data$date[1],downloads.data$date[tot.days])),
                 yaxis = list(zeroline = TRUE))
        print(p1)

	if (nbrPlts == 2) {
		p2 <- plot_ly(data = downloads.data, x = ~date) %>% 
			#add_histogram(y = ~count) %>%
			add_trace(y = ~count, color = ~count, size=~count/max(sqrt(count)),
				mode="markers+line", fill="tozeroy",
#				text = paste("downloads: ",~count)
#				 ) %>%
					marker=list(colorscale="Blues",reversescale=TRUE) ) %>%
			add_trace(y = ~count, color=~count, mode = 'lines', fill = 'tozeroy')     %>%
			layout(title = mytitle,
				yaxis = list(zeroline = TRUE),
				xaxis = list(range = c(downloads.data$date[1],downloads.data$date[length(downloads.data$date)])) )
#				colorscale="Blues", showscale = FALSE, showlegend = TRUE)
			print(p2)

	p <- subplot(p1,p2)
	} else {
		p <- p1
		}

	htmlwidgets::saveWidget(as_widget(p), HTMLfile)

	return(p)
}


# summaries
summaries <- function(data1,data2) {
#' function to display the summary of the data
#' @param  data1  first dataset, eg. total data
#' @param  data2  second dataset, eg. last month data
#'
#' @export
#'
#' @examples
#' packageXdownloads <- retrievePckgData("ehelp")[[1]]
#' packageXlastmonth <- retrievePckgData("ehelp")[[2]]
#' summaries(packageXdownloads,packageXlastmonth)

	hdr <- paste(paste(rep("#",70),collapse=''), '\n')
	hdr2 <- paste(paste(rep("-",35),collapse=''), '\n')
	hdr3 <-  paste(paste(rep("~",60),collapse=''), '\n')

	cat(hdr)
	cat(paste("period: ",data1$date[1],' - ',data1$date[length(data1$date)]),'\n')
	cat(hdr2)
	print(summary(data1))
	cat(hdr3)
	cat(paste("period: ",data2$date[1],' - ',data2$date[length(data2$date)]),'\n')
	cat(hdr2)
	print(summary(data2))
	cat(hdr)
}

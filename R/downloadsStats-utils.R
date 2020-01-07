# downloadsStats-utils.R
#  -- M.Ponce




#################################################################################################
##	Utilities file for the Visualize.CRAN.Downloads package
#
#################################################################################################


### date's aux fns
today <- function() {
#' function that returns the current date 
#' @keywords internal
	t1 <- Sys.Date()
		message("Ending date was not specifed, will assume today: ",t1)
		return(t1)
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

checkDates <- function(t0,t1) {
#' function to check dates, ie that t0<t1 and t0!=t1
#' @param  t0  initial date
#' @param  t1  final date
#' @return a list with t0 being [[1]] and t1 being [[2]]
#'
#' @keywords internal
# check whether t0 is greater than t1
	if (as.Date(t0) > as.Date(t1)) {
# flip dates, t0 will be set to the older date
		ttemp <- t0
			t0 <- t1
			t1 <- ttemp
	} else if (as.Date(t0) == as.Date(t1)) {
# dates should be different
		stop(t0," and ", t1," should be different!")       
	}
	return(list(t0,t1))
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
		print(i0t)
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
	validOpts <- tolower(c("nostatic","nointeractive", "nocombined", "noMmovinBand","noConfBands", "compare"))
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

				# alternate depending on optiosn provided by the user
				if ('nocombined' %in% tolower(opts))  cmb <- FALSE
				if ('noconfbands' %in% tolower(opts))  noCBs <- TRUE
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


			# summaries
			#summaries(pckg.stats.total,pckg.stats.lstmnt)
			summaries(pckg.stats.total)
		}
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

	hdr <- paste(paste(rep("#",70),collapse=''), '\n')
	hdr2 <- paste(paste(rep("-",35),collapse=''), '\n')
	hdr3 <-  paste(paste(rep("~",60),collapse=''), '\n')

	tot.days <- length(data1$date)
        emph.range <- (tot.days-(deltaTs+1)):(tot.days)
        data2 <- data1[emph.range,]

	cat(hdr)
	line1 <- paste("Processing Package",data1$package[1])
	len.line1 <- nchar(line1)
	p.line1 <- paste(paste(rep("#",(70-len.line1-2)/2),collapse=''))
	cat(p.line1,line1,p.line1,'\n')
	cat(hdr)
	cat(paste("period: ",data1$date[1],' - ',data1$date[length(data1$date)]),'\n')
	cat(hdr2)
	print(summary(data1))
	cat(hdr3)
	cat(paste("period: ",data2$date[1],' - ',data2$date[length(data2$date)]),'\n')
	cat(hdr2)
	print(summary(data2))
	cat(hdr)
	cat('\n\n\n')
}

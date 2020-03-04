# downloadsStats-plotting-utils.R
#  -- M.Ponce



#################################################################################################
##	Plotting utilities for the Visualize.CRAN.Downloads package
#
#################################################################################################


check.FileDir <- function(dirSave=NULL,fileName) {
#' function to check the existance of the directory and combine directory and file names
#'
#' @param  dirSave  specify a valid directory where to save the file
#' @param  fileName  name of the file
#'
#' @return  directory and filename combined, ie path to fileName
#'
#' @keywords internal

	if (missing(fileName)) stop("Please indicate a filename!")

	# check whether dirSave was specified...
	if (!is.null(dirSave)) {
		# Check dir existance
		if (!dir.exists(dirSave)) {
			stop(paste0("'",dirSave,"'")," is not a valid directory!")
		} else {
			FileName <- file.path(paste(paste(dirSave,"/",fileName, sep="")))
		}
	} else {
		FileName <- file.path(fileName)
	}

	return(FileName)
}


outputType <- function(device, dirSave=NULL,fileName, fileFmts, combinePlts, msg="plot") {
#' function to validate the type of file format and open the corresponding file
#'
#' @param  device  string to select the output format: 'PDF'/'PNG'/'JPEG' or 'screen'
#' @param  fileName  name of the file
#' @param  dirSave  specify a valid directory where to save the file
#' @param  fileFmts  list with the acceptable format types, eg. c("PDF","PNG","JPEG")
#' @param  combinePlts  boolean flag indicating whether plots are combined or not
#' @param  msg  optional string to customiza message to the user
#'
#' @importFrom grDevices dev.new jpeg png
#'
#' @keywords internal

	# predefined quality for "raster" figures (PNG/JPEG)
	raster.quality <- list(width = 7, height = 7, units = 'in', res = 300)

	# Check dir existance
	FileName <- check.FileDir(dirSave,fileName)

	# informing where the plot is going to be saved
	if (toupper(device) %in% fileFmts) message("Saving ",msg," in ", FileName)
 
	# open PDF file
	if (toupper(device)=="PDF") {
		pdf(FileName)
	} else if (toupper(device) %in% c("PNG","JPEG")) {
		if (combinePlts) {
			if (toupper(device)=="PNG") {
				message("PNG file format indicated. \n We recommend using PDF for optimized quality instead.")
				#png(fileName, raster.quality)
				do.call(png, c(FileName,raster.quality))
			} else {
				do.call(jpeg, c(FileName,raster.quality))
				message("JPEG file format indicated. \n We recommend using PDF for optimized quality instead.")
			}
		# if plots are not combined is NOT doable with PNG/JPEG
		} else {
			stop("PNG and JPEG formats do not support separated plots in one file! \n For plots in separated pages, please use instead 'PDF'")
		}
	} else if (toupper(device)=="SCREEN") {
		message("Plots will be displayed in screen.")
		dev.new()
	} else {
		warning("The argument 'device' has an unrecognized value ",device,'\n',
			"Valid options are: ",paste(fileFmts,collapse=" ")," or 'SCREEN'.",'\n',
			"Graphics will be displayed in screen...")
		dev.new()
	}
}


######################


confBand <- function(x,y, x0,x1,y0,y1, windowsNbr=10, period=ceiling(length(y)/windowsNbr), lcolour='gray',ltype=4,lwidth=2, filling=TRUE) {
# function to draw confidence bands, using generalized moving averages/sds
# importFrom  grDevices  rgb
# importFrom  graphics polygon
# @keywords internal

                lineWrapper <- function(x,y, x0,x1,y0,y1, line.col,line.lt,line.wdt) {
                # wrapper function to draw lines
                        lines(x,y, col=line.col, lty=line.lt, lwd=line.wdt,
                                xlim=c(x0,x1), ylim=c(y0,y1), ann=FALSE)
                }

                ym <- movingFn(y,mean,period)
                ysd <- movingFn(y,sd,period)

                lineWrapper(x,ym, x0,x1,y0,y1, lcolour,ltype,lwidth)
                lineWrapper(x,ym+(ysd/2), x0,x1,y0,y1, lcolour,ltype+1,lwidth/2)
                lineWrapper(x,ym-(ysd/2), x0,x1,y0,y1, lcolour,ltype+1,lwidth/2)

                # shading of the confidence region
                if (filling){
                        xprime <- c(x,rev(x))
                        yprime <- c((ym+(ysd/2)),rev(ym-(ysd/2)))
                        if(sum(is.na(yprime))>0)
                                yprime[which(is.na(yprime))] <- yprime[min(which(is.na(yprime))-1)]
                        graphics::polygon(xprime,yprime, col=grDevices::rgb(0.5,0.5,0.5, .25), border=NA)
                }
        }

######################

axes.TimePlt <- function(tot.days,pckg.stats.total,yaxis.side=4) {

	time.int <- time.intervals(tot.days)
	T.unit <- time.int[[1]]
	everyT <- time.int[[2]]

	selectDates <- as.Date(pckg.stats.total$date[seq_along(pckg.stats.total$date) %% everyT == 0])
	selectDates.labels <- paste0( substr(month.name[as.integer(substr(selectDates,6,7))],1,3) )
                                        #       ,"-", substr(selectDates,1,4) )

	axis.Date(1,at = seq(min(pckg.stats.total$date), max(pckg.stats.total$date), by=T.unit))

	if (tot.days-1 > 365) {
		#axis(1,at=NULL, labels=F)
		#text(x = selectDates, par("usr")[3]*.97, labels = substr(month.name[as.integer(substr(selectDates,nchr0,nchr1))],1,3), pos = 1, xpd = TRUE,cex=.85)
		text(x = selectDates, par("usr")[3]*.995, labels = selectDates.labels, srt = 45, pos = 1, xpd = TRUE,cex=.65)
	}
	if (yaxis.side != 0) axis(side=yaxis.side)

}

######################


### static plots
staticPlots <- function(pckg.stats.total, #pckg.stats.lstmnt,
		device="PDF",
		fileName=paste0("DWNLDS_",pckg.stats.total$package[1],".",tolower(device)),
		dirSave=NULL,
		combinePlts=FALSE, noMovAvg=FALSE, noConfBands=FALSE,
		cutOff.pts=250, dbg=FALSE){
#' function that generates visual trends of the package downloads logs from CRAN, it will generate 4 plots: two histograms, a pulse plot and the main plot is a plot of the downloads as a function of time
#' @param  pckg.stats.total  total downloads from the package
#' @param  device  string to select the output format: 'PDF'/'PNG'/'JPEG' or 'screen'
#' @param  fileName  an optional string argument specifying the name of the file where to save the plots
#' @param  dirSave  specify a valid directory where to save the plot
#' @param  combinePlts a boolean indicating whether the plots generated will be combined into one single figure or not
#' @param  noMovAvg  a boolean indicating whether moving statistical estimators, such as, the moving average will be displayed
#' @param  noConfBands  a boolean indicating whether a confidence band will be displayed
#' @param  cutOff.pts  an integer value indicating the cut-off value to determine whether there would be a subsample for clarity sake in the plots
#' @param  dbg  internal flag for activating debugging options, i.e. display furhter information in screen
#'
#' @importFrom grDevices  dev.off pdf
#' @importFrom graphics  abline axis axis.Date hist
#'                       lines par plot points text
#' @importFrom stats  sd
#'
#' @export
#'
#' @examples
#' \donttest{
#' packageData <- retrievePckgData("ggplot2")
#' totalDownloads <- packageData[[1]]
#' #lastmonthDownloads <- packageData[[2]]
#' staticPlots(totalDownloads, device="screen")
#' staticPlots(totalDownloads,combinePlts=TRUE, device="screen")
#' }
#'

######################
	emphasize <- function(x,y, delta, x0,x1,y0,y1, line.color="darkblue",line.wdth=2, subsample=FALSE) {
	# function to add trends in subsequent ranges of dates, eg. average in the last month
	# @keywords internal

		tot.lng <- length(x)
		emph.range <- max(0,tot.lng-(delta+0)):(tot.lng)
		x.range <- x[emph.range]
		y.range <- y[emph.range]
		if (subsample) {
			   x.range <- x[(1:tot.lng) %% 2 == 0]
			   y.range <- y[(1:tot.lng) %% 2 == 0]
			   print("sbs")
			   print(x.range)
			   print(y.range)
		}
		par(new=TRUE)
		plot(x.range,y.range,
			'b', cex=.3, col=line.color, lty=1, lwd=0.25,  #line.wdth,
			ann=FALSE, axes=FALSE,
			xlim=c(x0,x1),
			ylim=c(y0,y1) )

		# add average for the period
		range.meanVal <- mean(y[emph.range])
		lines(x.range,rep(range.meanVal,length(x.range)), type='l', lty=1, lwd=(line.wdth/2*3), col=line.color,
			ann=FALSE,
			xlim=c(x0,x1),
			ylim=c(y0,y1) )

		# add text with mean value
			text(x[sample(c(emph.range[1]+1,tot.lng-1),1)],
					1.075*range.meanVal,
					#sample(c(0.95,1.075),1)*range.meanVal,
					paste(as.integer(range.meanVal)),
					col=line.color, cex=1.175 )
	}

	######################

	### preserve user graphical env.
	# save the state of par() before running the code
	oldpar <- par(no.readonly = TRUE)
	# restore the previous state after the fn is done, even if it fails, so the user environment is not altered
	on.exit(par(oldpar))
	###

	# supported file formats
	fileFmts <- c("PDF","PNG","JPEG")

	# define some useful quantities
	max.downloads <- max(pckg.stats.total$count)
	max.dwlds.date <- pckg.stats.total$date[pckg.stats.total$count == max.downloads]
	fst.date <- pckg.stats.total$date[1]
	tot.days <- length(pckg.stats.total$date)
	lst.date <- pckg.stats.total$date[tot.days]
	pckgName <- pckg.stats.total$package[1]

	# will assume 30 days-long months
	mnt.days <- 30	#length(pckg.stats.lstmnt$date)
	if (dbg) print(max(tot.days-(mnt.days+0),1))
	lstmnt.range <- max(tot.days-(mnt.days+0),1):tot.days
	pckg.stats.lstmnt <- pckg.stats.total[lstmnt.range,]

	# graphical params
	# cutOff.pts <- 250

	# define a time unit vector, for
	#	days / weeks / months / trimesters / semesters / years / 2-years / 5-years
	time.units <- c(1, 7, 30, 90, 180, 365, 365*2, 365*5)
	# identify time intervals in range of dates
	period.units <- tot.days/time.units
	periods    <- period.units > 1
	if (dbg) print(periods)

	# reporting package name...
	message("Processing package ",pckgName)

	# check format of output selected and report where the plot is going to be saved/displayed
	outputType(device, dirSave,fileName, fileFmts, combinePlts, msg="static plots")

	if (combinePlts) {
		#par(new=TRUE)
		par(mar=c(4.5,3,1.5,2))
		par(mfrow=c(4,3))
	}

        ### histogram
        # bins in units of weeks
        bins.days <- as.integer(tot.days/7)
	# bins in units of months
	bins.mnt <- as.integer(tot.days/30)
	# bins in units of trimesters
	bins.3mnts <- as.integer(tot.days/90)
	# bins in units of semesters
	bins.6mnts <- as.integer(tot.days/180)

	###
	bins.units <- as.integer(period.units)

	# select the bins according to the largest unit within the range
	##if (tot.days >=  mnt.days) {
		bins <- mean(bins.units[ceiling(max(which(periods))/1.)]+1, sqrt(tot.days), 15)
	##} else {
	#bins <- bins.mnt
	#bins <- sqrt(tot.days)/2
	##	bins <- ceiling(tot.days/2)
	##}

	if (dbg) {
		print(tot.days)
		print(time.units)
		print(bins.units)
		print(periods)
		print(max(which(periods)))
		print(bins)
	}

	####### subplots ############

	# subplot #1: histogram per date
        hist(pckg.stats.total$date,pckg.stats.total$count, freq=T, breaks=bins, ann=FALSE,
		col='gray', main="Downloads histogram")
	abline(h=mean(pckg.stats.total$count), lty=2)

	# subplot #2: histogram per nbr of downloads
	#if (combinePlts) par(mfg=c(2,1))
	hist(pckg.stats.total$count, freq=T, ann=FALSE, col='lightgray', main="")
	abline(h=mean(pckg.stats.total$count), lty=2)

	# pulse plot
	plot(pckg.stats.total$date,(pckg.stats.total$count), type="S", lwd=0.5, col="gray10", ann=FALSE)
	#abline(h=mean(pckg.stats.total$count), lty=2)
	#abline(h=mean(pckg.stats.total$count)+sd(pckg.stats.total$count), lty=3)
        #abline(h=mean(pckg.stats.total$count)-sd(pckg.stats.total$count), lty=3)
	if (!noMovAvg)
		confBand(pckg.stats.total$date,pckg.stats.total$count,
			fst.date,lst.date, 0,max.downloads*1.05,
			,,
			'royalblue',3,0.5, filling=FALSE)

	confBand(pckg.stats.total$date,pckg.stats.total$count,
		fst.date,lst.date, 0,max.downloads*1.05,
		,length(pckg.stats.total$count),
		'gray30',2,0.25, filling=!noConfBands)

	#############################

	# reset canvas to 1 plt per page
	par(mfrow=c(1,1))
	if (combinePlts) {
		par(new=TRUE)
		par(mar=c(2.5,1.5,7.5,2.5))
	}

	### plotting downloads per day
	### if there are too many points will subsample to make the plot cleaner...
	if (tot.days > cutOff.pts) {
		nbrPts <- as.integer(tot.days/cutOff.pts)
                subsample.date <- pckg.stats.total$date[1:tot.days%%nbrPts == 0]
                subsample.counts <- pckg.stats.total$count[1:tot.days%%nbrPts == 0]
		message("Nbr of data points (",tot.days,") exceeds internal cut-off (",cutOff.pts,"); plot will resub-sample to ",nbrPts,".") 
		plot(subsample.date,subsample.counts, 'b', cex=.5,
                        xlim=c(fst.date,lst.date),
                        ylim=c(0,max.downloads*1.05) ,
                        ann=FALSE, axes=FALSE )
		lines(pckg.stats.total$date, (pckg.stats.total$count),
			lwd=0.35, col='gray',
			#lty=3,lwd=0.35,
                        xlim=c(fst.date,lst.date),
                        ylim=c(0,max.downloads*1.05) ,
                        ann=FALSE)#, axes=FALSE )
	} else {
		plot(pckg.stats.total$date, (pckg.stats.total$count), 'b',
			xlim=c(fst.date,lst.date),
			ylim=c(0,max.downloads*1.05) ,
			ann=FALSE, axes=FALSE )
	}

	if (!noMovAvg) {
		if (tot.days < mnt.days) {
			nbr.Ws <- min(2,tot.days)
		} else {
			nbr.Ws <- 10
		}
		confBand(pckg.stats.total$date, (pckg.stats.total$count),
			fst.date,lst.date,0,max.downloads*1.05,
			,nbr.Ws,
			'royalblue',,1, filling=!noConfBands)
	}

	# print some stats in the plot
	text(pckg.stats.total$date[as.integer(tot.days*0.15)],.95*max(pckg.stats.total$count),
		paste0('"',pckgName,'"'," Package",'\n', fst.date," / ",lst.date,'\n',
			"Total downloads: ",sum(pckg.stats.total$count),'\n',
			"Tot.Avg: ",round(mean(pckg.stats.total$count),digits=2),'\n',
			"Last month: ",sum(pckg.stats.lstmnt$count),'\n',
			"LMAvg: ",round(mean(pckg.stats.lstmnt$count),digits=2)
		)
	)

	# emphasize last month data
	#par(new=TRUE)
	#plot(pckg.stats.lstmnt$date, (pckg.stats.lstmnt$count),
	#	'b', col='darkblue', lwd=2,
	#	ann=FALSE, axes=FALSE,
	#	xlim=c(fst.date,lst.date),
	#	ylim=c(0,max.downloads*1.05) )


	# most representative time range...
	largest.timeUnit <- max(which(periods))
	deltaT <- max(1,time.units[largest.timeUnit])
	if (dbg) print(deltaT)
#	if (tot.days <= cutOff.pts) {
		emphasize(pckg.stats.total$date,pckg.stats.total$count, deltaT, fst.date,lst.date,0,max.downloads*1.05, "darkblue", .85)
#	} else {
#		emphasize(subsample.date,subsample.counts, deltaT, fst.date,lst.date,0,max.downloads*1.05, "darkblue", .85)
#	}
	if ( (largest.timeUnit > 1) & (time.units[largest.timeUnit-1] != 30) )
		emphasize(pckg.stats.total$date,pckg.stats.total$count, time.units[largest.timeUnit-1], fst.date,lst.date,0,max.downloads*1.05, "darkred", 1.5)

	# last month trend...
	if (deltaT != 30) emphasize(pckg.stats.total$date,pckg.stats.total$count, 30, fst.date,lst.date,0,max.downloads*1.05, "darkred", 1.5)
	#print(pckg.stats.lstmnt$date)

	# stats from last month
	mean.lstmnt <- mean(pckg.stats.lstmnt$count)
	sd.lstmnt <- sd(pckg.stats.lstmnt$count)
	message(paste("Average downloads last month: ",round(mean.lstmnt,digits=2),"; sd=",round(sd.lstmnt,digits=2)))
	# avergage last month
	##lines(pckg.stats.lstmnt$date,rep(mean.lstmnt,mnt.days), type='l', lwd=2, col='darkblue',
	##	ann=FALSE,
	##	xlim=c(fst.date,lst.date),
	##	ylim=c(0,max.downloads*1.05) )
	#text(pckg.stats.lstmnt$date[2],1.075*mean.lstmnt, paste(as.integer(mean.lstmnt)), col='darkblue' )
	##text(lst.date,1.075*mean.lstmnt, paste(as.integer(mean.lstmnt)), col='darkblue' )

#        if (combinePlts) {
                #axis(side=1,
		#	at=pckg.stats.total$date[seq_along(pckg.stats.total$date)%%6==0],
		#	labels=as.character.Date(pckg.stats.total$date[seq_along(pckg.stats.total$date)%%6==0], "%d-%m-%y")
		#)

#.#		time.int <- time.intervals(tot.days)
#.#		T.unit <- time.int[[1]]
#.#		everyT <- time.int[[2]]
#.#
#.#		selectDates <- as.Date(pckg.stats.total$date[seq_along(pckg.stats.total$date) %% everyT == 0])
#.#		selectDates.labels <- paste0( substr(month.name[as.integer(substr(selectDates,6,7))],1,3) )
#.#					#	,"-", substr(selectDates,1,4) )
#.#
#.#		axis.Date(1,at = seq(min(pckg.stats.total$date), max(pckg.stats.total$date), by=T.unit))
#.#
#.#		if (tot.days-1 > 365) {
#.#			#axis(1,at=NULL, labels=F)
#.#			#text(x = selectDates, par("usr")[3]*.97, labels = substr(month.name[as.integer(substr(selectDates,nchr0,nchr1))],1,3), pos = 1, xpd = TRUE,cex=.85)
#.#			text(x = selectDates, par("usr")[3]*.995, labels = selectDates.labels, srt = 45, pos = 1, xpd = TRUE,cex=.65)
#.#		}
#.#                axis(side=4)
	axes.TimePlt(tot.days,pckg.stats.total)
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
	if (toupper(device) %in% fileFmts) dev.off()

	invisible()
}



### interactive plots
interactivePlots <- function(downloads.data, mytitle=paste(downloads.data$package[1],"Package downloads counts"),
				nbrPlts = 2, month.ln=30,
				HTMLfile=paste0("Interactive_DWNLDS_",downloads.data$package[1],".html"),
				device="HTML", dirSave=NULL ) {
#' function that generates interactive plots of the package downloads logs from CRAN
#' @param  downloads.data  total downloads from the package
#' @param  mytitle  optional char argument specifying the title to be displayed
#' @param  nbrPlts  optional numeric argument specifying number of plots to generate
#' @param  month.ln optional numeric argument specifying the lenght of the month in days
#' @param  HTMLfile  an optional string argument specifying the name of the file where to save the plots
#' @param  device  an optional string describing whether the interactive plot will be set to screen or to save in an HTLM file
#' @param  dirSave  specify a valid directory where to save the plot
#'
#' @importFrom  plotly  %>% add_annotations add_trace as_widget plot_ly subplot layout
#' @importFrom  htmlwidgets  saveWidget
#' @export
#'
#' @examples
#' \donttest{
#' packageXdownloads <- retrievePckgData("ggplot")[[1]]
#' interactivePlots(packageXdownloads)
#' }
#'
       
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
			color = ~count, 
			text = paste("downloads: ",~count), 
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
			add_trace(y = ~count,
				#color = ~count,
				size=~count/max(sqrt(count)),
				mode="markers+line", fill="tozeroy",
#				text = paste("downloads: ",~count)
#				 ) %>%
					marker=list(color=~count,colorscale="Blues",reversescale=TRUE) ) %>%
			add_trace(y = ~count,
					color=~count,
					mode = 'lines', fill = 'tozeroy',
					marker=list(color=~count))     %>%
			layout(title = mytitle,
				yaxis = list(zeroline = TRUE),
				xaxis = list(range = c(downloads.data$date[1],downloads.data$date[length(downloads.data$date)])) )
#				colorscale="Blues", showscale = FALSE, showlegend = TRUE)
			print(p2)

	p <- subplot(p1,p2)
	} else {
		p <- p1
		}


	if (tolower(device) != 'screen') {
		# check dir existance
		FileName <- check.FileDir(dirSave,HTMLfile)

		# informing where the plot is going to be saved
	        message("Saving interactive plots in ", FileName)
		htmlwidgets::saveWidget(as_widget(p), FileName)
	} else {
		message("Option device='screen' detected -- Interactive Plots will be displayed in browser only during an interactive session.")
	}

	return(p)
}


###### Comparison Plot
comparison.Plt <- function(pckgDwnlds.lst, t0,t1, cmb=TRUE,noMAvgs=FALSE,noCBs=FALSE, device="PDF",dirSave=NULL){
#' function that generates a comparison plot among several packages
#' @param  pckgDwnlds.lst  nested list containing the several packages to process
#' @param  t0  initial date
#' @param  t1  final date
#' @param  cmb  boolean flag, indicating whether the plots are shown in the same (default) graph or not
#' @param  noMAvgs  boolean flag, indicating whether moving averages are displayed (default) or NOT --set to TRUE--
#' @param  noCBs  boolean flag, indicating whether shaded confidence intervals are displayed (default) or NOT  --set to TRUE--
#' @param  device  string to select the output format: 'PDF'/'PNG'/'JPEG' or 'screen'
#' @param  dirSave  specify a valid directory where to save the file
#'
#' @importFrom graphics  legend
#'
#' @keywords internal

	### preserve user graphical env.
	# save the state of par() before running the code
	oldpar <- par(no.readonly = TRUE)
	# restore the previous state after the fn is done, even if it fails, so the user environment is not altered
	on.exit(par(oldpar))
	###

	# supported file formats
	fileFmts <- c("PDF","PNG","JPEG")

	#print(str(pckgDwnlds.lst))
	#lapply(pckgDwnlds.lst,summary)

	# define some containers for aggregating information about the packages...
	nbrPckgs <- length(pckgDwnlds.lst)
	dates.min <- c()
	dates.max <- c()
	counts.min <- c()
	counts.max <- c()
	pckgNames <- c()
	tot.days <- 0


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

		# tot.days <- max(tot.days,length(pckgDwnlds.lst[[j]][["date"]]))
		nbr.days <- length(pckgDwnlds.lst[[j]][["date"]])
		if (tot.days < nbr.days) {
			tot.days <- nbr.days
			j.peak <- j
		}
	}


	min.date <- as.Date(as.character(t0))
	max.date <- as.Date(as.character(t1))
	if (min.date != pckgDwnlds.lst[[j.peak]][["date"]][[1]]) {
		warning("Date range adjustment: initial date reset from ", min.date," to ", pckgDwnlds.lst[[j.peak]][["date"]][[1]], call.=FALSE)
		min.date <- pckgDwnlds.lst[[j.peak]][["date"]][[1]]
	}
	if (min.date != pckgDwnlds.lst[[j.peak]][["date"]][[tot.days]]) {
		 warning("Date range adjustment: initial date reset from ", min.date," to ", pckgDwnlds.lst[[j.peak]][["date"]][[tot.days]], call.=FALSE)
		max.date <- pckgDwnlds.lst[[j.peak]][["date"]][[tot.days]]
	}
	xrange <- c(min.date,max.date)

	#min.date <- min(dates.min)
	#max.date <- max(dates.max)
	counts.min <- (min(counts.min))
	counts.max <- (max(counts.max)*1.05)
	yrange <- c(counts.min,counts.max)
	#print(xrange)
	#print(yrange)

	fileName <- paste0("DWNLDS_",paste(pckgNames,collapse='-'),".",tolower(device))
	#message("Combined plots for packages: ",paste(pckgNames,collapse=' ')," will be saved in ",fileName)
	outputType(device, dirSave,fileName, fileFmts, cmb, msg=paste0("Combined plots for packages: ",paste(pckgNames,collapse=' ')) )

	# set margins values
	par(mar=c(3,2.5,0.85,0.5))
	#plot(pckgDwnlds.lst[[1]]$date, pckgDwnlds.lst[[1]]$count, 'n')
	if (cmb)  plot(xrange,yrange, 'n', xlim=xrange, ylim=yrange, ann=FALSE, axes=FALSE)

	for (j in 1:length(pckgDwnlds.lst)) {
		if (cmb) par(new=TRUE)
		yvar <- (pckgDwnlds.lst[[j]]$count)
		if (!cmb) yrange <- c(min(yvar),max(yvar))
		plot(pckgDwnlds.lst[[j]]$date, yvar, 'o', col=j, lwd=0.35, cex=0.35,
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

		legend("topright",paste("Date range ",min.date,"/",max.date),
			cex=0.65, box.lty=0)
		legend("top", pckgNames,
			cex=0.5,box.lty=0, pch=1,lty=1,pt.cex=0.5,
			col=1:length(pckgDwnlds.lst),border='white')
	}

	# draw axes...
        #T.unit <- time.intervals(tot.days)[[1]]
	#print(T.unit)
	#axis.Date(1,at = seq(min.date, max.date, by=T.unit))
	#axis(2)
	axes.TimePlt(tot.days,pckgDwnlds.lst[[j.peak]],2)
	#axis(side=2, at=seq(counts.min,counts.max, by=abs(counts.max-counts.min)/10),labels=FALSE)
	#box()

	# close file...
	if (toupper(device) %in% fileFmts) dev.off()
} 

#########################################################################################

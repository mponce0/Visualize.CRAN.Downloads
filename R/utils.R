movingFn <- function(x, fn=mean, period=length(x), type="forward") {
#' generic fn that computes the "fn" on a moving window
#' @param  x  a numeric vector
#' @param  fn  a function to be applied/computed, default is set to mean()
#' @param  period  size of the "moving window", default set to the lenght of the vector
#' @param  direction  type of moving avergage to consider: "forward", "centered", "backwards"
#'
#' @return  a vector with the 'moving operation' applied to the x vector

	if (!is.numeric(x)) stop("argument x must be of type numeric!")
	if (!is.function(fn)) stop("fn must be a function!")
	if (!is.numeric(period)) stop("Argument period must be of type numeric!")

	mavg <- c()
	n <- length(x)

	#print(period) 

	if ( (period==0) || (period>=length(x)) ) {
		mavg <- rep(fn(x),length(x))
	} else {
		#print("using",as.character(fn))
		for (i in 1:length(x)) {
			irange <- i:min(i+(period-1),n)
			#cat(irange)
			#print(x[irange])
			#cat("...", fn(x[irange],na.rm=TRUE),'\n')
			mavg <- c(mavg,fn(x[irange],na.rm=TRUE))
		}
	}

	return(mavg)
}

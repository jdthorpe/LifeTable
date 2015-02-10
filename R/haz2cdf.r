# utility functions for converting between CDF's and discrete 
# hazard function's

#' Convert a descrete hazard function into a descrete CDF
#' 
#' Convert a descrete hazard function into a descrete CDF.
#' 
#' @export
#' @param x A numeric vector representing a descrete hazard function
haz2cdf <- function(x)
	c(0,1-cumprod(1-x))

#' Convert a descrete CDF into a descrete hazard function
#' 
#' Convert a descrete CDF into a descrete hazard function.
#' 
#' @export
#' @param x A numeric vector representing a descrete Cumulative Distribution Function (CDF)
cdf2haz <- function(x){
	haz <- numeric(0)
	for(i in 1:(length(x)-1)){
		# survival function at t
		S_t <- 1-x[i]
		# drop at time t
		D_t <- x[i+1] - x[i]
		haz[i] <- D_t / S_t
	}
	names(haz) <- paste(names(x)[-length(x)],
						names(x)[-1],
						sep = 'to')
	return(haz)
}


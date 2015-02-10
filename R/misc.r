# ------------------------------------------------------------
# Miscelaneous utility functions functions 
# ------------------------------------------------------------


# ------------------------------------------------------------
# probability to log-odds
# ------------------------------------------------------------
#' Transfrom values from the probability to the log-odds scale
#' 
#' Transfrom values from the probability to the log-odds scale.
#' 
#' @export
#' @param x a vector of probabilities
#' @return  vector of log odds corresponding to \code{x}, the vector of probabilities.
#' @examples
#' logOddsToProbs(c(.25,.5.75))
#' 

logodds <- function(x) {
	if(any(x < 0 | x > 1))
		stop('x must be beween 0 and 1')
	log(x/(1-x))
}

#' Transfrom values from the log-odds to the probability scale
#' 
#' Transfrom values from the log-odds to the probability scale.
#' 
#' @export
#' @param x a vector of log odds
#' @return  vector of probabilities corresponding to \code{x}, the vector of log-odds.
#' @examples
#' logOddsToProbs(c(-1,0,1))
#' 

# ------------------------------------------------------------
# log-odds to probability
# ------------------------------------------------------------
logOddsToProbs <- function(x) 
	ifelse(x == Inf,
		   1,
		   exp(x) / (1 + exp(x)) )

#-- # JUST CHECKING...
#-- for(i in -5:5)
#-- 	stopifnot( abs(logodds(logOddsToProbs(i)) - i) < 1e-14)
#-- 
#-- for(i in (1:99)/100)
#-- 	stopifnot( abs(logOddsToProbs(logodds(i)) - i) < 2e-16)

#-- # JUST CHECKING...
#-- for(i in -5:5)
#-- 	stopifnot( abs(logodds(logOddsToProbs(i)) - i) < 1e-14)
#-- 
#-- for(i in (1:99)/100)
#-- 	stopifnot( abs(logOddsToProbs(logodds(i)) - i) < 2e-16)

# ------------------------------------------------------------
# various internal functions
# ------------------------------------------------------------
inverseExponent <- function(x) {
	if(any(x < 0 | x > 1))
		stop('x must be beween 0 and 1')
	-1/log(x)
}

invExpToProbs <- function(x) 
	ifelse(x <=0,
		   0,
		   exp(-1/(x)))


uniqueStrings <- function(x){
	x <- as.character(unclass(x))
	return(unique(x[!is.na(x)]))
}

`%^%` <- function(A,n){
	# Matrix Exponent
	stopifnot(as.integer(n) == n)
	stopifnot(n>=1)
	if(n == 1)
		return(A)
	else return(A %^% (n - 1) %*% A)
}


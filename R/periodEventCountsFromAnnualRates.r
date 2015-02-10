#' Calculate the number of events (incidence)
#' 
#' Calculate the number of events (incidence) expected in an interval
#' given the intial state and the (absolute, continuous) hazard matrix
#' 
#' @export
#' @param x0 A vector with the initial state of the cohort at the 
#' start of the interval
#' 
#' @param M A Matrix with the absolute, continuous transition paramters
#' 
#' @return A vector with the incidence of each event during the interval. 
#' \strong{Note that} in general this is \strong{not} the same as the 
#' differnce between the state of the cohort at the start and end of the 
#' interval.

EventCountsFromContinuousHazards <- function(x0, #the initial state 
										   M #the continuous hazaard matrix
										   ){
	# INITIALIZE THE OUTPUT CONTAINER
	eventCounts <- rep(0,ncol(M))

	# LOOP OVER THE SET OF (NON-SOURCE) NODES
	(is.sourceNode <- apply(M,1,function(x)all(x == 0)))
	for(i in which(!is.sourceNode)){
		M_n <- M 
		M_n[,i] <- 0 
		# solve the HLSCC model
		x1  <-  continuousHazardsToLifeTableStep(x0,M_n)
		# the difference between the befor and after states
		# for state N is it's incidence
		eventCounts[i] <- (x1-x0)[i]
	}
	return(eventCounts)
}


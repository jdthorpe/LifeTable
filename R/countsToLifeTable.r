
#' Compute of a life table from count and relative risk data
#' 
#' Compute (portions) of a life table from count and relative risk data
#' where counts are expressed as proportions of the \strong{entire} birth
#' cohort.
#' 
#' @export
#' @family lifetable
#' @param incidence A data.frame with one row per age interval, ordered by age.
#' 
#' @param x0 A vector with the distribution of the population
#' across the state space at birth of the cohort.  In the OvaSim model,
#' states represent the accumulation of events, so typically at birth, 
#' none of the members of the cohort have experienced any events, and the
#' \code{x0} will be something like: 
#' 
#' 	\code{c(healty = 1, cancer =0, benign = 0, tubalLidation = 0, dead=0)}
#' 
#' though it doesn't have to be. For example if different RR models
#' are at different sets of age intervals, then the initial state may 
#' be the final from a previous call to \code{countsToLifeTable}.
#' 
#' @param RR A numeric matrix where each row represents the various 
#' relative risks of arriging at a given node from each 
#' of the other nodes.  Note that loops in the transition
#' states are not allowed, so most of the elements of \code{RR} will be
#' zero (including \strong{all} of the diagonal elt's). All RR's 
#' must be positive.
#' 
#' @note
#' In \code{countsToLifeTable()}, 'counts' are really proportions of the
#' poulation who will experience an event during the age-interval. 
#' (i.e. the fraction of the \strong{entire} 
#' cohort, many of which could be dead before the start of the 
#' current interval.   
#' 
#' This is as opposed to most readily available incidence data which are 
#' expressed in terms of "events per women alive at the start of the age
#' interval", which really describes a rate and not a proportion of the
#' birth cohort. In the latter case you should use
#' \code{link{ratesToEventCounts}}
#' 

countsToLifeTable <- function(incidence,x0,RR){

	# VALIDATE THE INPUTS
	stopifnot(length(x0) == nrow(RR))
	stopifnot(length(x0) == ncol(RR))
	stopifnot(length(x0) == ncol(incidence))
	stopifnot(all(names(x0) == names(incidence)))
	stopifnot(all(rownames(RR) == names(incidence)))
	stopifnot(all(colnames(RR) == names(incidence)))
	
	out <- matrix(-1,nrow(incidence)+1,ncol(incidence))
	colnames(out) <- names(x0)
	out <- rbind(x0)

	# --------------------------------------------------
	# handling Errors in R (gracefully) is ugly...
	# --------------------------------------------------
	# BEGINING OF ERROR HANDLING + WORK
	failed <- list(FALSE)
	withRestarts(
		withCallingHandlers({
			# ------------------------------
			# THE ACTUAL WORK 
			# ------------------------------
			for(i in 1:nrow(incidence)){

				# the next state is the initial state for the next iteration
				rateMX <- continuousHazardsFromIncidenceCounts(x0=out[i,],
										 observedEvents=as.numeric(incidence[i,]),
										 RR)
				nextState <- continuousHazardsToLifeTableStep(x0, rateMX)
				out <- cbind(out,nextState)
			}
			# ------------------------------
			# END OF THE ACTUAL WORK 
			# ------------------------------
		},error = function(a){
			failed[[1]] <<- TRUE
			invokeRestart('abort')
		} )
	,abort=function(a){
		#pass
	})
	if(failed[[1]])
		warning(paste('continuousHazardsFromIncidenceCounts() failed at iteration ',i,
					  '.\nReturning results from life table steps up to this point ',
					  sep = ''))
	# --------------------------------------------------
	# END OF ERROR HANDLING + WORK
	# --------------------------------------------------
	return(out)
}


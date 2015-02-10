#' Compute a continuous hazards matrix
#' 
#' Calculate rates on the basis of the entire cohort from rates
#' expressed as proportions of the *alive* cohort.
#' 
#' @export
#' @family lifetable
#' 
#' @param x0 a vector with the distribution of the \strong{observed}
#' population over (a portrion of) the state space at the start of the life
#' table step.
#' 
#' @param incidence The (oberved) incidence of each event (relative to
#' \code{sum(x0)}).
#' 
#' @param RR A numeric matrix where each row represents the various 
#' relative risks of arriging at a given node from each 
#' of the other nodes.  Note that loops in the transition
#' states are not allowed, so most of the elements of \code{RR} will be
#' zero (including \strong{all} of the diagonal elt's). All RR's 
#' must be positive.
#' 
#' @param transformation the transformation to use in optimizing the rates.
#' options include 'logOdds', 'logNegativeSquaredInverse',and 'identity',
#' matched by \code{match.arg}.  (default = 'logOdds').
#' 
#' @note
#' When incrementing a life table, it is necessary to know both where the
#' individuals go, but also where they came from in order to keep track of the
#' whole population.  \code{continuousHazardsFromIncidenceCounts} solves that problem 
#' by solving for a transiziton (rate) matrix is a helper function for
#' 
#' \code{continuousHazardsTolifeTableStep} is great if you have rates, but what if what you have is 
#' are statistics on the number of events expected in the next interval?
#' 
#' what if you knew the number of BSO's, benign DX's, XXOC's, and cancers,
#' you epect in the population, and you know the relative risks of these 
#' events within the population, but you don't know the rates.


continuousHazardsFromIncidenceCounts <- function(x0,
						 incidence,
						 RR,
						 handleError=FALSE
						 ){

	# Strategy: from the RR matrix, we will make a first 
	# guess at the transition rates using a simple model
	# that solves the descrete problem where only one 
	# edge in the state graph may be taversed in a single
	# time interval.

	# From there, we'll use the Newton-Raphson method to 
	# estimate the rates, by (1) estimating the steady 
	# state solution using continuousHazardsToLifeTableStep(),
	# and then calculating the incidence of each outcome
	# based on a subgraph, where the directed edges leading
	# away from that node are removed.  

	# ---------------------------------------------------
	# Step 0: validate parameters
	# ---------------------------------------------------
	if(any(is.na(x0 ))) stop('Missing values in paremeter "x0" not allowed')
	if(any(is.na(incidence))) stop('Missing values in paremeter "incidence" not allowed')
	if(any(incidence < 0)) stop('incidence event counts must be positive')
	if(any(incidence > sum(x0))) stop('Incidence cannot exceed population size')
	if(any(is.na(RR ))) stop('Missing values in paremeter "RR" not allowed')
	if(any(RR <0 )) stop('Relative risks in the matrix RR must be non-negative ')

	stopifnot(nrow(RR) == ncol(RR))
	stopifnot(nrow(RR) == length(incidence))
	stopifnot(nrow(RR) == length(x0))

	(sourceNodes <- which(apply(RR,1,function(x)all(x == 0)) | (incidence == 0)))

#-- 	transformation <- match.arg(transformation,c('logOdds',
#-- 												 'logNegativeSquaredInverse',
#-- 												 'identity'))

	if(length(sourceNodes) == 0)
		stop('No loops alowed in the matrix "RR":  there should be at least 1 
row with no non-zero elts. (E.g. there must be >=1 source nodes.)'  )

		# implements the solution to 
	if(any(diag(RR)!=0))
		stop('No loops alowed in the matrix "RR" (non-zero Diagnoal Elts, in this case.)'  )


	if(any(incidence[sourceNodes] > 0))
		stop('Inconsistent "incidence" and "RR" parameters: non-zero incidence expected 
			 where the rates are all zero')

	validateMatrix((RR>0)+0)

	# ---------------------------------------------------
	# step 1: initial guess
	# ---------------------------------------------------

	start <- rep(0,length(incidence))
	for(i in 1:length(incidence)){
		if(i %in% sourceNodes)
			next
		# the row in the matrix RR corresponding to a node
		# represents the relative risks of arriving at that
		# node from the remaining nodes
		relRisks <- as.numeric(RR[i,])
		start[i] <- min(0.99, incidence[i] / sum(relRisks*x0))
	}
	(start <- start[-sourceNodes])

	start <- pmax(log(start),log(1E-16))
	# -------------------------------------------------------------
	# step 2: use newton-raphson method to estimate hazard paremters
	# -------------------------------------------------------------

	# ---------------------------------------------------
	# a utility function that expands the paremters 
	# from the newton-raphson (NR) method, counts the
	# events at each node based on those paremters
	# and returns an appropriate vector for the 
	# next iteration of the NR method
	# ---------------------------------------------------
	inner <- function(param){
		# process the parameter vector
		(M <- RR*expand(exp(param),sourceNodes))
#-- 		if(any(M > 1))
#-- 			stop(rateMessage)
		eventCounts <- EventCountsFromContinuousHazards(x0,M)
		# we're finding the root of the difference between the expected
		# and actual event counts, so return that!
		return( (eventCounts - incidence)[-sourceNodes])
	}

	# ---------------------------------------------------
	# the actual work...
	# ---------------------------------------------------
	# there is a bit of trick error and warning handling 
	# because the error messages from multiroot are farily
	# confusing...
	factorizationError <- list(FALSE)
	withRestarts(
		withCallingHandlers({
			tmp <- rootSolve:::multiroot(inner, 
							 start = start,#ifelse(start < 1e-9,1e-9,start),
							 rtol = 1E-8,
							 ctol = 1E-8,
							 atol = 1E-8,
							 maxiter = 1000)
		}
		,warning = function(e){
#-- 			print(',warning = function(e){')
#-- 			browser()
			if(e$message == "error during factorisation of matrix (dgefa);         singular matrix"){
				factorizationError[[1]] <<- TRUE
				invokeRestart('abort')
			}
		})
	,abort = function(a){
		# pass
	})

	if(factorizationError[[1]]){
		if(handleError){
			warning(noSolutionMessage)
			return(-1)
		}else
			stop(noSolutionMessage)
	}

	return(RR*expand(exp(tmp$root),sourceNodes))
}

noSolutionMessage <- 
	'A solution for (finite) hazards could not be reached. The 
specified incidence may exceed size of the at-risk population 
for one or more events or the terminal step has been reached.'


#-- rateMessage <- 
#-- "continuousHazardsFromIncidenceCounts stoped with an probability estimate > 1 (100%).
#-- This may be due to a probablity == 1, (which often occures at the final step
#-- of a life-table) in which case, you should identify that probabiltity 
#-- manually and factor it out out before re-applying.
#-- 
#-- Otherwise, you may use 'continuousHazardsFromIncidenceCounts( ... ,useLogOdds=TRUE)'
#-- which will avoiding this error but may result in an under estimate of the
#-- offending probablity and in an over estimate of competing risks, which will
#-- result in (at least slight) inaccuracies.
#-- 
#-- In addition, the Newton Raphson method converges much more slowely for 
#-- small probablities, and so it may be less accurate, especially for events
#-- with a small probability within a given interval (resulting in over-estimates
#-- of the expected rate of the event...)"

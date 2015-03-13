#' Compute life table
#' 
#' Compute or extend a life table from Hazard Ratios and incidence, or
#' absolute (net) hazards.
#' 
#' @export
#' @family lifetable
	#' 
#' @param initialState A vector with the distribution of the population
#' across the state space at the start of the life table, or a life table 
#' that is to be extended.
#' 
#' @param finalState a N vector or (S)xN matrix with the final states at the 
#' (each of several) time interval(s) in which a population is observed
#' 
#' @param hazard If the parameter \code{incidence} is sppecified, \code{hazard} is
#' treated as a matrix with with the \strong{relative} hazards of transitioning
#' between states, otherwise \code{hazard} is treated as the  abosolute (net)
#' hazards of transitioning between states.
#'
#' @param netHazards a N vector or (S)xN matrix containing the net hazards
#' for one or more time intervals. Note that only of of \code{hazard} and 
#' \code{netHazards} should be specified.
#' 
#' @param hazardModel see \code{hazard} (to be completed)
#' 
#' @param hType the type of the hazards included in the arguemnt \code{hazard}.
#' One of 'continuous' or 'discrete' (matched by match.arg())
#' 
#' @param incidence If provided, the this specifies the incidences [number
#' of arrivals at each node] during the interval.  The incidence may be 
#' expressed as a proportion of the cohort that is expected to undergoe the event
#' during the interval (default) or as a fraction of those at risk for the event.
#' To use the latter, the paramter 'at.risk' must be specified.  
#' 
#' Note that if \code{incidence} is a matrix or data.frame, rows in \code{incidence} 
#' are considered incidence at consecutive intervals in the life table. 
#' 
#' @param at.risk Porportion of the birth cohort \code{sum(initialState)} that 
#' was used as the denominator for each each element of the parameter 
#' \code{incidence}. This can be specfied by a nueric value (default = 
#' \code{sum(initialState)}), a vector with the proportion of the cohort used for 
#' each individual parameter, or a function with takes a single parameter 
#' with the current distribution of the cohort (begining with statstate), and returns 
#' a vector withthe proporitn of the cohort that represents the denominator 
#' for each incidence parameter in the current iteration of the life table.  
#' 
#' @param steps The number of records to add to the life table, for each row of 
#' 'incidence' or each value of hasMX. If \code{incidence > 1}, then 
#' \code{incidence > 1} intermediate records are added to the returned life table 
#' 
#' @param checkNames If \code{TRUE}, names / dimnames of the various inputs are checked 
#' for consistency. (default=\code{FALSE})
#' 
#' @param quietly If \code{TRUE}, messages stating the methods being used to 
#' calculate the life table are suppressed.
#' 
#' @note
#' This implements a solution to a Homogeneous Linear Systemwith Constant 
#' Coefficients described in section 7.5 of "Elementary Differential
#' Equations and Boundary Value Problems" by Boyce and Diprima, 6th 
#' Edition in order to solve the realted continuous time markov model.

lifeTable <- function(initialState,# a N vector or (S+1)xN matrix where S=(number or steps) and N=(number of nodes) 
					  hazardModel=hazard,# a NxN Matrix or list of such matricies with length S 
					  hazard,# a NxN Matrix or list of such matricies with length S 
					  netHazards,# a N vector or (S)xN matrix 
					  hType='continuous',# 'continuous' or 'discrete' (matched by match.arg())
					  incidence,# a N vector or (S)xN matrix 
					  at.risk,# a N vector or (S)xN matrix 
					  finalState,# a N vector or (S)xN matrix 
					  steps=1,
					  checkNames=FALSE,
					  quietly=FALSE){

	if(missing(hazardModel)&&missing(hazard))
		stop('argument "hazardModel" is missing, with no default')
	# --------------------------------------------------
	# check the 'initialState' parameter
	# --------------------------------------------------
	if(inherits(initialState,'lifetable')){
		stop('not completed')
		initialLT <- initialState
		initialState <- initialState$states[nrow(initialState$states),]
	}else{
		initialLT <- NULL
	}
	stopifnot(is.numeric(initialState))
	stopifnot(!any(is.na(initialState)))
	stopifnot(!any(is.nan(initialState)))

	# --------------------------------------------------
	# calculate hazards from hazard ratios + netHazards
	# --------------------------------------------------
	if(!missing(netHazards)){
		if(!missing(hazard))
			stop("parameters `hazard` and `netHazards` must not both be specified")
		match.arg(hType,c('continuous','discrete'))
		if(hType == 'discrete')
			netHazards <- -log(1-netHazards)
		if(is.list(hazardModel)){
			if(length(hazardModel)>1 ||
			   !is.numeric(hazardModel[[1]])|| 
			   !is.matrix(hazardModel[[1]]))
				stop("parameter `hazardModel` must contain a single numeric matrix when `netHazards` is specified")
			hazardModel <- hazardModel[[1]]
		}
		stopifnot(is.numeric(hazardModel))
		stopifnot(is.matrix(hazardModel))

		netHazards <- rbind(netHazards)
		stopifnot(ncol(netHazards) == nrow(hazardModel))
		stopifnot(ncol(netHazards) == ncol(hazardModel))

		hazard <- list()
		for(i in seq(nrow(netHazards))){
			hazard[[i]] <- hazardModel*netHazards[i,]
			for(j in which(!is.finite(netHazards[i,]))){
				tmp  <-  hazard[[i]][j,]
				tmp[is.nan(tmp)] <- 0
				hazard[[i]][j,] <- tmp  
			}
		}
		names(hazard)  <- rownames(netHazards) 
	}

	# --------------------------------------------------
	# check the shape of the 'hazardModel'
	# --------------------------------------------------
	if(!inherits(hazardModel,'list'))
		hazardModel <- list(hazardModel)

	stopifnot(all(sapply(hazardModel,inherits,'matrix')))
	if(checkNames){
		stateNames <- names(initialState)
		stopifnot(is.null(stateNames))
		stopifnot(all(sapply(hazardModel,
							 function(x)all(colnames(x) == stateNames))))
		stopifnot(all(sapply(hazardModel,
							 function(x)all(rownames(x) == stateNames))))
	}else{
		stopifnot(all(sapply(hazardModel, nrow)==length(initialState)))
		stopifnot(all(sapply(hazardModel, ncol)==length(initialState)))
	}
	stopifnot(is.numeric(unlist(hazardModel)))
	stopifnot(!any(is.na(unlist(hazardModel))))
	stopifnot(!any(is.nan(unlist(hazardModel))))

	# --------------------------------------------------
	# INITIALIZE THE LIFETABLE OBJECT 
	# --------------------------------------------------
	LT=list(initialState=initialState,# a (S+1)xN matrix where S=(number or steps) and N=(number of nodes) 
			states=NULL,
			hazards=NULL, # a list with length S contianing  NxN (continuous) transition matricies
			incidence=NULL,# a SxN matrix 
			hazardModel=hazardModel,# a NxN matrix of with the relative hazards 
			netHazards=NULL)# a NxN matrix of mode logical or characer describing the risk model
			

	# --------------------------------------------------
	# code for the individual scenarios as follows
	# --------------------------------------------------
	# (1) Initial State + Incidence + HR's    : !missing(incidence)
	# (2) Initial State + Absolute Hazards    : missing(incidence) & missing(finalState)
	# (3) Initial State + Final State + HR's  : missing(incidence) & !missing(finalState)

	if(!missing(incidence)){

		if(!missing(hazard))
			stop("Parameters `hazard` and `incidence` should not both be sepcified")
		# (1) Initial State + Incidence + HR's
		if(!quietly)
			cat('Calculating states from incidence and relative risk matrix (`hazards`)\n')

		# --------------------
		# parameter validation 
		# --------------------

		# IF INCIDENCE IS A VECTOR, MAKE IT A MATRIX WITH 1 ROW...
		incidence <- rbind(incidence)
		.ROWNAMES  <-  rownames(incidence)
		if(!missing(at.risk))
		if(!mode(at.risk)=='function'){
			at.risk <- rbind(at.risk)
			stopifnot(dim(at.risk) == dim(incidence))
			if(nrow(at.risk)>1)
				stop('At risk population depends on the initial population, hence matrix `at.risk` should not contian more than 1 row')
		}

		if(checkNames){
			stopifnot(all(colnames(incidence) == stateNames))
		}else{
			stopifnot(ncol(incidence) == length(initialState))
		}
		if(nrow(incidence)>1 && length(hazardModel)==1)
			hazardModel <- rep(hazardModel,nrow(incidence))
		if(nrow(incidence) != length(hazardModel))
			stop("parameter `hazardModel` must have length 1 or length equal to that of `incidence`." )

		# --------------------
		# Actual work
		# --------------------
		for(i in 1:nrow(incidence)){

			if(!missing(at.risk)){
				# get the portion of the population that is at risk for each outcome / event
				if(mode(at.risk)=='function'){
					denom = at.risk(initialState)
					# validation 
					if(checkNames && !all(names(denom) == stateNames))
						stop("User provided function `at.risk` must return a vector with the same names as it's argument")
					if(!is.numeric(denom))
						stop("User provided function `at.risk` must return a numeric vector")
					if(length(initialState) != length(denom))
						stop("User provided function `at.risk` must return a vector with the same length as it's argument")
				} else {
					denom <- at.risk[i,]
				}
				# ADJUST THE INCIDENCE TO REFLECT THE FACT THAT ONLY 
				# A PORTION OF THE POPULATION IS A RISK
				.INCIDENCE = incidence[i,]*(denom/sum(initialState))
			}else{
				.INCIDENCE = incidence[i,]
			}

			# CALCULATE THE ABSOLUTE HAZARDS
			M <- continuousHazardsFromIncidenceCounts(x0=initialState,
									 incidence=as.numeric(.INCIDENCE),
									 RR = hazardModel[[i]],
									 .handleError=TRUE)
			
			if(identical(M,-1)){
				if(i < nrow(incidence)){
					warning(paste('No solution found at row ',i,'returning partial solution'))
				} else 
					warning(paste('No solution found at final row.  Finite hazards cannot be at terminal state. Condsider implementing an argument "final" to implement calculations of the final state.'))
				break
			}

			# CALCULATE THE NEXT STATE
			nextState <- continuousHazardsToLifeTableStep(initialState, M)

			# CALCULATE STANDARDHAZARDS
			.NETHAZARDS <- apply(M/hazardModel[[i]],1,median,na.rm=TRUE)
			.NETHAZARDS[is.na(.NETHAZARDS)] <- 0

			newIncidence <- EventCountsFromContinuousHazards(initialState,M)

			# --------------------
			# Book Keeping
			# --------------------
			# STATES
			LT$states <- rbind(LT$states,nextState)

			# HAZARDS
			dimnames(M) <- list(names(initialState),names(initialState))
			LT$hazards <- c(LT$hazards, list(M))

			# INCIDENCE
			LT$incidence <- rbind(LT$incidence, newIncidence) 

			# the next state is the initial state for the next iteration
			initialState <- nextState

			# STANDARD HAZARDS
			LT$netHazards <- rbind(LT$netHazards, .NETHAZARDS) 

		}

	}else if(missing(finalState)){
		# (2) Initial State + Absolute Hazards    : missing(initialState) & missing(incidence)
		if(!quietly)
			cat('Calculating states using net hazards\n')

		# --------------------
		# parameter validation 
		# --------------------
		if(!missing(at.risk))
		   stop("Parameter 'at.risk' is not used when 'incidence' is not specified.")

		# --------------------------------------------------
		# check the shape of the 'hazard'
		# --------------------------------------------------
		if(!is.list(hazard))
			hazard <- list(hazard)
		.ROWNAMES  <-  names(hazard)
		if(length(hazard)>1 && length(hazardModel)==1)
			hazardModel <- rep(hazardModel,length(hazard))
		if(length(hazardModel)!= length(hazard))
			stop("parameter `hazardModel` must have length 1 or length equal to that of `hazard`." )

		stopifnot(all(sapply(hazard,inherits,'matrix')))
		if(checkNames){
			stopifnot(all(sapply(hazard,
								 function(x)all(colnames(x) == stateNames))))
			stopifnot(all(sapply(hazard,
								 function(x)all(rownames(x) == stateNames))))
		}else{
			stopifnot(all(sapply(hazard, nrow)==length(initialState)))
			stopifnot(all(sapply(hazard, ncol)==length(initialState)))
		}
		stopifnot(is.numeric(unlist(hazard)))
		stopifnot(!any(is.na(unlist(hazard))))
		stopifnot(!any(is.nan(unlist(hazard))))

		# --------------------
		# Actual work
		# --------------------
		for(i in 1:length(hazard)){
			# CALCULATE THE NEXT STATE
			nextState <- continuousHazardsToLifeTableStep(x0=initialState, 
														  M=hazard[[i]])

			# CALCULATE INCIDENCE OVER THE COMMING INTERVAL
			nextIncidence <- EventCountsFromContinuousHazards(x0=initialState,
															  M=hazard[[i]])

			# CALCULATE STANDARD HAZARDS
			.NETHAZARDS <- apply(hazard[[i]]/hazardModel[[i]],1,median,na.rm=TRUE)
			.NETHAZARDS[is.na(.NETHAZARDS)] <- 0

			# --------------------
			# Book Keeping
			# --------------------
			# STATES
			LT$states <- rbind(LT$states, nextState)

			# INCIDENCE
			LT$incidence <- rbind(LT$incidence, nextIncidence) 

			# the next state is the initial state for the next iteration
			initialState <- nextState

			# STANDARD HAZARDS
			LT$netHazards <- rbind(LT$netHazards, .NETHAZARDS) 

		}

		# --------------------
		# Book Keeping
		# --------------------
		# HAZARDS
		LT$hazards <- c(LT$hazards,hazard)

	}else{
		# (3) Initial State + Final State + HR's  : missing(initialState) & !missing(incidence)
		stop('not yet implemented')
		warning('Running time for calculating life tables from a State Table is order 2^N, where N is the number of Risk parameters')

	}
		
	colnames(LT$states) <- 
	colnames(LT$incidence) <- 
	colnames(LT$netHazards) <- names(LT$initialState) 

	rownames(LT$states)     <- .ROWNAMES [1:nrow(LT$states)]
	rownames(LT$incidence)  <- .ROWNAMES [1:nrow(LT$incidence)]
	rownames(LT$netHazards) <- .ROWNAMES [1:nrow(LT$netHazards)]

	if(!missing(steps)){
		stop('not yet implemented')
	}

	if(!is.null(initialLT)){
		stop('not yet implemented')
	}
	
	class(LT)  <-  'LifeTable'
	return(LT)
}

#' @export
print.LifeTable <- function(x,...){
	cat('A LifeTable object with the following states:\n')
	print(rbind(x$initialState,x$states))
}

#' @export
head.LifeTable <- function(x,n=5,...){
	cat('A LifeTable object with the following states:\n')
	head(rbind(x$initialState,x$states),n)
}

#' @export
tail.LifeTable <- function(x,n=5,...){
	cat('A LifeTable object with the following states:\n')
	tail(rbind(x$initialState,x$states),n)
}




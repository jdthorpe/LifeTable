
#' optimizes switch probabilities (hazards) 
#' 
#' optimizes switch probabilities for a single iteration of a life
#' table.
#' 
#' @export
#' @family lifetable
#' 
#' @param startState The distribution of individuals in each model state
#' prior to the transition interval  
#' 
#' @param endState The distribution of individuals in each model state
#' after to the transition interval  
#' 
#' @param start (optional) An initial guess at the transition probabilities
#' 
#' @param hazNameMX
#' 
#' @param propHazMx
#' 
#' @param verbose If TRUE, runtime updates are printed to the terminal
#' and get_pAB_matrix(...,verbose=TRUE) is called on the first iteration
#' of the Newton-Raphson root-finding method (default= FALSE).
#' 
#' @param sharedParamList (optional) a set of parameters shared across iterations of
#' a life table calculateion, created by calling
#' \code{prepParams(hazNameMX,propHazMx)}
#' 
#' @importFrom rootSolve multiroot

optimizeSwitchProbs <- function( 
		startState, # the probabilies of being in any one state before the transition: first use: 811
		endState,# the probabilies of being in any one state after the transition: first use: 838
		paramGuess, # firtst use, 855
	    hazardsMX,
		sharedParamList){

	initial__txParamNames <- 
	txParamNames <- sharedParamList[['txParamNames']]
	txHazList <- sharedParamList[['txHazList']]
	txParamList <- sharedParamList[['txParamList']]
	hazNameMX <- sharedParamList[['hazNameMX']]
	propHazMx <- sharedParamList[['propHazMx']]

	extended_switchNames <- c(txParamNames,
							  uniqueStrings(sharedParamList$"txHazNameMat"))

	# sometimes a transistion does not occure in a certian age bracket.
	# Hence, we need to look for transition probabilities that 
	# do not occure in the given life table step and reduce the problem 
	# size accordinlgy 

	nullSwitches <- hazNameMX[hazardsMX ==0]
	nullSwitches <- nullSwitches[!is.na(nullSwitches)] 
	if(length(nullSwitches)){
		# validate assumptions
		for(nullSwitch in nullSwitches){
			if(!all(hazardsMX[(!is.na(hazNameMX)) & hazNameMX == nullSwitch] == 0))
				stop('for now, switches that are off for only some transitions are not supported')
		}
	}

	sharedParamList_copy <- sharedParamList
	if(length(nullSwitches)){
		# more than one source node, so  we need to optimze a smaller system
		sharedParamList <- reduce(sharedParamList,nullSwitches)

		# export the reduced parameters
		(txParamNames <- sharedParamList[['txParamNames']])
		(txHazList <- sharedParamList[['txHazList']])
		(txParamList <- sharedParamList[['txParamList']])
		(hazNameMX <- sharedParamList[['hazNameMX']])
		(propHazMx <- sharedParamList[['propHazMx']])
	}

	if(missing(paramGuess)){
		stopifnot(mode(hazNameMX) == 'character')
		stopifnot(mode(hazardsMX / propHazMx ) == 'numeric')
		haz_temp <- as.numeric(hazardsMX / propHazMx )
		switchProbParamGuess <- tapply(1-exp(-haz_temp), as.character(hazNameMX), mean)

		stopifnot(all(txParamNames %in% names(switchProbParamGuess)))
		stopifnot(length(txParamNames) == length(switchProbParamGuess))
		switchProbParamGuess <- switchProbParamGuess[match(txParamNames,names(switchProbParamGuess))]

		if(any(nullSwitches %in% names(switchProbParamGuess))){
			print('(any(nullSwitches %in% names(switchProbParamGuess)))')
			while(T)browser()
		}
		if(any(switchProbParamGuess == 1)){
			print('(any(switchProbParamGuess == one))')
			while(T)browser()
		}
		if(any(switchProbParamGuess == 0)){
			print('(any(switchProbParamGuess == zero ))')
			while(T)browser()
		}
		paramGuess <- logodds(switchProbParamGuess)
	}

	# if the switch probabilities are zero or 1, then the 
	# jacobian will not be invertable and the newton raphson method
	# will fail.  A better way do handle this would probably be to 
	# reduce the problem size due to the fact that a guess of one or 
	# zero indicates that the probabability does not actually need to be
	# estimated by the N-R method.
	if(any(!is.finite(paramGuess)))
		stop('paramGuess has non-finite elements')


	jacfun <- getJacFun(startState,txParamList,txParamNames)

	paramDistanceFun <- function(params){

		if(any(is.na(params))){
			msg <- paste('The following probabilities were errant (missing) during Newton-Raphson optimization: ',
				  paste(names(params)[which(is.na(params))],collapse = ', ')
				  ,'.  These parameters may need to be handled external to the analytic model',sep = '')
			stop(msg)
		}

		unityProbs <- (logOddsToProbs(params) == 1)
		if(any(unityProbs)){
			msg <- paste('The following probabilities(s) reached unity: ',
				  paste(names(params)[which(unityProbs)],collapse = ', ')
				  ,'.  These parameters may need to be handled external to the analytic model',sep = '')
			warning(msg)
		}

		startTime <- Sys.time()
		
		switchProbs <- paramsToSwitchProbs(params,txParamNames,txHazList,hazNameMX)

		if(any(is.na(switchProbs)|is.nan(switchProbs))){
			print('any(is.na(switchProbs)|is.nan(switchProbs))')
			while(T)browser()
		}

		mx <- get_pAB_matrix(allGatesMX=sharedParamList[['allGatesMX']],
							 allFromToMX=sharedParamList[['allFromToMX']],
							 switchProbs=switchProbs,
							 txParamNames=txParamNames,
							 txParamList=txParamList,
							 txHazList=txHazList,
							 hazNameMX=hazNameMX,
						     hashSwitchNames=sharedParamList[['hashSwitchNames']],
							 exitSwitches=sharedParamList[['ES']],
							 hasExitHash=sharedParamList[['hasExitHash']],
							 AB_prob_cond_on_switches_hash=sharedParamList[['AB_prob_cond_on_switches_hash']])

		# ------------------------------------------------------------
		# the distance in probability space.  
		# ------------------------------------------------------------
		# note that it does not matter exactly what we return, but
		# it *MUST* be on the same scale as the jacobian!!!
		# the jacobian is calculated for the probabilities 
		# so we retuned values must be in the probability space too!!!
		out <- ((mx %*% startState) - endState)

		# no! this is not the same scale as the jacobian!!!
		# out <- (logodds(mx %*% startState) - logodds(endState))

		elapsed = format(as.numeric(difftime( Sys.time(),startTime ,units = 'secs')),digits=4)
		cat('Total runtime: ',elapsed,'Seconds\n')

		# note that length(out) must equal length(params)
		# but length(endState) is greter than length out.  
		#fixme: # Since sum(endState) == 1, we can omit any one value 
	    #fixme: # and still optimze the same problem...
	    #fixme: rather, i think we're optimizing the arrivals, and the 
		# hazard ratios control whe remaining differences between
		# the start and end states

		# we return the 
		return(out[rownames(sharedParamList[['allGatesMX']]) %in% names(params)])
		
	}

	# use multiroot to opimize the parmeters
	tmp <- multiroot(paramDistanceFun,
			  start = paramGuess,
			  jacfunc=jacfun,
			  jactype='fullusr')
			  
	# prepare the output vector
	out <- rep(0,length(initial__txParamNames))
	names(out) <- initial__txParamNames
	# Here we convert the parameters to probabilitiess b/c the paramters are on the log-odds scale
	outParams <- tmp$root
	out[names(outParams)] <- logOddsToProbs(outParams)
	return(out)
}



paramsToSwitchProbs <- function(params,txParamNames,txHazList,hazNameMX){
	# ------------------------------
	# JUST A BIT OF ERROR CHECKING
	# ------------------------------
	stopifnot(length(params) == length(txParamNames)) 
	if(is.null(names(params)))
		stop("'param' object must have names'")
	if(any(names(params) != txParamNames)){
		print('(names(params) != txParamNames)')
		while(T)browser()
	}
	stopifnot(names(params) == txParamNames) # should be one less than the number of nodes
	#if(any(is.na(params)))browser()
	if(any(is.na(params))){
		print("params argument passed to 'paramsToSwitchProbs' contains missing values" )
		while(T)browser()
		stop("params argument passed to 'paramsToSwitchProbs' contains missing values" )
	}
	# ------------------------------
	# BECAUSE THE PARAMETERS ARE ESTIMATED OVER THE REAL NUMBERS,
	# WE NEED CONVERT THEM TO THE INTERVAL [0,1]
	# ------------------------------
	switchProbs <- logOddsToProbs(params)
	if(!length(txHazList))# fast path out in case there are no hazard ratios to apply
		return(switchProbs)

	# apend the and/or probs to these probabilities
	for(tuple in txHazList)
		switchProbs <- c(switchProbs,
						 if(tuple[3] < 1){
							 if(switchProbs[hazNameMX[tuple[1],tuple[2]]] >=1){
								 print('switchProbs[hazNameMX[tuple[1],tuple[2]]]')
								 browser()
							 }

							 hr_to_and_prob(switchProbs[hazNameMX[tuple[1],tuple[2]]],
											tuple[3])
						 }else{
							 hr_to_or_prob(switchProbs[hazNameMX[tuple[1],tuple[2]]],
											tuple[3])
						 } )
	# NAME THE SWITCH PROBS FOR CONSISTENCY
	names(switchProbs) <- c(txParamNames,
							paste('HR_', 1:length(txHazList),sep = ''))
	return(switchProbs)
}


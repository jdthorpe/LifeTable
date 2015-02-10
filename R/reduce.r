
reduce <- function(sharedParamList,nullSwitches){
	# --------------------------------------------------
	# this function reduces the problem space that will
	# be optimized (if necessary)
	# --------------------------------------------------
	stopifnot(all(!is.null(nullSwitches)))
	stopifnot(length(nullSwitches)>0)
	stopifnot(mode(nullSwitches)=='character')

	# --------------------------------------------------
	# reduce the proportional hazard matrix
	# --------------------------------------------------
	reduced_propHazMx <- sharedParamList$propHazMx
	for(name in nullSwitches)
		reduced_propHazMx[sharedParamList$hazNameMX == name] <- 1

	# --------------------------------------------------
	# txHazList 
	# --------------------------------------------------
	# (calculated identically to the orignal variable
    # but using the reduced propHazMx and txHazNameMat)

	reduced_txHazList <- list() # (tx == transition)
	for(i in 1:nrow(reduced_propHazMx))
	for(j in 1:nrow(reduced_propHazMx)){
		if(is.na(reduced_propHazMx[i,j]) || reduced_propHazMx[i,j] == 1)
			next
		reduced_txHazList <- c(reduced_txHazList,
					   list(c(i,j,reduced_propHazMx[i,j])))
	}

	# --------------------------------------------------
	# reduce the hazard name matrix
	# --------------------------------------------------
	reduced_hazNameMX <- sharedParamList$hazNameMX
	for(name in nullSwitches)
		reduced_hazNameMX[reduced_hazNameMX == name] <- NA

	# --------------------------------------------------
	# txParamNames
	# --------------------------------------------------
	reduced_txParamNames <- uniqueStrings(reduced_hazNameMX)

	# --------------------------------------------------
	# txHazNameMat
	# --------------------------------------------------
	(reduced_txHazNameMat <- sharedParamList$txHazNameMat)
	reduced_txHazNameMat[sharedParamList$hazNameMX %in% nullSwitches] <- NA

	# --------------------------------------------------
	# exit switches
	# --------------------------------------------------
	retuced_ES <- apply(rbind(reduced_txHazNameMat, reduced_hazNameMX ), 2,uniqueStrings)

	# --------------------------------------------------
	# txParamList - just remove the unneeded entries...
	# --------------------------------------------------
	reduced_txParamList <- sharedParamList$txParamList
	for(name in nullSwitches)
		reduced_txParamList[[name]] <- NULL

	# ------------------------------------------------------------
	# reduced_switchNames
	# ------------------------------------------------------------
	reduced_switchNames <- c(reduced_txParamNames,
						   if(length(reduced_txHazList))
							   uniqueStrings(reduced_txHazNameMat))

	# ------------------------------------------------------------
	# allGatesMX
	# ------------------------------------------------------------
	# (calculated identically to the orignal variable
    # but using the reduced propHazMx and txHazNameMat)


	reduced_switchStates <- rep(1,length(reduced_switchNames))
	names(reduced_switchStates) <- reduced_switchNames

	reduced_allGatesMX <- allGatesMX_alt <- switchStatesToGateMx(
							 reduced_switchStates,
							 reduced_txParamList,
							 reduced_txHazList,
							 reduced_txParamNames)


	# ------------------------------------------------------------
	# allFromToMX
	# ------------------------------------------------------------
	reduced_allFromToMX <- matrix(F,length(states),length(states))
	for(from in 1:length(states))
	for(to in 1:length(states))
		if(length(alt_pathsFromAtoB(from,to,reduced_allGatesMX)))
			reduced_allFromToMX[from,to] <- T

#-- 	print('allFromToMX')
#-- 	print('allFromToMX')
#-- 	print('allFromToMX')
#-- 	print('allFromToMX')
#-- 	while(T)browser()

	# ------------------------------------------------------------
	# return the reduced parameters
	# ------------------------------------------------------------

	return(list('ES'=retuced_ES,
				'txParamNames'=reduced_txParamNames,#txParamNames,
				'allGatesMX'=reduced_allGatesMX,#allGatesMX,
				'txHazList'=reduced_txHazList,#txHazList,
				'txParamList'=reduced_txParamList,#txParamList,
				'txHazNameMat'=reduced_txHazNameMat,
				'allFromToMX'=reduced_allFromToMX,#allFromToMX, 
				# parameters
				'propHazMx'=reduced_propHazMx,
				'hazNameMX'=reduced_hazNameMX,
				# these are unchanged...
			  	'hashSwitchNames'=sharedParamList$hashSwitchNames,
				'hasExitHash'=sharedParamList$hasExitHash,
				'AB_prob_cond_on_switches_hash'=sharedParamList$AB_prob_cond_on_switches_hash,
				# don't clear the hashes
				'clear'=function(){ # pass
				}))
}

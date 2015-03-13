#' generate paramters for a set of life table calculaions
#' 
#' generate paramters for a set of life table calculaions.
#' @export
#' @importFrom rootSolve multiroot
#' @param hazNameMX A parameter matrix (character) see example below for details
#' @param propHazMx A proportional hazards matrix 
#' 
prepParams <- function(hazNameMX,  # a parameter matrix (character) see example below for details
					   propHazMx){ # a proportional hazards matrix (numeric) see example below for details

	startTime <- Sys.time()

	# validate the hazNameMX and propHazardMx
	stopifnot(nrow(hazNameMX) == ncol(hazNameMX))
	stopifnot(nrow(propHazMx) == ncol(hazNameMX))
	stopifnot(ncol(propHazMx) == ncol(hazNameMX))
	stopifnot(mode(propHazMx) == 'numeric')
	stopifnot(mode(hazNameMX) == 'character')
	if(any(propHazMx != 1 & is.na(propHazMx))){
		print('proportional hazards may only differ from 1 when a transition probability is specified in hazNameMX')
		browser()
		stop('proportional hazards may only differ from 1 when a transition probability is specified in hazNameMX')
	}
	stopifnot(all(propHazMx == 1 | !is.na(propHazMx))) # same


	# CREATE A LIST CONTAINING THE PARAM NAMES AND COORDINATES
	txParamList <- list() # (tx == transition)
	validateMatrix(0 + !is.na(hazNameMX))
	for(i in 1:nrow(hazNameMX))
	for(j in 1:nrow(hazNameMX)){
		if(is.na(hazNameMX[i,j]))
			next
		if(i == j)
			stop('hazNameMX may not have loops (non-missing diagonal elements, in this case)')
		if(! hazNameMX[i,j] %in%  names(txParamList))
			txParamList[[hazNameMX[i,j]]] <- list(c(i,j))
		else
			txParamList[[hazNameMX[i,j]]] <- c(txParamList[[hazNameMX[i,j]]],
										   list(c(i,j)))
	}

	# typically <0.01 second...
	#elapsed = format(as.numeric(difftime( Sys.time(),startTime ,units = 'secs')),digits=4)
	#cat('prepTime A : ',elapsed,'Seconds\n')

	# CREATE A LIST CONTAINING THE hazard ratios AND their COORDINATES
	txHazNameMat <- propHazMx 
	txHazNameMat[ ] <- as.character(NA)
	txHazNameMat[which(propHazMx !=1) ] <- paste('HR_', 1:sum(propHazMx !=1),sep = '')
	hrCount <- 0
	txHazList <- list() # (tx == transition)
	for(i in 1:nrow(propHazMx))
	for(j in 1:nrow(propHazMx)){
		if(is.na(propHazMx[i,j]) || propHazMx[i,j] == 1)
			next
		if(i == j){
			stop('propHazMx may not have loops (diagonal elts in this case)')
		}
		hrCount <- hrCount + 1
		# fixme: get rid of this, and possibly also the 'txHazList' variable
		if(!txHazNameMat[i,j] == paste('HR_', hrCount,sep = '')){
			print("(!txHazNameMat[i,j] == paste('HR_', hrCount,sep = ''))")
			pause <- TRUE
			while(pause)browser()
		}
		txHazList <- c(txHazList,
					   list(c(i,j,propHazMx[i,j])))
	}

	# GET THE UNIQUE TRANSITION PARAMETER NAMES
	txParamNames <- uniqueStrings(hazNameMX)

#-- 	replaced with the much faster code (which depends on the fact that 
#-- 	all the switches are and/ or rules and no switches are negated...
#-- 	allGatesMX <- allPossibleGates(length(txParamNames)+ length(txHazList), 
#-- 						 c(txParamNames,
#-- 						   if(length(txHazList))
#-- 							   paste('HR_', 1:length(txHazList),sep = '')),
#-- 						 # ... args
#-- 						 txParamList,
#-- 						 txHazList,
#-- 						 txParamNames)

	switchNames <- c(txParamNames,
						   if(length(txHazList))
							   uniqueStrings(txHazNameMat))

	switchStates <- rep(1,length(switchNames))
	names(switchStates) <- switchNames
	allGatesMX <- allGatesMX_alt <- switchStatesToGateMx(switchStates,
							 txParamList,
							 txHazList,
							 txParamNames)

	# typically <0.01 second...
	#elapsed = format(as.numeric(difftime( Sys.time(),startTime ,units = 'secs')),digits=4)
	#cat('prepTime B : ',elapsed,'Seconds\n')

	#print(paste('VALIDATING allPossibleGates'))
	validateMatrix(allGatesMX)

	allFromToMX <- matrix(F,length(states),length(states))
	for(from in 1:length(states))
	for(to in 1:length(states))
		if(length(alt_pathsFromAtoB(from,to,allGatesMX)))
			allFromToMX[from,to] <- TRUE

	exitSwitches <- function(node){
		# return a vector of switch names that 
		# affect the chance that a person 
		# exits a given node
		a <- uniqueStrings(hazNameMX[,node])
		b <- sapply(txHazList,function(x)x[2] == node)
		if(any(b))
			return(c(a,paste('HR_',which(b),sep = '')))
		else 
			return(a)
	}
	allExitSwitches <- function(){
		# returns a list of switches that effect the 
		# ability of an individual to leave this node
		out <- list()
		for(i in 1:nrow(hazNameMX))
			out[[i]] <- exitSwitches(i)
		names(out) <- rownames(hazNameMX)
		return(out)
	}

	ES <- allExitSwitches()

	nSwitches <- (length(txParamList) + length(txHazList))

	# typically <0.1 second...
	# elapsed = format(as.numeric(difftime( Sys.time(),startTime ,units = 'secs')),digits=4)
	# cat('prepTime C : ',elapsed,'Seconds\n')

	# ------------------------------------------------------------
	# set up the switchToGateMx Hash
	# ------------------------------------------------------------

	hasExitHash <- hash::hash()
	AB_prob_cond_on_switches_hash <- hash::hash()

	switchStates <- rep(0,nSwitches)
	names(switchStates) <- c(txParamNames,
					   if(length(txHazList))
						   paste('HR_', 1:length(txHazList),sep = ''))

	cat('Generating ', (2^nSwitches),' state matricies [ = 2^(',length(txParamList),'states + ',length(txHazList),'hazard parameters) ]')
	for(i in 1:2^nSwitches){

		(switchesHashKey <- rawToChar(as.raw(as.integer(intToBits(i)) + 48)[1:nSwitches]))
		(switchStates[] <- as.integer(intToBits(i)[1:nSwitches]))
		# FIXME: could make this contitional on any(switchStatesToGateMx(...))
		(gateStates <- switchStatesToGateMx(switchStates,
											 txParamList,
											 txHazList,
											 txParamNames))

		hasExitHash[[switchesHashKey]] <- apply(gateStates==1,2,any)
		# fixme: states is only inherited by convention
		for(from in 1:length(states))
		for(to in 1:length(states)){
			(pathList <- alt_pathsFromAtoB(from,to,gateStates))
			if(!length(pathList))
				next
			pathSwitchHashKey <- paste(switchesHashKey,from,to,sep =':')

			# calculate AB_prob_cond_on_switches
			pAB <- 0
			for(path in pathList){
				# for each segment in the path
				p_take_this_path <- 1
				for(si in 1:(length(path)-1)){ # si is for segment i
					# the probabilty that the paths is taken 
					# is 1 /  the number of paths that can be taken 
					# from that node given the gates that are open.
					p_take_this_path <- p_take_this_path /sum(gateStates[,path[si]])
				}
				pAB = pAB + p_take_this_path
			}
			if(pAB>0)
				AB_prob_cond_on_switches_hash[[pathSwitchHashKey]] <- pAB
		}
	}


	elapsed = format(as.numeric(difftime( Sys.time(),startTime ,units = 'secs')),digits=4)
	cat('Final prepTime : ',elapsed,'\n')
	return(list('ES'=ES,
				'txParamNames'=txParamNames,
				'allGatesMX'=allGatesMX,
				'txHazList'=txHazList,
				'txParamList'=txParamList,
				'hasExitHash'=hasExitHash,
				'allFromToMX'=allFromToMX,
				'hashSwitchNames'=switchNames,
				'AB_prob_cond_on_switches_hash'=AB_prob_cond_on_switches_hash,
				'txHazNameMat'=txHazNameMat,
				'clear'=function(){
					hash:::clear(AB_prob_cond_on_switches_hash)
					hash:::clear(hasExitHash)
			   	},
				# the parameters to this whole thing...
			   	'hazNameMX'=hazNameMX,
				'propHazMx'=propHazMx))
}


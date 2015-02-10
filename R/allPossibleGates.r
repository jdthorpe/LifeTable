
allPossibleGates <- function(nSwitches=length(switchNames),
							 switchNames=NULL,
							 ...){
	# a convenience function: iterates through all the possible gate 
	# states to determin the set of all gates between nodes that 
	# may be opened with some combination of switches
	startTime <- Sys.time()
	out <- F
	switchStates <- rep(0,nSwitches)
	if(!is.null(switchNames))
		names(switchStates) <- switchNames
	for(i in 1:2^nSwitches){
		# set the switch states 
		switchStates[] <- as.integer(intToBits(i)[1:nSwitches])
		#names(switchStates) <- switchNames
		# GET THE GATE STATS
		tryCatch({
			out = out |  switchStatesToGateMx(switchStates,...)
		},error=function(x){
			print(paste('error in switchStatesToGateMx ',i))
			while(T)browser()
		})
	}
	elapsed = format(as.numeric(difftime( Sys.time(),startTime ,units = 'secs')),digits=4)
	cat('allPossibleGates runtime: ',elapsed,'Seconds\n')
	return(out)
}


#-- allPossibleGates <- local({
#-- 
#-- 	# this funciton is defined in a 'local()' environment so we don't
#-- 	# have to unlock the package bindings for these two cache variables
#-- 	use_allPossibleGates_cache <- TRUE
#-- 	allPossibleGates_cache <- list(NA,NA)
#-- 
#-- 	function(nSwitches=length(switchNames),
#-- 								 switchNames=NULL,
#-- 								 validate=TRUE,...){
#-- 		if(use_allPossibleGates_cache){
#-- 			cacheID <- c(list(nSwitches),list(...))
#-- 				if(identical(allPossibleGates_cache[[1]] ,cacheID)){
#-- 					# return the cached result
#-- 					return(allPossibleGates_cache[[2]])
#-- 			}else{
#-- 				# set a new cacheID
#-- 				#print('resetting allPossibleGates Cache')
#-- 				allPossibleGates_cache[[1]] <<- cacheID
#-- 			}
#-- 		}
#-- 
#-- 		# a convenience function: iterates through all the possible gate 
#-- 		# states to determin the set of all gates between nodes that 
#-- 		# may be opened with some combination of switches
#-- 		out <- FALSE
#-- 		switchStates <- rep(0,nSwitches)
#-- 		if(!is.null(switchNames))
#-- 			names(switchStates) <- switchNames
#-- 		for(i in 1:2^nSwitches){
#-- 			# set the switch states 
#-- 			switchStates <- as.integer(intToBits(i)[1:nSwitches])
#-- 			names(switchStates) <- switchNames
#-- 
#-- 			# GET THE GATE STATS
#-- 			out = out |  switchStatesToGateMx(switchStates,...)
#-- 		}
#-- 		if(validate){
#-- 			#print(paste('VALIDATING allPossibleGates Cache',i))
#-- 			validateMatrix(out)
#-- 		}
#-- 
#-- 		if(use_allPossibleGates_cache)
#-- 			# and finally cashe the result
#-- 			allPossibleGates_cache[[2]] <<- out
#-- 		return(out)
#-- 	}
#-- 
#-- })


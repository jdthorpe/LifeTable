
#' Compute a set of Discrete Hazards
#' 
#' Compute a set of Discrete Hazards.
#' 
#' This function takes (1) a dataset where each row contains the  
#' distribution of population across the model states at a particular age, and
#' matricies that specify (2) the transition probabilities that are to be estimated
#' and (3) the hazard ratios that are to be applied to variaous transitions.  
#' 
#' @export 
#' @family lifetable
#' 
#' @param lifeTable a data.frame with one row per age, ordered by age, where
#' each row represents the distribution of the population across the model 
#' states at the \strong{start} of each interval.
#' 
#' @param stateFields  (optional, default: names(lifeTable)) The fields in the
#' datafraem which contain the counts or probabilies for each state.
#' 
#' @param ctsHazMxList a list of transition matricies
#' 
#' @param hazNameMX A character matrix  with names of the transition probabilies
#' to be estimated .  Entries that are NA specify transitions that cannot be
#' directly made in the model.  (see example below for details).
#' 
#' @param propHazMx A numeric matrix specifying the proportional hazards (see
#' example below for details)
#' 
#' @param file (optional) Either a character string naming a file or a 
#' connection open for writing.  '""' indicates output to the console.  This 
#' can be useful for monitoring a long running procedure.
#' 
#' @return A set of absolute hazards for each state-transition and each age
#' interval
#' 

#-- distByAge2DiscreteHazards <- function(lifeTable,
#-- 							   stateFields = names(lifeTable),
#-- 							   ctsHazMxList,# a list of transition matricies
#-- 							   hazNameMX ,# a parameter matrix (character) see example below for details
#-- 							   propHazMx, # a proportional hazards matrix (numeric) see example below for details
#-- 							   file){

distByAge2DiscreteHazards <- function(lifeTable,# a data.frame with one row per age, ordered by age
							   stateFields = names(lifeTable),# (optional) the fields in the datafraem which contain the counts or probabilies for each state
							   ctsHazMxList,# a list of transition matricies
							   sharedParamList,
							   # fixme: dtos() is not available to all users
							   writeFile=file.path(tempdir(),
												   paste(format(as.Date(date(), "%a %b %d %H:%M:%S %Y"), "%Y%m%d"),
														 '_distByAge2DiscreteHazards_tmp_outputs.csv',sep =''))
							   ){
	#FIXME (1) : estimate memory use
	#FIXME (2) : catch memmory allocation errors

	hazNameMX=sharedParamList[['hazNameMX']]# a parameter matrix (character) see example below for details
	propHazMx=sharedParamList[['propHazMx']]# a proportional hazards matrix (numeric) see example below for details

	# VALIDATE THE INPUTS
	stopifnot(all(stateFields %in% names(lifeTable)))
	stopifnot(length(stateFields) == nrow(hazNameMX))
	stopifnot(length(stateFields) == ncol(hazNameMX))
	stopifnot(length(stateFields) == nrow(propHazMx))
	stopifnot(length(stateFields) == ncol(propHazMx))
	stopifnot(nrow(lifeTable) == length(ctsHazMxList)+1)
	if(!all(sapply(ctsHazMxList,function(x)all(x>=0))))
		stop('all matricies in ctsHazMxList must be composed of non-negative values')
#-- 	for(i in 1:length(ctsHazMxList))
#-- 		if(any(apply(ctsHazMxList[[i]],
#-- 					 2,sum) > 1 + 1E-15))
#-- 			stop('Bad ctsHazMxList: columns of each matrix must sum to 1 or less')

	if(!(all(dimnames(hazNameMX)[[1]] %in%  stateFields)))
		warning("It's a good ideat to have rowNames(hazNameMX) and rowNames(hazNameMX) be the same as stateFields")

	# THE STATEFIELDS SHOULD ALL ADD UP TO ONE
	# (tolerate a little bit of error)
	margin1 <- apply(lifeTable[,stateFields],1,sum)
	stopifnot(all(abs(1-margin1)< 1e-14))
	lifeTable <- lifeTable / margin1

	cat('optimizing probablities for this graph:\n')
	print(sharedParamList$allGatesMX)

	# fixme: RowNames should be better 
	finalRowNames <- paste(	rownames(lifeTable)[-nrow(lifeTable)],'to',rownames(lifeTable)[-1])

	changeProbs <- NULL
	for(i in 1:(nrow(lifeTable)-1)){
		print(paste('Row: ',i))
		(startState<-as.numeric(lifeTable[i,stateFields]))
		(endState  <-as.numeric(lifeTable[i+1,stateFields]))
		if(any(is.na(endState)))
			stop("Missing values in life table")
		if(any(is.na(startState)))
			stop("Missing values in life table")


		tryCatch({
			baseTransitionProbs <- optimizeSwitchProbs(startState=startState/ sum(startState), # the probabilies of being in any one state before the transition
											  endState=endState/ sum(endState),# the probabilies of being in any one state after the transition
											  #paramGuess= log(1-exp(-ctsHazMxList[[i]] / propHazMx )[-1,1]),
											  hazardsMX = ctsHazMxList[[i]],
											  sharedParamList=sharedParamList)
		},error=function(e){
			print('ERROR: baseTransitionProbs <- optimizeSwitchProbs( hazNameMX = hazNameMX')
			while(T)browser()
		})

		changeProbs <- rbind(changeProbs,baseTransitionProbs)

		if(length(writeFile) & (!is.na(writeFile[1]))){
			out <- changeProbs
			try({rownames(out) <- finalRowNames[1:nrow(out)]})#fixme don't let this line bottch the whole operation
			try(write.csv(out,writeFile[1]))#fixme don't let this line bottch the whole operation
		}
	}

	# turn the individual probabilities into cumulative distributions 
	rownames(changeProbs) <- finalRowNames
	# the change probs are actually hazards, 
	return(changeProbs)
}


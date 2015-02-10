get_pAB <- function(from,# from node
					to,# to node
					switchProbs, #probabilitis that each switch is open at a any given time
					exitSwitches,
					AB_prob_cond_on_switches_hash,
					hasExitHash,
					hashSwitchNames,
					...){
	# --------------------------------------------------
	# this function calculates the probality of traversing from one node 
	# to the another given the function that maps switch states to gate
	# states
	# --------------------------------------------------

	ARGS <- list(...)
	txHazList <- ARGS[['txHazList']]
	hazNameMX <- ARGS[['hazNameMX']]

	# ------------------------------------------------------------
	# get lists of switch names that affect travel between the 
	# to and from nodes ('betweens'), and that afect exiting the
	# to node ('exits')
	# ------------------------------------------------------------
	# note that because AtoBswitches is calcuated each time get_pAB
	# is called (rather than using a hash) it is slightly slower, 
	# *but* it allows us to modifiy the hazNameMX and txHazList
	# between iterations of the life table processing, which allows
	# us to simply (with minimal coding) and efficiently reduce the 
	# problem that is being solved, when one or more paths between 
	# nodes is not possible at some step of the life table processing
	# (b/c no one experiences an event after or before some age )
	# [in this case no TL's before age 18 or after 65, and no cancer 
	# dx after age 100]

	AtoBswitches <- function(from,to){
		inner <- function(parent,child){
			(childIndex <- which(rownames(hazNameMX)==child))
			(parentIndex <- which(colnames(hazNameMX)==parent))
			# check for modifiers 
			(modSwitches <- which(sapply(txHazList,function(x)(x[1] == childIndex)&(x[2] == parentIndex))))
			if(length(modSwitches))
				(modSwitches <- paste('HR_',modSwitches,sep = ''))
			# include the switches between this node and the parent
			return(c( hazNameMX[child,parent],# the main switch
					 modSwitches))#mofiers
		}

		parents <- colnames(hazNameMX)[!is.na(hazNameMX[to,])]
		out <- character(0)
		for(parent in parents){
			if(parent == from){# the parent is the from node
				out <- unique(c(out,
								inner(parent,to),
								exitSwitches[[parent]]))
			}else{
				tmp <- AtoBswitches(from,parent)
				if(length(tmp))# the parent is connected to the from node
					out <- unique(c(out,
									tmp,
									inner(parent,to),
									exitSwitches[[parent]]))
			}
		}
		return(out)
	}

	(toName <- rownames(hazNameMX)[to])
	(fromName <- rownames(hazNameMX)[from])

	(exits <- exitSwitches[[toName]])
	(betweens <- AtoBswitches(fromName,toName))

	# --------------------------------------------------
	# this utility function allows iteration over just 
	# the combinations of just the switches travel between
	# the FROM and to nodes (subSwitches == betweens)
	# or away from the TO node (subSwitches == exits)
	# --------------------------------------------------
	hashKey <- rep(0,length(hashSwitchNames))
	names(hashKey) <- hashSwitchNames
	getSwichStates <- function(i,subSwitches){
		# make a local copy of this variable
		switchStates <- hashKey
		switchStates[subSwitches] <- as.integer(intToBits(i)[1:length(subSwitches)])
		return(switchStates)
	}

	# ------------------------------
	# SPECIAL CASE
	# ------------------------------
	if(from == to){ 
		#only iterate over the exits
		if(length(exits)){
			pDontExitB <- 0
			totalExitSwitchProb  <- 0
			for(i in 1:2^length(exits)){
				# set the switch states 
				switchStates <- getSwichStates(i,exits)
				# GET THE PROBAILITY OF THIS PARTICULAR STATE
				# independence means we only have to include the 
				# switches that are being iterated over in the 
				# calculation of the probability
				pSwitchStates <-  prod(ifelse(switchStates[exits], switchProbs[exits], 1-switchProbs[exits]))
				if(is.na(pSwitchStates)){
					print('if(is.na(pSwitchStates))')
					while(T)browser()
				}

				totalExitSwitchProb <- totalExitSwitchProb +  pSwitchStates

				# GET THE GATE STATS
				(switchesHashKey <- paste(switchStates,collapse = ''))
				(exitIsOpen <-  hasExitHash[[switchesHashKey]] )

				# IF THERE IS AN OPEN GATE LEADING AWAY FROM THE END POINT
				# THEN THE PATH DOES NOT END AT THIS POINT
				if(exitIsOpen[to])
					next
				pDontExitB = pDontExitB + pSwitchStates
			}
		} else { 
			# now way out means probablitiy of staying here == 1
			pDontExitB <- 1
		}

		return(pDontExitB)
	}

	# ------------------------------
	# USUAL CASE
	# ------------------------------
	# for each combination of switches, calculate the 
	# probablity of that combination, and the 
	# probablility that the path is taken conditional
	# on that combination

	# is this un-necessary? yes!
	if(length(intersect(betweens,exits))){
		betweens <- union(betweens,exits)
		exits = character(0)
		#reset the exit probability to zero...
		pDontExitB <- 1
	}

	#only iterate over the exits $ same code as the from==to section above
	if(length(exits)){
		pDontExitB <- 0
		#totalExitSwitchProb  <- 0
		for(i in 1:2^length(exits)){
			# set the switch states 
			switchStates <- getSwichStates(i,exits)
			# GET THE PROBAILITY OF THIS PARTICULAR STATE
			# independence means we only have to include the 
			# switches that are being iterated over in the 
			# calculation of the probability
			pSwitchStates <-  prod(ifelse(switchStates[exits],
										switchProbs[exits],
										1-switchProbs[exits]))
			if(is.na(pSwitchStates)){
				print('if(is.na(pSwitchStates))')
				while(T)browser()
			}
			# GET THE GATE STATS
			(exitIsOpen <-  hasExitHash[[paste(switchStates,collapse = '')]] )

			# IF THERE IS AN OPEN GATE LEADING AWAY FROM THE END POINT
			# THEN THE PATH DOES NOT END AT THIS POINT
			if(exitIsOpen[to])
				next
			pDontExitB = pDontExitB + pSwitchStates
		}
	} else { 
		# now way out means probablitiy of staying here == 1
		# -- or -- exits are not distinct from betweens....
		pDontExitB <- 1
	}

	pAB <- 0
	if(length(betweens)){
		#QC alt_contribs <- 0
		# if there are now switches between a and b, then
		# the probability of arriving at B from A is zero.
		for(i in 1:2^length(betweens)){
			# set the switch states 
			switchStates <- getSwichStates(i,betweens)

			# GET THE PROBAILITY OF THIS PARTICULAR STATE
			pSwitchStates = prod(ifelse(!!switchStates[betweens],switchProbs[betweens],1-switchProbs[betweens]))

			# GET THE GATE STATES
			(switchesHashKey <- paste(switchStates,collapse = ''))

			# IF THERE IS AN OPEN GATE LEADING AWAY FROM THE END POINT
			# THEN THE PATH DOES NOT END AT THIS POINT
			if(hasExitHash[[switchesHashKey]][to])
				next

			# GET THE set of PATHS beween the 'FROM' TO 'TO' nodes 
			# given the THE CURRENT SET OF OPEN GATES
			AB_prob_cond <- AB_prob_cond_on_switches_hash[[paste(switchesHashKey,from,to,sep =':')]]
			if(is.null(AB_prob_cond))
				next

			# MULTIPLYING BY PSWITCHSTATES REMOVES CONDITIONING ON THE SWITCH STATES
			# independence means we only have to include the 
			# switches that are being iterated over in the 
			# calculation of the probability
			pAB <- pAB + AB_prob_cond*pSwitchStates
		}
	}
	return(pAB*pDontExitB)
}

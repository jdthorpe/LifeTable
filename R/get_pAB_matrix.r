
get_pAB_matrix <- function(allGatesMX,
						   allFromToMX,
						   switchProbs,
						   ...){
	# --------------------------------------------------
	# From a set of switch probabiliteis and a function 
	# that maps switch states to gate states, return the 
	# probablity transistion matrix [pAB] where pAB[i,j]
	# is the probability of transition from node j to
	# node i.
	# --------------------------------------------------
	# calculate parameters
	N <- nrow(allGatesMX)
	# prep containers
	out <- matrix(0,N,N)


	# iterate over the off-diagnoal transitions
	for(i in 1:N)
	for(j in 1:N){
		# if no paths are possible from node i to node j, then nothing to do
		# Fixme: use the path hash table
		if((i!=j) & !allFromToMX[j,i])
			next
		out[i,j] <- get_pAB(to=i,from=j,switchProbs,...)
		if(is.nan(out[i,j])){
			print('stop pAB nan')
			while(T)browser()
		}
	}


	# fill in the probabilities of not transitioning between states
	for(i in 1:N){
		# sum(out[,i] / sum(out[,i]))
		pChangeCategory <- sum(out[,i])
		if(is.na(pChangeCategory)){
			print('if(is.na(pChangeCategory))')
			while(T)browser()
		}
		if(pChangeCategory > 1 ) {
			if (pChangeCategory < 1 + 1E-5){
				# just a little rounding error...
				out[,i] <- out[,i]/pChangeCategory
			} else {
				print('if(pChangeCategory > 1)')
				while(T)browser()
				stop('pChangeCategory cannot be greater than 1')
			}
		}
	}
	return(out)
}


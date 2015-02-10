
switchStatesToGateMx <- function(s, #switchStates
								   txParamList,# transition paramters
								   txHazList, # transition hazards
								   txParamNames,
								   ...){ # transition parameter Names?

	# CHECKING THAT THE SWITCH NAMES ARE AS EXPECTED
	stopifnot(length(s) == (length(txParamList) + length(txHazList)))
	stopifnot(all(names(s) == c(txParamNames,
								if(length(txHazList)) 
									paste('HR_',1:length(txHazList),sep = ''))))
	gates <- matrix(0,length(states),length(states), dimnames = list(states,states))
	if(length(txHazList)){ #** required because we don't want the inner loop to run
		for(txName in txParamNames){
			# foo foo foo foo 
			for(pair in txParamList[[txName]]){
				# foo foo foo foo 
				gate <- s[txName]
				for(i in 1:length(txHazList)){#** the inner loop that fails when !length(txHazList)
					tuple = txHazList[[i]]
					if(all(tuple[1:2] == pair)){
						if(tuple[3] > 1)
							# the hazard is greater than 1 so use an or rule to 
							# increase the probability of the gate openeing
							gate <- gate | s[paste('HR_',i,sep = '')]
							#gate <- gate | s[i +length(txParamNames)]
						else 
							# the hazard is less than 1 so use an and rule to 
							# decrease the probability of the gate openeing
							gate <- gate & s[paste('HR_',i,sep = '')]
							#gate <- gate & s[i +length(txParamNames)]
					}
				}
				if(is.na(gate)){
					print('blahh')
					browser()
				}
				gates[pair[1],pair[2]] <- gate
			}
		}
	}else{
		for(txName in txParamNames){
			gate <- s[txName]
			for(pair in txParamList[[txName]])
				gates[pair[1],pair[2]] <- gate
		}
	}
	return(gates)
}


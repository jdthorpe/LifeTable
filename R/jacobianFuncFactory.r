# --------------------------------------------------------------------------------
# this is a function (of the start state, x0) which returns a 
# function (that depends on the parameter values) which itself,
# returns the Jacovian matrix for the given value of the parameters
# and the start state.  In otherwords this funcion F(x0) returns the 
# function F_{x0}(p) [that's valid LaTeX btw]. 
# --------------------------------------------------------------------------------
getJacFun <- function(x0,txParamList,txParamNames){
	inner <- function(params){
		partials <- list()
		for(a in txParamNames){
			partials[[a]] <- matrix(0,length(x0),length(x0))#,dimnames = list(states,states))
			for(pair in txParamList[[a]]){
				partials[[a]][pair[1],pair[2]] <-
						partialDerivative(lop = params[a] , # log odds of p
										  if(!is.na(propHazMx[pair[1],pair[2]]))
											  propHazMx[pair[1],pair[2]])
			}
		}
		# --------------------------------------------------
		# fill in the diagonal elt's (by defn, columns sum to zero)
		# --------------------------------------------------
		for(a in txParamNames)
		for(i in 1:length(x0))
			partials[[a]][i,i] <- partials[[a]][i,i] - sum(partials[[a]][,i])
		# GET THE JACOBIAN
		Jacobian <- NULL
		tryCatch({
			for(i in 1:(length(txParamNames))) 
				Jacobian <- cbind(Jacobian,partials[[txParamNames[i]]] %*% x0)
		},error=function(e){
			print('error in partials[[txParamNames[i]]] %*% x0')
			print('error in partials[[txParamNames[i]]] %*% x0')
			print('error in partials[[txParamNames[i]]] %*% x0')
			while(T)browser()
		})

		if(any(is.nan(Jacobian))){
			print('any(is.nan(Jacobian))')
			print('any(is.nan(Jacobian))')
			print('any(is.nan(Jacobian))')
			while(T)browser()
		}

#-- 		cat('Jacobian: \n')
#-- 		print(Jacobian[states %in% txParamNames,])
		#pause=T; while(pause) browser()

		# fixme: states is only availble here by coincidence...
		return(Jacobian[states %in% txParamNames,])
	}
	return(inner)
}


# --------------------------------------------------------------------------------
# a helper function to calculate the derivatives (aka elts of the jacobian) 
# with respect to p of the modified probiability (1- (1-P)^hr)
# where the log-odds of 'P' is specified by the parameter 'lop'.
# --------------------------------------------------------------------------------
partialDerivative <- function(lop,# the log odds of the base probability
							  hr = 1# the hazard rate applied to the base probability
							  ){
	E <- exp(lop)
	if(hr == 1)
		return( E / ((1+E)^2))
	else
		return( hr*(1-(E / (1+E)))^(hr-1) * (E / ((1+E)^2)))
}


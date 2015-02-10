
# --------------------------------------------------
# a guess (for the initial value) of the parameters
# --------------------------------------------------
# so it turns out that this is pretty importatn b/c 
# the altorithm converges slowly *and* I don't think 
# that convergence is guaranteed if the guess isn't 
# good
#
# this function is gives the exact solution to the 
# related problem where there are no proportional 
# hazards and there is no chance of making two state
# changes within one time interval.
# 
# Hence, if the guesses aren't good enough, you can 
# subdivide the time intervals into smaller intervals
# so that it is far less likely that two events
# happen in the same time interval
# --------------------------------------------------
# this function is qc'd in the file with the suffix 'withGuessFunQC.r'
# --------------------------------------------------
# --------------------------------------------------

# fixme: not used:??
paramGuess  <- function(...){

	# ------------------------------------------------------------
	# The function that acutally does the guessing
	# ------------------------------------------------------------
	inner <- function(startState,endState,txParamNames,txParamList){
		# GET THE LIST OF MATRICIES THAT DESCRIBE THE CONTRIBUTION OF 
		# EACH PARAMTER TO EACH OFF DIAGNONAL ELT OF THE TRANSITION MX
		partials <- list()
		for(a in txParamNames){
			partials[[a]] <- matrix(0,length(states),length(states),dimnames = list(states,states))
			for(pair in txParamList[[a]])
				partials[[a]][pair[1],pair[2]] <- 1
			# initial QC
			stopifnot(all(partials[[a]] == partials[[a]]))
		}
		# FILL IN THE DIAGONAL ELT'S (COLUMNS SUM TO ZERO) (inflow == outflow)
		for(a in txParamNames)
			for(i in 1:length(startState))
				partials[[a]][i,i] <-  - sum(partials[[a]][,i])
		# GET THE MATRIX M THAT WE WILL INVERT TO GET OUR SOLUTION
		M <- NULL
		for(i in 1:(length(startState)-1)) 
			M <- cbind(M,partials[[txParamNames[i]]] %*% startState)
		# APPLY THE INVERS OF M TO THE TRANSITION TO GET THE GATE PROBABILITIES
		simpleTransitionProbs <- solve(M[-1,]) %*%(endState - startState)[-1]
		# CONVERT TO THE LOG-ODDS SCALE (the scale of the parameters)
		tryCatch({ 
			paramGuesses <- logodds(as.numeric(simpleTransitionProbs))
		},error = function(e){
			print('paramGuesses <- logodds(as.numeric(simpleTransitionProbs))')
			while(T)browser()
		})
		names(paramGuesses) <- txParamNames
		return(paramGuesses)
	}

	# ------------------------------------------------------------
	# handling for the guessing function
	# ------------------------------------------------------------
	withCallingHandlers({out <-  inner(...)
		}, error = function(e){
			if( identical(as.character(e$call),c("logodds","as.numeric(simpleTransitionProbs)")) & 
			   (e$message == 'x must be beween 0 and 1')){
				invokeRestart('muffleWarning')
			}
		}, warning = function(w){
			if(identical(as.character(w$call),c("log","x/(1 - x)")) & 
			   (w$message == "NaNs produced")){
				invokeRestart('muffleWarning')
			}
		} )
	if(any(is.nan(out))){
		#browser()
		out[is.nan(out)] <- -100
		warning(NaN_Warning)
	}
	return(out)
}



NaN_Warning <- "NaN's in parameter guess (paramGuess(...)).  This is
usually caused by a row in the data in which there is zero risk of one
or more events This can happen when there is zero risk of an an event
occuring during some period of life.  For example, in the old model,
Benign disease does not occure past age 95 and malignant tumors do not
occure beyond age 100.  This will result in small but positive hazard
estimates for such an outcome (typically hazard < 10^-16).

Work arounds include (1) ignoring this warning, (2) modifitying the
probability distributions, (3) specifiying a separate risk graph for
age's that cause this warning, or (4) if the only risk in the final
period is XXOC, then dropping that row from the dataset, and appending a
row in which the hazard of XXOC is 1 and the remaining hazards are zero.

Option 4 may also be desireable so that XXOC ends up as a true CDF
(that goes from 0 to 1) so that everyone in the model is assigned
and age of death from other causes.
"


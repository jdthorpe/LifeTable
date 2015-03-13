
#' Compute a single increment of a life table.
#' 
#' Compute a single increment (step) of a life table given the initial state
#' and the transition (Rate) matrix.  As a conveninece, a certian number of
#' sub-steps (\code{steps}) can be returned. 
#' 
#' @export
#' @family lifetable
#' 
#' @param x0 A vector with the distribution of the population
#' across the state space at the start of the life table iteration.  
#' 
#' @param M A matrix with the abosolute (net) risks of transition from any
#' state in the state-space model to another.
#' 
#' @param steps positive integer. The number of rows to return in the life table for each 
#' row in the parameter \code{M}.  Setting \code{stpes = n} results in the addtion
#' of \code{n-1} intermediate records in the return value.
#' 
#' @note
#' This implements a solution to a Homogeneous Linear Systemwith Constant 
#' Coefficients described in section 7.5 of "Elementary Differential
#' Equations and Boundary Value Problems" by Boyce and Diprima, 6th 
#' Edition.  
#' 
#' NOTE THAT THIS FUNCTION REQUIRES RATES AND NOT COUNTS OR PROBABILITEIS!
#' if you have expected counts (i.e. proportions of the \strong{entire}
#' birth cohort) of event (arrivals at a node) within an 
#' interval, use \code{\link{countsToLifeTable}}

continuousHazardsToLifeTableStep <- function(x0, M, steps=1){

	popSize <- sum(x0)
	tmp_x <- x0
	tmp_M <- M

	# implements the solution to 
	if((steps < 1) || 
	   (steps != as.integer(steps)))
		stop("parmaeter 'steps' must be a positive integer")
	if(any(M < 0))
	   stop('All Rates Rates must all be positive')
	if(any(diag(M)!=0))
		stop('Diagonal elements should be zero')
	if(any(apply(M,2, function(x0){sum(!is.finite(x0))>1})))
		stop('If a rate in any column is infinite, then the remaining values must be zero')


	# handle the case where all of the members from one node are destined to go to another node
	#columnsWithUnity <- which(apply(M,2,function(x0)any(x0 == 1)))
	columnsWithInf <- which(apply(M,2,function(x0)any(!is.finite(x0))))
	if(length(columnsWithInf)){
		# --------------------------------------------------
		# special case
		# --------------------------------------------------
		# drop the row and column corresponding to the
		# node with the infinite outflow

		# identify the source noede (d) (which will be [d]ropped from the M)
	    # where all individuals are headed for the sink with 100% probablity

		(d <- columnsWithInf[1] )
		(s <- which(!is.finite(M[,d])))# the [s]ink where all the inidiviuals source node will go
		stopifnot(length(s) == 1)# this *should* be assured by the second 'stop()' above
		M[s,] <- M[s,] + M[d,]
		(M <- M[-d,-d])

		if(all(M == 0)){
			# special case: only one transition specified
			# --> simply shift the source to the sink
			# and call it good
			if(steps == 1){
				# return a vector
				out <- x0
				out[d] <- 0
				out[s] <- out[s] + x0[d] 
			} else{
				# return a matrix
				out <- NULL
				for(stp in 1:steps){
					tmp <- x0
					tmp[d] <- x0[d] * (1-stp/steps)
					tmp[s] <- tmp[s] + (x0[d] * stp/steps)
					out <- rbind(out,tmp)
				}
			}
		} else {
			# there is other work to do besides shift a single risk pool to it's sink
			if(steps == 1){ 
				out <- continuousHazardsToLifeTableStep(x0[-d],M)

				# expand the vector
				out <- expand(out,d)
				out[s] <- out[s] + x0[d] 
			}else{ 
				# there is other work to do besides shift the source to the sink
				out <- expandMX(continuousHazardsToLifeTableStep(x0[-d],M,steps),d)

				# expand the matrix
				times <- (1:steps) / steps
				delta <- x0[d] * (times)
				out[,d] <- x0[d] - delta
				out[,s] <- delta
			}
		}
	} else {
		# --------------------------------------------------
		# ELSE usual case: graph where all transition rates 
		# are fintie (Nt necessarily less than 1

		# --------------------------------------------------
		# This implements a solution to a Homogeneous Linear System with Constant 
		# Coefficients described in section 7.5 of "Elementary Differential
		# Equations and Boundary Value Problems" by Boyce and Diprima, 6th 
		# Edition.  
		# --------------------------------------------------

		diag(M) <- 0
		diag(M) <- -apply(M,2,sum) # inflow == outflow
		eig <- eigen(M)
		# the constants
		cn <- solve(eig$vector)%*%as.numeric(x0)
		# the general solution
		(g <- t(t(eig$vector)*as.numeric(cn)))
		if(steps == 1){
			#return a vector
			out <- as.numeric(g%*%exp(eig$values))# implicitly t = 1 ... g*exp(eig$values * t)
		}else{
			#return a matrix
			out <- NULL
			for(s in 1:steps){
				times <- (s/steps)
				out <- rbind(out,t(g%*%exp(eig$values*times)))
			}
		}
	}

# FIXME: make sure rounding error in the life table is contained!!!
# FIXME: make sure rounding error in the life table is contained!!!
# FIXME: make sure rounding error in the life table is contained!!!
# FIXME: make sure rounding error in the life table is contained!!!

	if(inherits(out,'matrix')){
		for(rowno in nrow(out)){
			if(sum(out[rowno,])-popSize!=0){
				#print('fixing matrix error')
				if(abs(sum(out[rowno,])-popSize)/popSize > 1E-10){
						print('"(abs(sum(out[rowno,])-popSize) > 1E-10)" in continuousHazardsToLifeTableStep')
						while(TRUE)browser()
				}
				iter <- 0
				while(sum(out[rowno,])-popSize!=0){

					#cat('fixing row error: ',sum(out[rowno,])-popSize,', in row',rowno,'\n')
					out[rowno,] <- out[rowno,] * (popSize/ sum(out[rowno,]))
					iter <- iter + 1
					if(iter > 10){
						if(abs(sum(out[rowno,])-popSize)/popSize<1E-15)
							break # you can only try so hard...
						print('"(sum(out[rowno,])-popSize!=0)" in continuousHazardsToLifeTableStep')
						while(TRUE)browser()
					}
				}
			}
		}
	}else{
		if(sum(out)-popSize!=0){
			if(abs(sum(out)-popSize)/popSize > 1E-10){
					print('"(abs(sum(out)-popSize) > 1E-10)" in continuousHazardsToLifeTableStep')
					while(TRUE)browser()
			}
			iter <- 0
			while(sum(out)-popSize!=0){
				out <- out * (popSize/sum(out))
				iter <- iter + 1
				if(iter > 10){
					if((abs(sum(out)-popSize)/popSize<1E-15))
						break # you can only try so hard...
					print('"(sum(out)-popSize!=0)" in continuousHazardsToLifeTableStep')
					while(TRUE)browser()
				}
			}
		}

	}
	return(out)
}


expandMX <- function(M,indx){
	stopifnot(all(indx == sort(indx)))
	for(i in indx){
		if(i == 1){
			out <- cbind(0,M)
		} else if(i == (ncol(M) + 1)){
			out <- cbind(M,0)
		} else if(i <= ncol(M)){
			out <- cbind(M[1:(i-1)],
						 0,
						 M[i:ncol(M)])
		} else stop("bad 'indx' value")
	}
	return(out)
}

expand <- function(x,indx){
	# insert zeros so that the index values are all zero in the returned vector
	stopifnot(all(indx == sort(indx)))
	for(i in indx){
		if(i == 1){
			x <- c(0,x)
		} else if(i == (length(x)+1)){
			x <- c(x,0)
		} else if(i <= length(x)){
			x <- c(x[1:(i-1)],0,
					   x[i:length(x)])
		} else stop("bad 'indx' value")
	}
	return(x)
}


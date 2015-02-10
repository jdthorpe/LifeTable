# non-exported HELPER FUNCTIONS 

hr_to_and_prob <- function(a,hr){
	# --------------------------------------------------
	# when two proabilities are related by a hazard ratio
	# and one of the probabilities (pA) is known, we can 
	# calculate the other probability (pB) using that hazard ratio.   
	# In this case, we want to find a probability (pC) 
	# such that pB = pA and pC 
	# --------------------------------------------------
	# AN OR RULE ONLY WORKS WITH THE HR (OF B RELATIVE TO PA) 
	# IS GREATER THAN OR EQUAAL TO 1
	if(hr == 1)
		return(1)
	stopifnot(hr <= 1)
	# R is the ratio of pB to pA
	R <- (1 - (1-a)^hr)/a
	# for an and rule, pC is equal to R...
	if(is.na(R)){
		print('is na R')
		browser()
	}
	stopifnot(!is.na(R))
	return(R)
}

hr_to_or_prob <- function(a,hr){
	# --------------------------------------------------
	# when two proabilities are related by a hazard ratio
	# and one of the probabilities (pA) is known, we can 
	# calculate the other probability (pB) using that hazard ratio.   
	# In this case, we want to find a probability (pC) 
	# such that pB = pA or pC 
	# --------------------------------------------------
	# AN OR RULE ONLY WORKS WITH THE HR (OF B RELATIVE TO PA) 
	# IS GREATER THAN OR EQUAAL TO 1
	if(hr == 1)
		return(0)# short cut outta here...
	if((1-a)^hr == 1) # fixme: a rounding error hack.  for really small values of 'a', this is == 1
		return(0)# effectively p(B) == 0, therefore p(C) = 0 the 'or' combination will never be true
	stopifnot(hr>1)
	# R is the ratio of pB to pA
#-- 	R <- (1 - (1-a)^hr)/a
#-- 	if((R<1) & (R> 1 - 1e-14))
#-- 		R <- 1
#-- 	stopifnot(R>=1)
#-- 	# the probabiltiy of the the alternate variable 'c'
#-- 	orProb <- (R-1)*a/(1-a)
	orProb <- 1-(1-a)^(hr-1)
	stopifnot(orProb >= 0)
	stopifnot(orProb <= 1)
	return(orProb)
}


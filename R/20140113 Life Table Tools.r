# --------------------------------------------------------------------------------
# Programmer: Jason Thorpe
# Date        Monday, January 13, 2014 8:36:02 AM
# Language:   R (.r) Version 2.15.0
# Purpose:    a set of tools for calculating life tables when multiple,
# 				 simultaneous risks exist at each step
# Comments:   
# --------------------------------------------------------------------------------



#-- rates <- ratesFromIncidenceCounts(c(0,1,0),# values at the start of the step
#-- 						 observedEvents,# incidence of each observable event relative to sum(x0)
#-- 						 RR)
#-- 
#-- x0 <- c(1,0,0)
#-- observedEvents <- c(0,
#-- 		 AgeClinicalDx_marginalCDF_haz[15],
#-- 		 AgeDeathOtherCauses_marginalCDF_haz[15])
#-- RR <- matrix(c(0,0,0,
#-- 			   1,0,0,
#-- 			   1,0,0),
#-- 		   byrow=TRUE,
#-- 		   3,3)
#-- 
#-- rates <- ratesFromIncidenceCounts(x0,# values at the start of the step
#-- 						 observedEvents,# incidence of each observable event relative to sum(x0)
#-- 						 RR)
#-- 
#-- rates <- ratesFromIncidenceCounts(c(0,1,0),# values at the start of the step
#-- 						 observedEvents,# incidence of each observable event relative to sum(x0)
#-- 						 RR)
#-- 
#-- foo <- ratesToLifeTableStep(x0,rates)
#-- cbind(foo,observedEvents)
#-- 
#-- # the risk years is 
#-- 
#-- # example
#-- 
#-- # getting the transition matrix right is still interesting b/c 
#-- (pcan=AgeClinicalDx_marginalCDF_haz[i])
#-- (pxx=AgeDeathOtherCauses_marginalCDF_haz[i])
#-- mx <- matrix(c(0,0,0,
#-- 			 pcan,0,0,
#-- 			 pxx,0,0),
#-- 		   byrow=TRUE,
#-- 		   3,3)
#-- 
#-- ratesToLifeTableStep
#-- 
#-- ratesToLifeTableStep(x=statesAtAge,# initial values
#-- 								 mx=)# transition matrix






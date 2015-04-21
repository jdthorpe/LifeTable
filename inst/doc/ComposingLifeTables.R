## ----, echo=F------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
# make graphs here.

## ------------------------------------------------------------------------
# add figure here.

## ------------------------------------------------------------------------
library(LifeTable)
data(EOC_develop)
head(EOC_develop,3)

## ------------------------------------------------------------------------
stateTable <- cbind(healthy = 1- EOC_develop$CumProbDevelopCancer 
							   - EOC_develop$CumProbDeathWithoutCancer,
		cancer = EOC_develop$CumProbDevelopCancer,
		DeathWithoutEOC = EOC_develop$CumProbDeathWithoutCancer) 
rownames(stateTable) <- seq(5,100,5)

# for completeness, we can add the state of the population at birth
stateTable <- rbind(
			birth = c( healthy = 1, # everyone starts out without cancer
					cancer = 0, 
					DeathWithoutEOC = 0) ,
			 stateTable)

head(stateTable,3)

## ------------------------------------------------------------------------

# the hypothetical birth cohort of 10,000,000 begins healthy
initialState <- c(healthy=10000000, 
				  cancerOrDead=0,
				  DeathWithoutEOC=0)

# incidence numbers taken from EOC_develop 
incidenceMatrix <- cbind(
		healthy = 0,
		cancer = EOC_develop$DevelopCancer,
		DeathWithoutEOC = EOC_develop$DeathsWithoutCancer)
rownames(incidenceMatrix) <- rownames(EOC_develop)

# Hazard ratios are relative to the othere elements of in the row
modelMatrix <- matrix(
		c(0,0,0,
		  1,0,0,
		  1,0,0),nrow=3,byrow=T)

Cancer_Vs_Death <- lifeTable(
				  initialState=initialState,
				  incidence=incidenceMatrix,
				  hazardModel =modelMatrix )

head(Cancer_Vs_Death,3)


## ------------------------------------------------------------------------
head(Cancer_Vs_Death$states/10000000,3)

## ------------------------------------------------------------------------
head(Cancer_Vs_Death$netHazards,3)

## ------------------------------------------------------------------------
data(GyneclogicalSureryRatesPer100k)

# (Notice that these data use very coarse age categories)
head(GyneclogicalSureryRatesPer100k,3)


# we're intereseted in all excision of the uterus, so we 
# need to include both sub-categories.
AnualHyterectomyRates <- (GyneclogicalSureryRatesPer100k$"Hysterectomy__abdominal_and_vaginal" +
					 GyneclogicalSureryRatesPer100k$"Other_excision_of_cervix_and_uterus") / 100000
AnualTubalLigationRates <- GyneclogicalSureryRatesPer100k$"Ligation_of_fallopian_tubes"/ 100000
AnualOophorectomyRates <- GyneclogicalSureryRatesPer100k$"Oophorectomy__unilateral_and_bilateral"/ 100000


## ------------------------------------------------------------------------
data(US_population_2003)
head(US_population_2003,3)

US_population_2003[20,] <- US_population_2003[20,] + US_population_2003[21,]
US_population_2003 <- US_population_2003[-21,]

scale_factor <- US_population_2003$all / US_population_2003$female

AnualHyterectomyRates   <- AnualHyterectomyRates * scale_factor 
AnualTubalLigationRates <- AnualTubalLigationRates * scale_factor
AnualOophorectomyRates  <- AnualOophorectomyRates * scale_factor

## ------------------------------------------------------------------------
data(EOC_death)
head(EOC_death,3)

# calculate the at-risk population * 
dead  <- EOC_death$"CumProbDeath" 
alive <- 1 - dead

# scale the rates to the proporition of the birth cohort
# that is alive in each age range rather than 100,000
HyterectomyIncidence   <- alive * 5 * AnualHyterectomyRates   / 100000 
TubalLigationIncidence <- alive * 5 * AnualTubalLigationRates / 100000 
OophorectomyIncidence  <- alive * 5 * AnualOophorectomyRates  / 100000 

# (Note that each age category spans 5 years, hence the factor of 5)


## ------------------------------------------------------------------------
# scale the rates to the proporition of the birth cohort
# that is alive in each age range rather than 100,000
HyterectomyIncidence_alt   <- alive * (1-exp( - 5 * AnualHyterectomyRates   / 100000 ))
TubalLigationIncidence_alt <- alive * (1-exp( - 5 * AnualTubalLigationRates / 100000 ))
OophorectomyIncidence_alt  <- alive * (1-exp( - 5 * AnualOophorectomyRates  / 100000 ))

## ------------------------------------------------------------------------

initialState <- c(healthy                = 1, # everyone starts out healthy 
				  TubesLigated           = 0, # incidence: Tubal Ligation
				  NoUterus               = 0, # incidence: hysterectomy
				  NoOvariesAndNoEOC      = 0, # incidence: oophorectomy 
				  OvarianCancer          = 0, # incidence: cancer
				  DeathWithoutEOC        = 0) # incidence: dead without cancer 

IncidenceTable <- cbind(
	healthy              = 0,
	TubesLigated         = TubalLigationIncidence_alt,
	NoUterus             = HyterectomyIncidence_alt,
	NoOvariesAndNoEOC    = OophorectomyIncidence_alt,
	OvarianCancer        = Cancer_Vs_Death$incidence[,'cancerOrDead']/10000000,
	DeathWithoutEOC      = Cancer_Vs_Death$incidence[,'DeathWithoutEOC']/10000000)

hazardModel <- matrix(
	c(  0,0,0,0,0,0,  # healthy              
		1,0,0,0,0,0,  # TubesLigated         
		1,1,0,0,0,0,  # NoUterus             
		1,1,1,0,0,0,  # NoOvariesAndNoEOC 
		1,1,1,0,0,0,  # OvarianCancer        
		1,1,1,1,0,0), # DeathWithoutEOC      
	nrow=6,
	byrow=T)


## ------------------------------------------------------------------------

life_table_with_Procedures <- lifeTable(
		initialState=initialState,
		incidence   =IncidenceTable,
		hazardModel =hazardModel )

head(life_table_with_Procedures)

tail(life_table_with_Procedures,3)


## ------------------------------------------------------------------------

hazardModel_2 <- matrix(c(
		0,0    ,0    ,0,0,0,
		1,0    ,0    ,0,0,0,
		1,1-0.2,0    ,0,0,0, # rate/risk of hysterectomy is 20% lower in women with a prior TL
		1,1    ,1    ,0,0,0,
		1,1-0.3,1-0.4,0,0,0, # rate/risk of EOC is 30% lower in women with a TL and 40% lower in women with prior Hysterectomy.
		1,1    ,1    ,1,0,0),6,byrow=T)

life_table_with_Procedures <- lifeTable(
		initialState=initialState,
		incidence   =IncidenceTable,
		hazardModel =hazardModel )

head(life_table_with_Procedures,3)

tail(life_table_with_Procedures,3)


## ------------------------------------------------------------------------
# Extract the net Hazards matrix
netHazards_TL_at_40 <- life_table_with_Procedures$netHazards

# Set the net hazards of TL to infinity at age 40 which will
# result in all women having their tubes ligated at age 40
#  if they are in the at-risk population for TL at that time
netHazards_TL_at_40['40 <= Age < 45','NoUterus'] <- Inf

# Re-calculat the life table using the modified net hazard matrix
TL_at_40 <- lifeTable( initialState=initialState,
		netHazards  =netHazards_TL_at_40,
		hazardModel =hazardModel_2 )

tail(TL_at_40,3)

## ------------------------------------------------------------------------
# extract the net hazards matrix after age 40
Net_Hazards_after_40 <- life_table_with_Procedures$netHazards[-1:-8,]
head(Net_Hazards_after_40)

# a new initial state starting with the entire population (n=1)  
# in the 'tubes ligated' state.
initialState_TL <- c(healthy        = 0, # everyone starts out healthy 
				  TubesLigated      = 1, # incidence: Tubal Ligation
				  NoUterus          = 0, # incidence: hysterectomy
				  NoOvariesAndNoEOC = 0, # incidence: oophorectomy 
				  OvarianCancer     = 0, # incidence: cancer
				  DeathWithoutEOC   = 0) # incidence: dead without cancer 

# re-calculate the life table begining with TL at age 40
TL_at_age_40 <- lifeTable(
		initialState = initialState_TL,
		netHazards   = Net_Hazards_after_40,
		hazardModel  = hazardModel_2 )

tail(TL_at_age_40,3)


## ------------------------------------------------------------------------
# extract the net hazards matrix after age 40
Net_Hazards_after_40_No_TL <- Net_Hazards_after_40

# set the hazard of TL to zero, so that the woman will not ever undergo TL
Net_Hazards_after_40_No_TL[,'TubesLigated'] <- 0 

No_TL_at_age_40 <- lifeTable(
		initialState = initialState,
		netHazards   = Net_Hazards_after_40_No_TL,
		hazardModel  = hazardModel_2 )

tail(No_TL_at_age_40,3)

## ------------------------------------------------------------------------
data(HistologyByAge)
head(HistologyByAge,3)

initialState <- c('healthy' = 1,
					 'TubesLigated' = 0,
					 'NoUterus' = 0,
					 'NoOvariesAndNoEOC' = 0,
					 'ClearCell' = 0,
					 'Endometrioid' = 0,
					 'Misc' = 0,
					 'Mucinous' = 0,
					 'Serous' = 0,
					 'DeathWithoutEOC ' = 0)

hazardModel <- matrix(
		c(  0,0    ,0    ,0,0,0,0,0,0,0,  # healthy              
			1,0    ,0    ,0,0,0,0,0,0,0,  # TubesLigated         
			1,1-0.2,0    ,0,0,0,0,0,0,0,  # NoUterus             
			1,1    ,1    ,0,0,0,0,0,0,0,  # NoOvariesAndNoEOC 
			1,1    ,1    ,0,0,0,0,0,0,0,  # OvarianCancer: ClearCell     
			1,1    ,1    ,0,0,0,0,0,0,0,  # OvarianCancer: Endometrioid         
			1,1    ,1    ,0,0,0,0,0,0,0,  # OvarianCancer: Misc                
			1,1    ,1    ,0,0,0,0,0,0,0,  # OvarianCancer: Mucinous            
			1,1-0.3,1-0.4,0,0,0,0,0,0,0,  # OvarianCancer: Serous              
			1,1    ,1    ,1,0,0,0,0,0,0), # DeathWithoutEOC      
		10,byrow=T)

hazardModel

# (Just re-asigning for clarity)
.LT = life_table_with_Procedures

IncidenceTable <- cbind(
		"healthy"           = .LT$incidence[,'healthy'],
		"TubesLigated"      = .LT$incidence[,'TubesLigated'],
		"NoUterus"          = .LT$incidence[,'NoUterus'],
		"NoOvariesAndNoEOC" = .LT$incidence[,'NoOvariesAndNoEOC'],
		'ClearCell'         = .LT$incidence[,'OvarianCancer'] * HistologyByAge$ClearCell,
		'Endometrioid'      = .LT$incidence[,'OvarianCancer'] * HistologyByAge$Endometrioid,
		'Misc'              = .LT$incidence[,'OvarianCancer'] * HistologyByAge$Misc,
		'Mucinous'          = .LT$incidence[,'OvarianCancer'] * HistologyByAge$Mucinous,
		'Serous'            = .LT$incidence[,'OvarianCancer'] * HistologyByAge$Serous,
		"DeathWithoutEOC "  = .LT$incidence[,'DeathWithoutEOC' ])

life_table_with_procedures_and_histology <- lifeTable(
		initialState=initialState,
		incidence   =IncidenceTable,
		hazardModel =hazardModel )

tail(life_table_with_procedures_and_histology,3)


## ------------------------------------------------------------------------
all(apply(life_table_with_Procedures$netHazards,1,sum)<= 
	apply(life_table_with_procedures_and_histology$netHazards,1,sum))

## ----,eval=FALSE---------------------------------------------------------
#  generateModelParams <- function(ages=seq(0,100,5),
#  							life_table_with_procedures_and_histology,
#  							what to do here?
#  							use dot args to pass multiple lifetables?
#  							exclude='DeathWithoutEOC')
#  decisionRuleWriter <- function(life_table_with_procedures_and_histology)


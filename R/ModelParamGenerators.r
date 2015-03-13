
#' create XML probabability distributions for PopSim
#' 
#' create XML probabability distributions for PopSim.
#' 
#' @export
#' @param ages a vector of ages representing the cut points.  Note that length(ages)
#'    should equal 1 + nrow(netHazards)
#' 
#' @param netHazards a matrix composed of columns of net hazards, or an object of class 'LifeTable'
#' @param file A writable connection or a character string naming the file to write to.
#' 
#' @examples
#' 
#' # using  "life_table_with_Procedures" from vignette("")
#' generateModelParams(seq(0,100,5),life_table_with_Procedures$netHazards)
#' generateModelParams(seq(0,100,5),life_table_with_Procedures)

generateModelParams <- function(ages,
								  netHazards,
								  hazardNames=colnames(netHazards),
								  file,
								  baseSeed = format(as.Date(date(), "%a %b %d %H:%M:%S %Y"), "%Y%m%d")) { 
	# validate parameters
	if(inherits(netHazards,'LifeTable'))
		netHazards <- netHazards$netHazards
	stopifnot(ncol(netHazards) == length(hazardNames))

	# remove the reference category
	not_Origin = apply(netHazards,2,function(x)any(x > 0))
	netHazards  <-  netHazards[,not_Origin]
	hazNames <- hazNames[not_Origin]

	modelParamCDFs <- apply(netHazards,2,haz2cdf)

	if(!missing(file)){
		sink(file)
		on.exit(sink())
	}

	# THE EVENT DISTRIBUTION PAREMETER
	cat("<eventDistributions type='dict'>",'\n')

		# THE AGE CUTPOINTS 
		cat("   <agePoints type='tuple'>",'\n')
		for(age in ages)
			cat("      <item type='float'>",age,"</item>",'\n',sep = '')
		cat("   </agePoints>",'\n')

		# THE CDF'S FOR EACH DISTRIBUTION
		cat("   <distributions type='dict'>",'\n')
		for(i in 1:ncol(modelParamCDFs)){
			cat("      <",colnames(modelParamCDFs)[i]," type='dict'>",'\n',sep = '')
			cat("         <seed type='float'>",baseSeed,i,"</seed>",'\n',sep = '')
			cat("         <probs type='tuple'>",'\n',sep = '')
			for(j in 1:nrow(modelParamCDFs))
				cat("            <item type='float'>",modelParamCDFs[j,i],"</item>",'\n',sep = '')
			cat("         </probs>",'\n',sep = '')
			cat("      </",colnames(modelParamCDFs)[i],">",'\n',sep = '')
		}
		cat("   </distributions>",'\n')

	cat("</eventDistributions>",'\n')

}


#' create a decision rule module for PopSim
#' 
#' create a decision rule module for PopSim.
#' 
#' @param hazardModel a matrix with the hazard model supplied to `lifeTable`
#' @param additive indicates whether the subsequent relative hazards are applied additively or if they replace the previously applied HR
#' @param file A writable connection or a character string naming the file to write to
#'
#' @export
#' @examples
#' stateNames <- c('H',# healthy
#'			'TL', # tubes ligated
#'			'BSO',# tubes and ovaries removed
#'			'Ben',# diagnosed with Benign Disease
#'			'Can',# cancer
#'			'XX') # dead without cancer
#'
#' hazardModel <- matrix(c(
#'		0,0    ,0    ,0,0,0,
#'		1,0    ,0    ,0,0,0,
#'		1,1-0.2,0    ,0,0,0,
#'		1,1    ,1    ,0,0,0,
#'		1,1-0.3,1-0.4,0,0,0,
#'		1,1    ,1    ,1,0,0),
#'					  ncol=6,
#'					  byrow=TRUE,
#'					  dimnames = list(stateNames,
#'									  stateNames))
#'
#' decisionRuleWriter(hazardModel)
#'


decisionRuleWriter <- function(hazardModel,
							   additive=F,
							   file){

	if(!all(hazardModel>=0))
		stop('hazards must be non-negative')
	if(inherits(hazardModel,'LifeTable'))
		hazardModel <- hazardModel$hazardModel

	origin = which(apply(hazardModel==0,1,all)) # the position of the orign in the matrices

	# the origin implies 'no prior events' and there should be only one 
	# state that represents 'no prior events'.
	stopifnot(length(origin) == 1)

	fromToMx <- hazardModel != 0

	if(!missing(file)){
		sink(file)
		on.exit(sink())
	}

	# indicate whether the subsequent relative hazards are applied additively or if they replace the previously applied HR
	cat('additive = ',if(additive)'True'else'False','\n')

	# get rid of the orign (not needed here)
	fromToMx <- fromToMx[-origin,-origin]
	hazardModel <- hazardModel[-origin,-origin]

#-- 	# bookKeeping to be consistent with the model nomenclature
#-- 	dm <- dimnames(hazardModel) # FIXME: bad assumptionj about dimnames
#-- 	dm[[1]][dm[[1]] == "DeadWithoutCancer"] <- 'death'
#-- 	dm[[2]][dm[[2]] == "DeadWithoutCancer"] <- 'death'
#-- 	dm[[1]][dm[[1]] == "BSO"] <- 'benign'
#-- 	dm[[2]][dm[[2]] == "BSO"] <- 'benign'
#-- 	dimnames(fromToMx)  <- dm
#-- 	dimnames(hazardModel) <- dm

	eventNames = rownames(hazardModel)
	for(i in 1:length(eventNames)){
		nullFun <- TRUE
 		# WRITE THE DECLARAION
		(name = eventNames[i])
		cat("def ",name,'_processor(event):',sep = '')
		# GET THE EXCLUSION/PREVENTION EVENTS
		(priors = fromToMx[name,-i])
		(exclusiveEvents = names(priors)[!priors])
		# GET THE HAZARD RATIOS
		(hazardRatios = hazardModel[name,-i])
		(hazardModifiers = names(hazardRatios)[hazardRatios != 1])

		if(eventNames[i] == 'death'){
			exclusiveEvents <- hazardModifiers <- NULL
		}
		# --------------------------------------------------
		# WRITE THE CODE GETS THE PERSON OBJECT
		# --------------------------------------------------
		if(length(exclusiveEvents) | length(hazardModifiers)){
			cat('
    someone = event.getGlobalEvent()')
		} else{
			cat('\n    pass\n\n')
			next
		}

		# --------------------------------------------------
		# write the code that handles the hazard modifications(acceleration/deceleration)
		# --------------------------------------------------
		if(length(hazardModifiers)){
			nullFun <- F
			cat('
    # APPLY HAZARD RATIOS ( ACCELERATION / DECELERATION )
    hazardRatios = {',paste('"',hazardModifiers,'" :',hazardRatios[hazardModifiers],collapse = ', ',sep = ''),'}
    hrKeys = hazardRatios.keys()
    hrEvents = someone.getEvents(lambda e: any(t == e.type or t in e.type for t in hrKeys),
                                ordered =True)
    for e in hrEvents:
        if e.age > event.age:
            break
        if isinstance(e.type,(list,tuple)):
            key = [t for t in e.type if t in hrKeys][0] #exixtance is assured by the query 
        else:
            key = e.type
        if additive:
            event.accelerate(relRisk=hazardRatios[key],ageCutPoint=e.age)
        else:
            changePoints = event._changePoints
            changePoints = [p for p in changePoints if p[1] < e.age]
            RR = hazardRatios[key]
            for p in changePoints:
                RR = RR / p[0]
            event.accelerate(relRisk=RR,ageCutPoint=e.age)
')
	   	}

		# --------------------------------------------------
		# write the code that handles the exclusion/prevention events
		# --------------------------------------------------
		if(length(exclusiveEvents)){
			nullFun <- F
			cat('
    if event.age:
        # APPLY PREVENTION
        types = (',paste('"',exclusiveEvents,'"',collapse = ', ',sep = ''),')
        priorEvents = someone.getEvents(lambda e: any(t == e.type or t in e.type for t in types) 
                                             and e.age < event.age,
                                    ordered =True)
        if len(priorEvents):
            event.agePrevented = priorEvents[0].age

')
		}
	}
	cat('
#these functions will be bound as methods to the events by the same name
eventProcessors = {',
	paste('\n            "',eventNames,'":',eventNames,'_processor,',sep = '',collapse = '')
	,'
        }
eventPreProcessors = {}
eventPostProcessors = {}

def decisionRule(m,screenEvent,someone):
    pass

def eligibilityRule(someone):
    return True

')
}


	


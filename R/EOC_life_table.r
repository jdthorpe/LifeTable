#' A life table for Devloping Ovarian Cancer 
#'
#' A life table for Devloping Ovarian Cancer in Women of all races generated using SEER data collected 2003-2005 using the DevCan software 
#'
#' @docType data
#' @keywords datasets
#' @format A data frame with 20 rows and 7 variables:
#' \describe{
#'   \item{AgeIinterval}{Age Interval}
#'   \item{AliveWithoutCancerAtBegining}{Total Alive And Cancer Free At Beginning Of Interval}
#'   \item{DevelopCancer}{Number Who Develop Cancer This Interval}
#'   \item{DeathsWithoutCancer}{Number Who Die From Other Causes This Interval Among cancer-free population at the beginning of the interval who did not develop cancer during the interval. }
#'   \item{CumProbDevelopCancer}{Cumulative Prob. Of Developing Cancer From Birth}
#'   \item{CumProbDeathWithoutCancer}{Cumulative Prob. Of Dying Cancer Free From Birth}
#'   \item{CumProbCancerOrDeathOtherCauses}{Cumulative Prob. Of Developing Cancer Or Dying Cancer Free From Birth}
#' }
#' 
#' @source {
#' 
#' Software Citation: 
#' 
#' DevCan: Probability of Developing or Dying of Cancer Software, Version 6.7.2. Statistical Research and Applications Branch, National Cancer Institute, 2014. \url{http://surveillance.cancer.gov/devcan}
#' 
#' Methods Citations: 
#' 
#' Fay, M.P., Pfeiffer, R., Cronin, K.A., Le, C. and Feuer, E.J. (2003)
#' "Age-conditional probabilities of developing cancer" Statistics in Medicine,
#' 22(11): 1837-1848.
#' 
#' Fay M.P. "Estimating age conditional probability of developing disease from
#' surveillance data".  Population Health Metrics 2004, 2:6.
#' \url{http://www.pophealthmetrics.com/content/2/1/6}
#' 
#' Database Citation: 
#' 
#' Surveillance, Epidemiology, and End Results (SEER) Program
#' (www.seer.cancer.gov) DevCan database: "SEER 18 Incidence and Mortality,
#' 2000-2011, with Kaposi Sarcoma and Mesothelioma". National Cancer Institute,
#' DCCPS, Surveillance Research Program, Cancer Statistics Branch, released
#' September 2014, based on the November 2013 submission. Underlying mortality
#' data provided by NCHS (www.cdc.gov/nchs).
#' }
"EOC_develop"

# EOC_develop <- read.csv("H:\\O\\popsim\\PopSim\\dist\\LifeTable\\data\\EOC_develop.csv")
# devtools::use_data(EOC_develop,pkg="H:/O/popsim/PopSim/dist/LifeTable")


#' A life table for Dying from Ovarian Cancer Vs. other causes
#'
#' A life table for Dying from Ovarian Cancer Vs. other causes in Women of all races generated using SEER data collected 2003-2005 using the DevCan software 
#'
#' @docType data
#' @keywords datasets
#' @format A data frame with 20 rows and 7 variables:
#' \describe{
#'   \item{AgeIinterval}{Age Interval}
#'   \item{AliveAtBegining}{Total Alive At Beginning Of Interval}
#'   \item{CancerDeaths}{Number Who Die From Cancer This Interval}
#'   \item{DeathsOtherCauses}{Number Who Die From Other Causes This Interval}
#'   \item{CumProbCancerDeath}{Cumulative Prob. Of Dying From Cancer By End Of Interval}
#'   \item{CumProbDeathOtherCauses}{Cumulative Prob. Of Dying From Other Causes By End Of Interval}
#'   \item{CumProbDeath} {Cumulative Prob. Of Dying By End Of Interval}
#' }
#' 
#' @source {
#' 
#' Software Citation: 
#' 
#' DevCan: Probability of Developing or Dying of Cancer Software, Version 6.7.2. Statistical Research and Applications Branch, National Cancer Institute, 2014. \url{http://surveillance.cancer.gov/devcan}
#' 
#' Methods Citations: 
#' 
#' Fay, M.P., Pfeiffer, R., Cronin, K.A., Le, C. and Feuer, E.J. (2003)
#' "Age-conditional probabilities of developing cancer" Statistics in Medicine,
#' 22(11): 1837-1848.
#' 
#' Fay M.P. "Estimating age conditional probability of developing disease from
#' surveillance data".  Population Health Metrics 2004, 2:6.
#' \url{http://www.pophealthmetrics.com/content/2/1/6}
#' 
#' Database Citation: 
#' 
#' Surveillance, Epidemiology, and End Results (SEER) Program
#' (www.seer.cancer.gov) DevCan database: "SEER 18 Incidence and Mortality,
#' 2000-2011, with Kaposi Sarcoma and Mesothelioma". National Cancer Institute,
#' DCCPS, Surveillance Research Program, Cancer Statistics Branch, released
#' September 2014, based on the November 2013 submission. Underlying mortality
#' data provided by NCHS (www.cdc.gov/nchs).
#' }
"EOC_develop"

#-- EOC_death <- read.csv("H:\\O\\popsim\\PopSim\\dist\\LifeTable\\data\\EOC_death.csv"  )
#-- devtools::use_data(EOC_death,pkg="H:/O/popsim/PopSim/dist/LifeTable")


#' Fractions of EOC subtypes (histologyies) by age
#'
#' Fractions of EOC subtypes (histologyies) by age.
#'
#' @docType data
#' @keywords datasets
#' @format A data frame with 20 rows and 6 variables:
#' \describe{
#'   \item{AgeIinterval}{Age Interval}
#'   \item{ClearCell}{Proportion of EOC diagnoses with ClearCell histology}
#'   \item{Endometrioid}{Proportion of EOC diagnoses with Endometrioid histology}
#'   \item{Misc}{Proportion of EOC diagnoses with Misc histology}
#'   \item{Mucinous}{Proportion of EOC diagnoses with Mucinous histology}
#'   \item{Serous}{Proportion of EOC diagnoses with Serous histology}
#' }
#' 
#' @source {
#' 
#' This is where the reference to SEER goes.
#' 
#' }
"HistologyByAge"


#' United States National Intercensal Estimates (2003) by Age and Sex
#'
#' United States National Intercensal Estimates (2003) by Age and Sex.
#'
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{all}{Intercensal Estimates (as of July 1,2003)}
#'   \item{male}{Intercensal Estimates of Male (as of July 1,2003)}
#'   \item{female}{Intercensal Estimates (as of July 1,2003)}
#' }
#' 
#' @source {
#' 
#' \url{http://www.census.gov/popest/data/intercensal/national/nat2010.html?JS=Y}
#' 
#' }
"US_population_2003"

#' Rates of Gynecologic Surgeries per 100k
#'
#' Rates of Gynecologic Surgeries per 100k (Total Estimated U.S. population, 2003).
#'
#' @format A data frame with 20 rows and 5 variables:
#' \describe{
#'   \item{"ageGroup"}{"ageGroup"}
#'   \item{"Hysterectomy__abdominal_and_vaginal"}{"Hysterectomy  abdominal and vaginal"}
#'   \item{"Ligation_of_fallopian_tubes"}{"Ligation of fallopian tubes"}
#'   \item{"Oophorectomy__unilateral_and_bilateral"}{"Oophorectomy  unilateral and bilateral"}
#'   \item{"Other_excision_of_cervix_and_uterus"}{"Other excision of cervix and uterus"}
#' }
#' 
#' @source {
#' 
#' \url{http://hcupnet.ahrq.gov/HCUPnet.jsp?Parms=H4sIAAAAAAAAAF2KMQqAMBAEv5Q7o9EyhYVNEM0H9iSHkWChlb83tg4Mw8I.uHEhTKtpUiFkhZdTnrRKkjL7iMXHERmbT7kmqxINOzFVbbXFb.vHoVrvJMGy6Vzbd.wMj8QvdKc0gW4AAAA3D1C16F1A5B2F2BC7879E9190450A9E5130FAB33&JS=Y}
#' 
#' }
#' 
#' @note {
#' The unit of analysis for HCUP data is the hospital discharge (i.e., the hospital stay), not a person or patient. more information on Discharges
#' 
#' Rates of discharges are calculated using national population estimates from "Intercensal Estimates of the Resident Population by Single Year of Age, Sex, Race, and Hispanic Origin for the United States: April 1, 2000 to July 1, 2010," Population Division, U.S. Census Bureau, Release date: September 2011. (http://www.census.gov/popest/data/intercensal/national/nat2010.html).
#' }
"GyneclogicalSureryRatesPer100k"




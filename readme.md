
## Overview

The LifeTable package integrates cross sectional event incidence data including:

 - disease diagnoses and competing risks, such as:
 - total deaths or deaths from other causes
 - risk altering behaviors

together with estimates of hazard ratios for risk altering behaviors obtained 
from longitudinal studies analyzed with the Cox PH model in order to generate 
accurate estimates on the efficacy of risk modifying interventions on 
incidence, survival, and (age conditional) life expectancy.

## Getting Started

1. Install the release version of `devtools` from CRAN with `install.packages(c("devtools","rootSolve"))`.

2. To install mfactor from github use: .


    ```R
    devtools::install_github("jdthorpe/LifeTable")
    ```
    note that in windows, you may have to build the github tools using: 

    ```R
    library(devtools)
    build_github_devtools()
    #### Restart R before continuing ####
    ```

    * Alternatively, download the github repository to a zip file and use:

        ```R
        install.packages("LifeTable.zip", repos = NULL)

        # Remove the package after installation
        unlink("LifeTable.zip")
        ```

3. View the [Introduction to the LifeTable package](http://htmlpreview.github.io/?https://github.com/jdthorpe/LifeTable/blob/master/inst/doc/ComposingLifeTables.html)  vignette:

	```R
	library(LifeTable)
	vignette("ComposingLifeTables")
	```


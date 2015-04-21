
## Overview

The LifeTable package integrates cross sectional event incidence data including:

 - disease diagnoses and competing risks, such as:
 - total deaths or deaths from other causes
 - risk altering behaviors
 - estimates of hazard ratios for risk altering behaviors obtained from longitudinal studies analyzed with the Cox PH model

in order to generate accurate estimates on the efficacy of risk modifying interventions
on incidence, survival, and (age conditional) life expectancy.


## Getting Started

1. Install the release version of `devtools` from CRAN with `install.packages("devtools")`.

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

    * Alternatively, downoload the github repository to a zip file and use:

        ```R
        install.packages("LifeTable.zip", repos = NULL)

        # Remove the package after installation
        unlink("mfactor.zip")
        ```

3. View the introduction vignette via:

	```R
	library(mfactor)
	vignette("ComposingLifeTables")
	```

	or browse the HTML version of the vignette (*sans* it's CSS resources) [here](http://htmlpreview.github.io/?https://github.com/jdthorpe/mfactor/blob/master/inst/doc/An-Introduction-to-Mfactors.html).

## Notes


================================================
## Installation Instructions

1. Install the release version of `devtools` from CRAN with `install.packages("devtools")`.

2. Follow the instructions below depending on platform.

    * **Mac and Linux**:

        ```R
        devtools::install_github("jdthorpe/LifeTable")
        ```

    * **Windows**:

        ```R
        library(devtools)
        build_github_devtools()

        #### Restart R before continuing ####
        install.packages("LifeTable.zip", repos = NULL)

        # Remove the package after installation
        unlink("LifeTable.zip")
        ```


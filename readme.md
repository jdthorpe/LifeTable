
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


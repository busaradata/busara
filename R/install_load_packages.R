###################################################################################################
######################### read and load libraries #################################################
###################################################################################################

# A list of both installed and unistalled packages are provided.
# The function loads the already installed packages for use
# Detects the un-installed/missing packages and installs them

#' @param x A vector of all the packages you want to install


#'
#' @examples
#' load_install_packages(c("tidyverse","knitr"))
#'
#' @export

load_install_packages <- function(x){
  for (i in x ) {
    if (suppressWarnings(suppressMessages(!require( i , character.only  =  TRUE )))) {
      install.packages(i , dependencies  =  TRUE )
      suppressWarnings(suppressMessages(require( i , character.only  =  TRUE )))
    }
  }
}

#load_install_packages(c("tidyverse","knitr","ggthemes","kableExtra", "readxl","plyr","readstata13","sjPlot","summarytools","plotly","fcuk","questionr","oddsratio","Hmisc", "lubridate"))




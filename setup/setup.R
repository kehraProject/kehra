################################################################################
######################## WEB APPLICATION #######################################
################################################################################

## Install and load SHINY and dependencies

# install.packages(c('shiny', 'devtools', 'dplyr', 'rgdal'))
# devtools::install_github('rstudio/shinyapps')
# devtools::install_github("rstudio/leaflet")

library(shiny)

## TEST APP
setwd('~/Dropbox/Projects/kehra/kehraApp')                # local (to be tested)
runApp()

## DEPLOY APP
# Configure account on shinyapps.io
shinyapps::setAccountInfo(name='cvitolo',
                          token='651E59F2F2A6132B108F2482C1F7D40E',
                          secret='PFVFk57PCeCfF4aYmkeI6/2DvubvaTtDboclpBzo')
# Deploy
shinyapps::deployApp('~/Dropbox/Projects/kehra/kehraApp')

# terminateApp("kehraApp")

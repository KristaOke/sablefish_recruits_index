#====================================================================
#Load data
#
#Krista, March 2023
#====================================================================
#Notes:
#====================================================================

require(tidyverse)
require(corrplot)


#=============================================================
#### Define Directory Structure ####
wd <- getwd()

dir.data <- file.path(wd,"data")
dir.output <- file.path(wd,"output")
dir.figs <- file.path(wd,"figs")

#Get data plot data =======

birddat <- read.csv(file.path(dir.data, "MDO_age0sablefish_indices.csv"))

birdtemp <-  read.csv(file.path(dir.data, "MDO_foragingarea_meanSST_year_month.csv"))

adfgdat <- read.csv(file.path(dir.data, "sablefishlengths88to22.csv"))


















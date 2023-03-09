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


#plot
bird.list <- birddat %>% gather(key=type, value=value, -year)
head(bird.list)

expore.plot <- ggplot(bird.list, aes(x=year, y=value, fill=type)) +
  geom_point() +
  facet_wrap(~type, scales='free') +
  theme(legend.position = "NA")
expore.plot

ggplot(birdtemp, aes(x=year, y=temp)) +
  geom_point() +
  facet_wrap(~month, scales='free') +
  theme(legend.position = "NA")

#oof adfg dat needs processing












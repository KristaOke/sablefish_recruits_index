#====================================================================
#Load data
#
#Krista, March 2023
#====================================================================
#Notes: Data from:
#Arimitsu, M.L., Hatch, S., 2022, Age-0 Sablefish size and growth indices from 
#seabird diets at Middleton Island, Alaska: U.S. Geological Survey data release, 
#https://doi.org/10.5066/P94KVH9X</othercit>
#Metadata, citation info in xml file in data folder
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


#plots------

#birds----
bird.list <- birddat %>% gather(key=type, value=value, -year)
head(bird.list)

expore.plot <- ggplot(bird.list, aes(x=year, y=value, fill=type)) +
  geom_point() +
  facet_wrap(~type, scales='free') +
  theme(legend.position = "NA")
expore.plot

expore.hist <- ggplot(bird.list, aes(x=value, fill=type)) +
  theme_linedraw() +
  geom_histogram() +
  geom_density(alpha=0.2) +
  facet_wrap(~type, scales='free') +
  theme(legend.position = "NA")
expore.hist
#most pretty normal, CPUE is not so log
birddat$log_CPUE <- log(birddat$CPUE)


zbirddat <- birddat %>% #group_by(Year) %>%
  mutate(propmass_zscor=scale(propmass),
         logCPUE_zscor=scale(log_CPUE),
         FreqOcr_zscor=scale(FO),
         growth_index_zscor=scale(growth_index),
         growth_anom_zscor=scale(growth_anomaly),
         pred_len_zscor=scale(pred_len))

#bird temp----

ggplot(birdtemp, aes(x=year, y=temp)) +
  geom_point() +
  facet_wrap(~month, scales='free') +
  theme(legend.position = "NA")

temp.hist <- ggplot(birdtemp, aes(x=temp)) +
  theme_linedraw() +
  geom_histogram() +
  geom_density(alpha=0.2) +
  facet_wrap(~month, scales='free') +
  theme(legend.position = "NA")
temp.hist
#within months look pretty normal

zbirdtemp <- birdtemp %>% group_by(month) %>%
  mutate(temp_zscor=scale(temp))

zbirdtemp$month[which(zbirdtemp$month==1)]<-"jan_ztemp"
zbirdtemp$month[which(zbirdtemp$month==2)]<-"feb_ztemp"
zbirdtemp$month[which(zbirdtemp$month==3)]<-"mar_ztemp"
zbirdtemp$month[which(zbirdtemp$month==4)]<-"apr_ztemp"
zbirdtemp$month[which(zbirdtemp$month==5)]<-"may_ztemp"
zbirdtemp$month[which(zbirdtemp$month==6)]<-"jun_ztemp"
zbirdtemp$month[which(zbirdtemp$month==7)]<-"jul_ztemp"
zbirdtemp$month[which(zbirdtemp$month==8)]<-"aug_ztemp"
zbirdtemp$month[which(zbirdtemp$month==9)]<-"sep_ztemp"
zbirdtemp$month[which(zbirdtemp$month==10)]<-"oct_ztemp"
zbirdtemp$month[which(zbirdtemp$month==11)]<-"nov_ztemp"
zbirdtemp$month[which(zbirdtemp$month==12)]<-"dec_ztemp"

#rotate wide
zbirdtempwide <-zbirdtemp %>% pivot_wider(names_from = month, values_from = temp_zscor, -temp)

#oof adfg dat needs processing












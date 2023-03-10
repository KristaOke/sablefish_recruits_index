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

library(tidyverse)
library(corrplot)
library(AKesp)
devtools::install_local("C:/Users/Krista.Oke/Analyses/Getting started/AKesp-main.zip")


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



#import from AKesp package------

AKesp::esp_stock_options() #look at stocks and pick one
sbltbl <- AKesp::get_esp_data(stock="Alaska Sablefish")

AKesp::check_data(sbltbl)

#drop socioecon indicators
sbltbl <- sbltbl[which(sbltbl$INDICATOR_TYPE=="Ecosystem"),]

sblwide <- pivot_wider(sbltbl[,c(1:3)], names_from = INDICATOR_NAME, values_from = "DATA_VALUE")
 
colnames(sblwide)

sblbigsub <- sblwide[,c("YEAR",
                    "Annual_Heatwave_GOA_Model",
                    "Annual_Sablefish_Growth_YOY_Middleton_Survey",
                    "Spring_Temperature_Surface_EGOA_Satellite",                      
                    "Spring_Temperature_Surface_GOA_Satellite",                       
                    "Spring_Temperature_Surface_SEBS_Satellite",
                    "Summer_Sablefish_CPUE_Juvenile_GOA_Survey",                      
                    "Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey",
                    "Summer_Temperature_250m_GOA_Survey",
                     "Spring_Chlorophylla_Biomass_EGOA_Satellite" ,                    
                     "Spring_Chlorophylla_Biomass_GOA_Satellite"  ,                    
                     "Spring_Chlorophylla_Biomass_SEBS_Satellite",
                    #"Summer_Euphausiid_Abundance_Kodiak_Survey",
                    "Annual_Copepod_Community_Size_EGOA_Survey"    ,                  
                     "Annual_Copepod_Community_Size_WGOA_Survey")]

sblbiglong <- sblbigsub %>% pivot_longer(values_to = "value", names_to = "indicator", -YEAR)

ggplot(sblbiglong, aes(YEAR, value)) + geom_point() + facet_wrap(~indicator, scales="free")

ggplot(sblbiglong, aes(value)) + geom_histogram() + facet_wrap(~indicator, scales="free")

#let's log "Annual_Heatwave_GOA_Model", "Summer_Sablefish_CPUE_Juvenile_GOA_Survey",                      
#"Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey",

sblbigsub$log_Annual_Heatwave_GOA_Model <- log(sblbigsub$Annual_Heatwave_GOA_Model)
sblbigsub$log_Summer_Sablefish_CPUE_Juvenile_GOA_Survey <- log(sblbigsub$Summer_Sablefish_CPUE_Juvenile_GOA_Survey)
sblbigsub$log_Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey <- log(sblbigsub$Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey)

sblbigsub <- sblbigsub[,c("YEAR",                                                     
                           #"Annual_Heatwave_GOA_Model",                                
                           "Annual_Sablefish_Growth_YOY_Middleton_Survey",             
                           "Spring_Temperature_Surface_EGOA_Satellite",                
                           "Spring_Temperature_Surface_GOA_Satellite",                 
                           "Spring_Temperature_Surface_SEBS_Satellite",                
                          # "Summer_Sablefish_CPUE_Juvenile_GOA_Survey",                
                          # "Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey",    
                           "Summer_Temperature_250m_GOA_Survey",                       
                           "Spring_Chlorophylla_Biomass_EGOA_Satellite",               
                           "Spring_Chlorophylla_Biomass_GOA_Satellite",                
                           "Spring_Chlorophylla_Biomass_SEBS_Satellite",               
                           "Annual_Copepod_Community_Size_EGOA_Survey",                
                           "Annual_Copepod_Community_Size_WGOA_Survey",                
                           "log_Annual_Heatwave_GOA_Model",                            
                           "log_Summer_Sablefish_CPUE_Juvenile_GOA_Survey",            
                           "log_Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey")]



zsbl <- sblbigsub %>% #group_by(Year) %>%
  mutate(Annual_Sablefish_Growth_YOY_Middleton_Survey_zscor=scale(Annual_Sablefish_Growth_YOY_Middleton_Survey),
         Spring_Temperature_Surface_EGOA_Satellite_zscor=scale(Spring_Temperature_Surface_EGOA_Satellite),
         Spring_Temperature_Surface_GOA_Satellite_zscor=scale(Spring_Temperature_Surface_GOA_Satellite),
         Spring_Temperature_Surface_SEBS_Satellite_zscor=scale(Spring_Temperature_Surface_SEBS_Satellite),
         Summer_Temperature_250m_GOA_Survey_zscor=scale(Summer_Temperature_250m_GOA_Survey),
         Spring_Chlorophylla_Biomass_EGOA_Satellite_zscor=scale(Spring_Chlorophylla_Biomass_EGOA_Satellite),
         Spring_Chlorophylla_Biomass_GOA_Satellite_zscor=scale(Spring_Chlorophylla_Biomass_GOA_Satellite),
         Spring_Chlorophylla_Biomass_SEBS_Satellite_zscor=scale(Spring_Chlorophylla_Biomass_SEBS_Satellite),
         Annual_Copepod_Community_Size_EGOA_Survey_zscor=scale(Annual_Copepod_Community_Size_EGOA_Survey),
         Annual_Copepod_Community_Size_WGOA_Survey_zscor=scale(Annual_Copepod_Community_Size_WGOA_Survey),
         log_Annual_Heatwave_GOA_Model_zscor=scale(log_Annual_Heatwave_GOA_Model),
         log_Summer_Sablefish_CPUE_Juvenile_GOA_Survey_zscor=scale(log_Summer_Sablefish_CPUE_Juvenile_GOA_Survey),
         log_Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey_zscor=scale(log_Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey))

zsbllong <- zsbl %>% pivot_longer(values_to = "value", names_to = "indicator", -YEAR)


ggplot(zsbllong, aes(value)) + geom_histogram() + facet_wrap(~indicator, scales="free")

zsbl <- zsbl[,c("YEAR",
  "Annual_Sablefish_Growth_YOY_Middleton_Survey_zscor"  ,           
   "Spring_Temperature_Surface_EGOA_Satellite_zscor" ,               
   "Spring_Temperature_Surface_GOA_Satellite_zscor" ,                
   "Spring_Temperature_Surface_SEBS_Satellite_zscor",                
   "Summer_Temperature_250m_GOA_Survey_zscor",                       
   "Spring_Chlorophylla_Biomass_EGOA_Satellite_zscor",               
   "Spring_Chlorophylla_Biomass_GOA_Satellite_zscor",                
   "Spring_Chlorophylla_Biomass_SEBS_Satellite_zscor",               
   "Annual_Copepod_Community_Size_EGOA_Survey_zscor",                
   "Annual_Copepod_Community_Size_WGOA_Survey_zscor",                
   "log_Annual_Heatwave_GOA_Model_zscor",                            
   "log_Summer_Sablefish_CPUE_Juvenile_GOA_Survey_zscor",            
   "log_Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey_zscor"
)]

#join datasets together------

zbirdsub <- zbirddat[,c("year", "logCPUE_zscor", "pred_len_zscor")]

allbirds <- left_join(zbirdtempwide, zbirdsub)


alldats <- left_join(allbirds, zsbl, by=c("year" = "YEAR"))






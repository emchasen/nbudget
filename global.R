library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(sp)
library(shinythemes)
library(plyr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra)
library(car)
library(grid)
library(openxlsx)
library(RColorBrewer)
library(rgdal) ## terra?
library(plotly)
library(tidyverse)
library(DT)
library(readxl)
library(shinyjs)
library(V8)
library(shinyalert)
library(dashboardthemes)
library(rsconnect)
library(gt)
#rsconnect::deployApp('http://www.kucharik-lab.com/nitrogen-decision-support-toll-beta-testing/')
## Section 2: Read in files ##
#find in InputFiles folder in Box. Make sure they are in your WD
#setwd("~/N_Budget") # Tracy's directory
nitrogenrater_data <- read_csv("data/nitrogenrater.csv")
#print(nitrogenrater_data)
NASS_Corn_data <- read.csv("data/NASS_Corn_5yr.csv", header =TRUE)
NASS_Potato_data <- read.csv("data/NASS_Potato_5yr_v2.csv", header =TRUE)
NASS_Corn <- NASS_Corn_data %>% 
  mutate_if(is.numeric, round)
NASS_Potato <- NASS_Potato_data %>% 
  mutate_if(is.numeric, round)
budget <- read_excel("data/N Budget Calculator3.xlsx")
Potato_N_Rate<-read_excel("data/Potato_N_Rate.xlsx")
#YieldGoal<-as.character(Potato_N_Rate$YieldGoal)
Corn_Loamy<- read_excel("data/Corn_Loamy.xlsx")
Corn_Sandy<- read_excel("data/Corn_Sandy.xlsx")
Corn_Sandy_Irr_Y<-subset(Corn_Sandy,Corn_Sandy$Irrigation == "Yes")
Corn_Sandy_Irr_N<-subset(Corn_Sandy,Corn_Sandy$Irrigation == "No")
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

## Section 3.4 - Static Input & Output variables and conversions ##
Precip_N = 0 #
Inches_Precip = 32
Conversion_factor = .226
N_content_pot = .4
N_content_corn = .73 # grain, unit is Bu
N_content_snap = 7.8 # unit is ton
N_content_oat = 0.61 # unit is Bu, grain

# Conversions
lb_ac = 160
kg_ha = 180
lb_ac_kg_ha=lb_ac*(2.47/2.2)
kg_ha_lb_ac=kg_ha*(2.2/2.47)

# Static input variables
Man = 0
Symbiotic_N_Fixation = 0

### Tracy - okay to delete?
#Irrigation = Irrigation_N*vals$Inches_Applied*.226

Precip = 5
Dry_Deposition = 5
Crop_Seed = 0
Nonsymbiotic_Fixation = 3
# Output
Erosion = 0
Runoff = 0
# Denitrification = 7 - changed to dynamic
Misc = 2
Ammonia_sen = 10
Inorganic_N = 0
Organic_N = 0
Mineralization = 0
Total_Storage_Change = Inorganic_N+Organic_N+Mineralization


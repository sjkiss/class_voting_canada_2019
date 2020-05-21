#This loads the raw data installed in Simon's cesdata package
#These are direct files with no recodes; they've only been renamed to be things like ces65, ces79, etc.
#Uncomment if updates must be installed
#remotes::install_github("sjkiss/cesdata")
library(cesdata)
library(tidyverse)
library(car)
library(labelled)
#Some checks
nrow(ces65)==2118
nrow(ces74)==2562
nrow(ces7980)==2761
nrow(ces84)==3377
nrow(ces88)==3609

#load the here() library
library(here)
###How to use the here()package
here()
here("R_Scripts/2_ces65_recode.R")


### Run the recode scripts
### These take forever; that's why I'm doing this. 
source(here("R_Scripts/2_1_ces65_recode.R"))
source(here("R_Scripts/2_2_ces68_recode.R"))
source(here("R_Scripts/2_3_ces72_nov_recode.R"))
source(here("R_Scripts/2_4_ces74_recode.R"))
source(here("R_Scripts/2_5_ces7980_recode.R"))
#source(here("R_Scripts/2_6_ces80_recode.R"))
source(here("R_Scripts/2_7_ces84_recode.R"))
source(here("R_Scripts/2_8_ces88_recode.R"))
source(here("R_Scripts/2_9_ces93_recode.R"))
source(here("R_Scripts/2_10_ces97_recode.R"))
source(here("R_Scripts/2_11_ces00_recode.R"))
source(here("R_Scripts/2_12_ces0411_recode.R"))
source(here("R_Scripts/2_13_ces15_recode.R"))
source(here("R_Scripts/2_14_ces19_recode.R"))

#This saves the ojbects that we 
save(ces65, ces68, ces72_nov, ces74, ces79, ces84, ces88, ces93, ces97, ces00, ces0411, ces15phone, ces19web, ces19phone, file="Data/recoded_cesdata.Rdata")
#now clear everything out
rm(list=ls())

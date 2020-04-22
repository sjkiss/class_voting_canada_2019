#File to Recode 1965 CES Data 
library(cesdata)
#load data
data("ces65")



library(tidyverse)
library(labelled)
library(car)
Recode(ces88$n3, "1:8=0; 98=0; 9:11=1; 99=NA")

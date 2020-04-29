#This package is a major package for importing data
library(haven)
#This package is useful for generating path names that enables code sharing even with different path directories.
library(here)
#check current working directory
here()
#the 2016 pumf file should be in the data subfolder. WE can get access to the Data subfolder by giving "Data" to the here() command.
here("Data")
#LIst files in the data folder to see that it is there.
list.files(here("Data"))
#read in the file.
census16<-read_sav(file=here("Data", "2016_individual_pumf.sav"))
#load tidyverse library of useful commands
library(tidyverse)
#The glimpse command give us a look at the data
glimpse(census16)
#This library helps search for keywords in variable labels
library(labelled)
#look for income
look_for(census16, "income")
census16$HHInc
#the variable is HHInc, use summary command to check
summary(census16$HHInc)
table(census16$HHInc)
#Check the histogram
census16 %>% 
  ggplot(., aes(x=HHInc))+geom_histogram()
#find cases with negative income
census16 %>% 
  #this filters the data set to rows with income less than zero
  filter(HHInc<0) %>% 
  #then select the variable and print it
  select(HHInc)
#so it looks like there are about 243 cases of people with negative income;
#I guess in theory it is possible for a business owner to have lost income; so let's gamble and keep it. I have not seen any referene in technical eocumentation to negative values
#check summary again,
summary(census16$HHInc)
#The maximum value in the dataset is 200000. 

#find the quintiles
library(gtools)
#This calculates the quintiles
quintiles_2016<-quantile(census16$HHInc, probs=c(0.2,0.4,0.6,0.8), na.rm=T)
#Ths calculates the average by quintile
census16 %>% 
  mutate(quintile=quantcut(HHInc, q=5,  labels=c(seq(1,5,1)))) %>% 
group_by(quintile) %>% 
  summarise(avg=mean(HHInc, na.rm=T), n=n())  %>% 
  mutate(year=rep('2016', nrow(.)))-> quintile_average_2016
quintile_average_2016

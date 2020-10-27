library(cesdata)
library(labelled)
data("ces19phone")
data('ces15phone')
library(tidyverse)
library(stringr)
#convert q7 to mip
ces19phone$mip<-tolower(ces19phone$q7)


#convert mip to mip_cat
ces19phone %>% 
  mutate(mip_cat=case_when(
    str_detect(mip, "climate") ~ "environment",
        str_detect(mip, "environ") ~ "environment",
    str_detect(mip, "health") ~ "health care",
       str_detect(mip, "tax") ~ "taxes",
           str_detect(mip, "impots") ~ "taxes",
               str_detect(mip, "santé") ~ "health care",
                   str_detect(mip, "crim[ei]") ~ "crime",
                       str_detect(mip, "jobs") ~ "jobs",
                           str_detect(mip, "deficit") ~ "debt_deficit",
                               str_detect(mip, "debt") ~ "debt_deficit",
                                #   str_detect(mip, "trudeau") ~ "leadership",
                                   str_detect(mip, "ethics") ~ "ethics",
                                       str_detect(mip, "immigr") ~ "immigration",
                                           str_detect(mip, "pipeline") ~ "pipeline",
                                               str_detect(mip, "ecologie") ~ "environment",
  
    TRUE ~ NA_character_
  )) -> ces19phone

library(labelled)

  ces19phone %>% 
  mutate(mip_enviro=case_when(
    mip_cat=='environment'~ 1,
    TRUE ~ 0
  ))->ces19phone
#
ces19phone$mip
ces19phone %>% 
distinct(mip, .keep_all=T) %>% 
  select(mip, mip_cat)-> mip19

mip19$mip_number<-rep("", nrow(mip19))
library(openxlsx)

val_labels(ces15phone$CPS15_1) %>% 
  data.frame()->mip15
#Create the workbook
mip<-createWorkbook()
#addWorksheet(mip, sheetName="Examples_2015")
addWorksheet(mip, sheetName="To_Be_Coded_2019")
writeData(mip, sheet="To_Be_Coded_2019", mip19)
writeData(mip,startCol = 4, startRow = 1, sheet="To_Be_Coded_2019", mip15)
saveWorkbook(mip, file="Results/mip.xlsx", overwrite=T)



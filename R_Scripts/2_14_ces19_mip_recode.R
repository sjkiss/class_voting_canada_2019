#Load original CES 
library(cesdata)
library(labelled)
library(tidyverse)
look_for(ces19phone, "important")
look_for(ces19web, "important")
look_for(ces19phone, "occupation")
ces19phone$p52

ces19phone$mip19<-tolower(ces19phone$q7)
ces19phone %>% 
  mutate(mip19_cat=case_when(
    str_detect(mip19, "climate") ~ "environment",
        str_detect(mip19, "environ") ~ "environment",
    str_detect(mip19, "health") ~ "health care",
       str_detect(mip19, "tax") ~ "taxes",
           str_detect(mip19, "impots") ~ "taxes",
               str_detect(mip19, "santé") ~ "health care",
                   str_detect(mip19, "crim[ei]") ~ "crime",
                       str_detect(mip19, "jobs") ~ "jobs",
                           str_detect(mip19, "deficit") ~ "debt_deficit",
                               str_detect(mip19, "debt") ~ "debt_deficit",
                                   str_detect(mip19, "trudeau") ~ "leadership",
                                   str_detect(mip19, "ethics") ~ "ethics",
                                       str_detect(mip19, "immigr") ~ "immigration",
                                           str_detect(mip19, "pipeline") ~ "environment",
                                               str_detect(mip19, "ecologie") ~ "environment",
  
    TRUE ~ NA_character_
  )) %>%
  select(mip19, mip19_cat) %>%
  distinct() %>% 
write_excel_csv(., "Results/mip19.csv")

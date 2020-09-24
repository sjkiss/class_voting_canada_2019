

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
                                   str_detect(mip, "trudeau") ~ "leadership",
                                   str_detect(mip, "ethics") ~ "ethics",
                                       str_detect(mip, "immigr") ~ "immigration",
                                           str_detect(mip, "pipeline") ~ "environment",
                                               str_detect(mip, "ecologie") ~ "environment",
  
    TRUE ~ NA_character_
  )) -> ces19phone

ces19phone %>% 
  mutate(mip_enviro=case_when(
    mip_cat=='environment'~ 1,
    TRUE ~ 0
  ))->ces19phone
table(ces19phone$mip_enviro)



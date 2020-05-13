library(here)
here()
here("R_Scripts/2_ces65_recode.R")
source(here("R_Scripts/2_ces65_recode.R"))
source(here("R_Scripts/3_ces68_recode.R"))
source(here("R_Scripts/4_ces72_nov_recode.R"))
source(here("R_Scripts/5_ces74b_recode.R"))
source(here("R_Scripts/6_ces79_recode.R"))
source(here("R_Scripts/7_ces74_recode (1980).R"))
source(here("R_Scripts/8_ces84_recode.R"))
source(here("R_Scripts/9_ces88_recode.R"))
source(here("R_Scripts/10_ces93_recode.R"))
source(here("R_Scripts/11_ces97_recode.R"))
source(here("R_Scripts/12_ces00_recode.R"))
source(here("R_Scripts/13_ces0411_recode.R"))
source(here("R_Scripts/14_ces15_recode.R"))
source(here("R_Scripts/14_ces19_recode.R"))

#####SPLITTING THE 04-11 FILE

### STEP 1

### The next thing to do would be to split the 2004-2011 file into separate files
### I don't think we want to mess around with the panel data
### I made a survey variable when we started
table(ces0411$survey)
names(ces0411)
##This code groups by the survey variable
ces0411 %>% 
  #make gtroups
  group_by(survey) %>% 
  #summarize those gtroups by counting
  summarize(n=n()) %>% 
  #arrange the data frame in descending order
  arrange(desc(n)) %>% 
  #print all the rows
  print(n=69)

#### STEP 2 FILTERING

#Panels not added but the rest have been
###CES04
ces0411 %>% 
  filter(survey=="CPS04 PES04 MBS04" | survey=="CPS04 PES04" | survey=="CPS04 PES04 MBS04 CPS06 PES06" | survey=="CPS04 PES04 CPS06 PES06" | survey=="CPS04 PES04 CPS06" | survey=="CPS04 PES04 MBS04 CPS06" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 CPS11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11" | survey=="CPS04 PES04 CPS06 CPS11 PES11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 CPS06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11 PES11 MBS11 WBS11")->ces04

### CES06
ces0411 %>%  
  filter(survey=="CPS06 PES06" | survey=="CPS04 PES04 MBS04 CPS06 PES06" | survey=="CPS04 PES04 CPS06 PES06" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11 WBS11")->ces06

### CES08
ces0411 %>% 
  filter(survey=="CPS08 PES08 MBS08" | survey=="CPS08 PES08")->ces08

### CES11
ces0411 %>% 
  filter(survey=="New RDD_2011 CPS11 PES11" | survey=="New RDD_2011 CPS11" | survey=="New RDD_2011 CPS11 PES11 MBS11" | survey=="New RDD_2011 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 CPS11 PES11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11 PES11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 CPS06 CPS11 PES11 MBS11 WBS11" | survey=="CPS04 PES04 CPS06 CPS11 PES11 MBS11" | survey=="CPS04 PES04 MBS04 CPS06 CPS11 PES11 MBS11 WBS11")->ces11

### Can you go through and get ces04, ces06, ces08, and ces11.
#To maximize the sample size, I think we want to get respondents who filled out the CPS, MBS, PES and WBS, right
#I am not sure to do with the respondents who filled out both the 04 and 06 surveys. On the one hand there are 400 of them, which is a lot. On the other hand, they are kind of an inadvertent panel 
#Fuck it let's keep them in.
#Here's what will happen:
### Filter them in to each of the ces04 and the ces06. 
###Then, in the ces04 dataframe, there will be a  union04 and a union06 variable, right?

#### STEP 3 RENAMING VARIABLES

### This is how we will rename the variables in each data frame.. removing the years. 

### CES04

ces04 %>% 
  rename(union_both=union_both06)->ces04
ces04 %>% 
  rename(union=union06)->ces04
ces04 %>% 
  rename(degree=degree04)->ces04
ces04 %>% 
  rename(region=region04)->ces04
ces04 %>% 
  rename(quebec=quebec04)->ces04
ces04 %>% 
  rename(age=age04)->ces04
ces04 %>% 
  rename(religion=religion04)->ces04
ces04 %>% 
  rename(language=language04)->ces04
ces04 %>% 
  rename(employment=employment04)->ces04
ces04 %>% 
  rename(sector=sector04)->ces04
ces04 %>% 
  rename(party_id=party_id04)->ces04
ces04 %>% 
  rename(vote=vote04)->ces04
ces04 %>% 
  rename(occupation=occupation04)->ces04
ces04 %>% 
  rename(income=income04)->ces04

### CES06

ces06 %>% 
  rename(union_both=union_both06)->ces06
ces06 %>% 
  rename(union=union06)->ces06
ces06 %>% 
  rename(degree=degree06)->ces06
ces06 %>% 
  rename(region=region06)->ces06
ces06 %>% 
  rename(quebec=quebec06)->ces06
ces06 %>% 
  rename(age=age06)->ces06
ces06 %>% 
  rename(religion=religion06)->ces06
ces06 %>% 
  rename(language=language06)->ces06
ces06 %>% 
  rename(employment=employment06)->ces06
ces06 %>% 
  rename(sector=sector06)->ces06
ces06 %>% 
  rename(vote=vote06)->ces06
ces06 %>% 
  rename(party_id=party_id06)->ces06
ces06 %>% 
  rename(occupation=occupation06)->ces06
ces06 %>% 
  rename(income=income06)->ces06

###CES08

ces08 %>% 
  rename(union_both=union_both06)->ces08
ces08 %>% 
  rename(union=union06)->ces08
ces08 %>% 
  rename(degree=degree08)->ces08
ces08 %>% 
  rename(region=region08)->ces08
ces08 %>% 
  rename(quebec=quebec08)->ces08
ces08 %>% 
  rename(age=age08)->ces08
ces08 %>% 
  rename(religion=religion08)->ces08
ces08 %>% 
  rename(language=language08)->ces08
ces08 %>% 
  rename(employment=employment08)->ces08
ces08 %>% 
  rename(sector=sector08)->ces08
ces08 %>% 
  rename(party_id=party_id08)->ces08
ces08 %>% 
  rename(vote=vote08)->ces08
ces08 %>% 
  rename(occupation=occupation08)->ces08
ces08 %>% 
  rename(income=income08)->ces08

###CES11

ces11 %>% 
  rename(union_both=union_both06)->ces11
ces11 %>% 
  rename(union=union06)->ces11
ces11 %>% 
  rename(degree=degree11)->ces11
ces11 %>% 
  rename(region=region11)->ces11
ces11 %>% 
  rename(quebec=quebec11)->ces11
ces11 %>% 
  rename(age=age11)->ces11
ces11 %>% 
  rename(religion=religion11)->ces11
ces11 %>% 
  rename(language=language11)->ces11
ces11 %>% 
  rename(employment=employment11)->ces11
ces11 %>% 
  rename(sector=sector11)->ces11
ces11 %>% 
  rename(party_id=party_id11)->ces11
ces11 %>% 
  rename(vote=vote11)->ces11
ces11 %>% 
  rename(occupation=occupation11)->ces11
ces11 %>% 
  rename(income=income11)->ces11

-------------------------------------------------------------------------------------------
######REJOINING THE FILES
### Ultimately we're going to join the data frames like this. 

library(tidyverse)
names(ces93)

#For some years there are no variables (e.g. 1965 does not have a union variable)
#This is not actually a big deal.
#The trick is that bind_rows keeps *every* single variable, from the data frames that are bound
#If two data frames share a variable then it combines them and populates the values on the one variable from both data frames
#If one of the data frame has a variable that the other does not then it just fills the rows with missing values
#I *think* that this is the quickest way forward. 

##We are going to make a list of each survey
ces.list<-list(ces65, ces68, ces72_nov, ces74, ces74b, ces79, ces84, ces88, ces93, ces97, ces00, ces04, ces06, ces08, ces11, ces15phone, ces19phone)
#WE are going to name each item in the list
names(ces.list)<-c('1965', '1968', '1972', '1980', '1974', '1979', '1984', '1988', '1993', '1997', '2000', '2004', '2006', '2008', '2011', '2015', '2019')
#removing election files
rm(ces00)
rm(ces04)
rm(ces0411)
rm(ces06)
rm(ces08)
rm(ces11)
rm(ces15phone)
rm(ces65)
rm(ces68)
rm(ces72_nov)
rm(ces74)
rm(ces74b)
rm(ces79)
rm(ces84)
rm(ces88)
rm(ces93)
rm(ces97)
rm(ces19phone)

# 
# str(ces.list)
# str(ces.list$`2019`)
# ces.list%>%
#   map(., ncol)
#bind_rows binds the rows of each element in the list together
#.id="survey"creates a new variable called "survey" and its values are the names of the list items. 


#Start with the list
ces.list %>% 
  #Bind the rows, making a new variable called survey that will be populated with the names of the list items
  bind_rows(., .id="election") ->ces
#Do a summary
summary(ces)
#Check the names
names(ces)

#You see how this has *all* the variables from both 1993 and 1997. 
#So here we just select out names variables that we want. 
# ces %>% 
  # select(c("union", "degree", "survey"))-> ces
###We forgot to include the new variable "election" in what is selected.

ces %>% 
  select(c("male", 
           "union_both",
           "union", 
           "degree", 
           "region", 
           "quebec", 
           "age", 
           "religion",
           "language", 
           "employment", 
           "sector",
           "party_id", 
           "vote", 
           "occupation",
          "income", "election") )-> ces
##

library(stringr)
table(str_detect(names(ces0411), "survey"))
table(str_detect(names(ces00), "survey"))

names(ces)
table(ces$union)

####



###This fits a couple of logistic regression models
#Start with the dataframe we made from the lists
ces %>% 
  group_by(election) %>% 
nest(data=c("union", "degree")) %>% 
  #mutate the data frame making a new variable called mod
  # This new variable will contain teh results of the glm model union on degree
  mutate(mod=map(data, ~glm(.$union~.$degree, family="binomial"))) %>% 
  #mutate the data frame again adding the results from this  model fit 
mutate(results=map(mod, broom::tidy)) %>% 
  #Unnest the new variable results 
  unnest(results)

  
  ### Trouble-shoot the errors
#Start with CES  
  ces %>% 
    # form groups by election year
    group_by(election) %>% 
    # summarize each variable by summing, removing any missing values
    summarize_all(sum, na.rm=T)

  ####So somehow, there are a lot of values scattered throughout where the recodes resulted only in cases of zero; i.e. no one is a union member in 1965. And none of the     ### Trouble-shoot the errors
  #Start with CES  
  ces %>% 
    # form groups by election year
    group_by(election) %>% 
    # summarize each variable by summing, removing any missing values
    summarize_all(funs(sum(is.na(.))))

  #Check the union_both variable
  table(ces$union_both)
table(ces$union_both, ces$election)  

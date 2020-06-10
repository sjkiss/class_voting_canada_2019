##load recoded ces files
#load("Data/recoded_cesdata.Rdata")

library(tidyverse)
library(labelled)
library(here)

### Checks
nrow(ces74)==2562 #TRUE
table(ces68$var323, ces68$var379)
table(ces68$var379, ces68$union_both)
table(ces74$size)
table(ces68$var379)
look_for(ces68, "marital")
##### SPLITTING THE 1979-1980 FILE
table(ces7980$male80)
names(ces7980)
names(ces93)
tail(names(ces0411))

library(labelled)
look_for(ces74, "respondent")
look_for(ces7980, "respondent")
look_for(ces74, "respondent")
look_for(ces7980, "filter")

#Get a summary of V9 sector and V4020
ces7980 %>% 
  select(V9, sector, V4020) %>% 
  summary()
#Gt a summary of ces74$V2
ces74%>% 
  select(V2) %>% 
  summary()
###### This code section creates a ces74 data frame from the ces74-79-80 panel survey
###### It does this because sector was only asked of non-manual respondents in ces74, but it was asked of everybody in ces79
###### Blais took the responses for the 79 question for the ces74 respondents who were reinterviewed in ces79 and made it to be their 74 response. So he went backward. 
###### All our other demographic variables were created from the pure cross-sectional survey, so I didn't want to waste all that work. 
###### When we get close to fully being able to replicate Blais, we can execute this code to create ces74 Until then we keep it off. 
table(ces7980$sector)
ces7980 %>%
  #Select V9, sector and panel
  select(V9, sector, V4020) %>%
  #inner join (return all rows from 7980 that have values in V9 that match in ces74 on V2)
  inner_join(., ces74, by=c("V9"="V2")) ->ces74.out
#how many respondents in ces74
nrow(ces74.out)
table(ces74.out$sector.x)
table(ces74.out$sector.y)
#The technical documentation says that there are 1295 CES74 panel respondents reinterviewed in CES79
## 1298 is close, but not exact
table(ces74.out$V4020)#
#There are 3 people who are not part of the ces74-79 panel that got caught with the same respondent IDS
ces74.out %>%
  #Filter in respondents who have a value of 1 on the 74-79 panel filter
  filter(V4020==1)->ces74.out

names(ces74.out)
table(ces74.out$sector.x)
table(ces74.out$sector.y)
#take ces74.out
ces74.out %>%
  #delete sector.y which is the sector variable from the pure ces74 study
  select(-sector.y) %>%
  #sector sector.x to be sector to match all the other variables
  rename(sector=sector.x)->ces74.out
#rename
ces74<-ces74.out

table(ces74$sector)

ces7980 %>% 
  filter(V4002==1)->ces79
ces7980 %>% 
  filter(V4008==1)->ces80

options(max.print=1500)
names(ces80)
names(ces7980)
### We have all of the demographic variables from the ces79 questions stored in the ces80 data set. 
##Show this
table(ces80$male)
### Show that they are the same for the demogrphics
table(ces80$male, ces80$male80)
table(ces80$region, ces80$region80)
##but they are different for political variables for obvious reasons. Demographics didn't change much but vote changed quite a bit.
table(ces80$vote, ces80$vote80)
##We just need to turn the variables that end with 80 into regularly named variables.
ces80 %>% 
  select(male=male80, region=region80, quebec=quebec80, age=age80, language=language80, party_id=party_id80, vote=vote80, union, union_both, degree, employment, sector, income, occupation, religion, non_charter_language, size)->ces80
names(ces80)
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


#### STEP 3 RENAMING VARIABLES

### This is how we will rename the variables in each data frame.. removing the years. 

### CES04

ces04 %>% 
  rename(union_both=union_both04)->ces04
ces04 %>% 
  rename(union=union04)->ces04
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
ces04 %>% 
  rename(non_charter_language=non_charter_language04)->ces04

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
ces06 %>% 
  rename(non_charter_language=non_charter_language06)->ces06

###CES08

ces08 %>% 
  rename(union_both=union_both08)->ces08
ces08 %>% 
  rename(union=union08)->ces08
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
ces08 %>% 
  rename(non_charter_language=non_charter_language08)->ces08

###CES11

ces11 %>% 
  rename(union_both=union_both11)->ces11
ces11 %>% 
  rename(union=union11)->ces11
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
ces11 %>% 
  rename(non_charter_language=non_charter_language11)->ces11


######REJOINING THE FILES

#For some years there are no variables (e.g. 1965 does not have a union variable)
#This is not actually a big deal.
#The trick is that bind_rows keeps *every* single variable, from the data frames that are bound
#If two data frames share a variable then it combines them and populates the values on the one variable from both data frames
#If one of the data frame has a variable that the other does not then it just fills the rows with missing values
#I *think* that this is the quickest way forward. 

##We are going to make a list of each survey
ces.list<-list(ces65, ces68, ces72_nov, ces74, ces79, ces80, ces84, ces88, ces93, ces97, ces00, ces04, ces06, ces08, ces11, ces15phone, ces19phone)
#WE are going to name each item in the list
names(ces.list)<-c('1965', '1968', '1972','1974', '1979','1980', '1984', '1988', '1993', '1997', '2000', '2004', '2006', '2008', '2011', '2015', '2019')
#removing election files
#Remove these only if you run into memory troubles
# rm(ces00)
# rm(ces04)
# rm(ces0411)
# rm(ces06)
# rm(ces08)
# rm(ces11)
# rm(ces15phone)
# rm(ces65)
# rm(ces68)
# rm(ces72_nov)
# rm(ces74)
# rm(ces74b)
# rm(ces79)
# rm(ces84)
# rm(ces88)
# rm(ces93)
# rm(ces97)
# rm(ces19phone)

# 
# str(ces.list)
# str(ces.list$`2019`)
# ces.list%>%
#   map(., ncol)
#bind_rows binds the rows of each element in the list together
#.id="survey"creates a new variable called "survey" and its values are the names of the list items. 

names(ces.list)
table(ces.list[["1984"]]$union_both)
#Start with the list
ces.list %>% 
  #Bind the rows, making a new variable called survey that will be populated with the names of the list items
  bind_rows(., .id="election") ->ces
#Do a summary
summary(ces)
#Check the names
tail(names(ces))
names(ces68)
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
          "income", 
          "non_charter_language", 
          "election", "size") )-> ces
##

library(stringr)
table(str_detect(names(ces0411), "survey"))
table(str_detect(names(ces00), "survey"))

names(ces)
ces$election
table(ces$union)


#### Currently region is regions of English Canada only
#### quebec is dichotomous Quebec v. non-quebec
#### Create region2 which is one region variable for all of Canada
ces %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces

####Turn region2 into factor with Quebec as reference case
#### This can be changed anytime very easily 
ces$region2<-factor(ces$region2, levels=c("Quebec", "Atlantic", "Ontario", "West"))
levels(ces$region2)
##Create female variable
## Sometimes we may want to report male dichotomous variable, sometimes female. 
ces %>% 
  mutate(female=case_when(
    male==1~0,
    male==0~1
  ))->ces
library(car)
#To model party voting we need to create party vote dummy variables
ces$ndp<-Recode(ces$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
ces$liberal<-Recode(ces$vote, "1=1; 2:5=0; NA=NA")
ces$conservative<-Recode(ces$vote, "0:1=0; 2=1; 3:5=0; NA=NA")

### Value labels often go missing in the creation of the ces data frame
### assign value labels
val_labels(ces$sector)<-c(Private=0, Public=1)
val_labels(ces$vote)<-c(Conservative=2,  Liberal=1, NDP=3)
####

###
#This command calls the file 2_diagnostics.R
source("R_scripts/3_recode_diagnostics.R", echo=T)
source("R_scripts/4_make_models.R", echo=T)



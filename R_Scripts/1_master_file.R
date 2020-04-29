library(here)
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
#####SPLITTING THE 04-11 FILE
### STEP 1
### The next thing to do would be to split the 2004-2011 file into separate files
### I don't think we want to mess around with the panel data
### I made a survey variable when we started
table(ces0411$survey)
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
#This is how we filter out the the specific survey years into separate surveys. 

ces0411 %>% 
  filter(survey=="CPS06 PES06")->ces06
#You can combine with the | operator | survey=="CPS06 PES06 MBS)6
#You can also do it by testing for the occurrence of a string

ces0411 %>% 
  select(contains("union"))
### Can you go through and get ces04, ces06, ces08, and ces11. 
#To maximize the sample size, I think we want to get respondents who filled out the CPS, MBS, PES and WBS, right
#I am not sure to do with the respondents who filled out both the 04 and 06 surveys. On the one hand there are 400 of them, which is a lot. On the other hand, they are kind of an inadvertent panel 
#Fuck it let's keep them in.
#Here's what will happen:
### Filter them in to each of the ces04 and the ces06. 
###Then, in the ces04 dataframe, there will be a  union04 and a union06 variable, right?
#### STEP 3 RENAMING VARIABLES

### This is how we will rename the variables in each data frame.. removing the years. 
ces06 %>% 
  rename(union=union_both06)->ces06

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
ces.list<-list(ces93, ces97)
#WE are going to name each item in the list
names(ces.list)<-c('1993', '1997')
#bind_rows binds the rows of each element in the list together
#.id="survey"creates a new variable called "survey" and its values are the names of the list items. 


#Start with the list
ces.list %>% 
  bind_rows(., .id="survey") ->ces
summary(ces)
#Check the names
names(ces)

#You see how this has *all* the variables from both 1993 and 1997. 
#So here we just select out names variables that we want. 
#Nicely because bind_rows creates missing values in list items that *do not* have shared values, we will end up with a complete set of variables for all data frames. 
ces %>% 
  select(c("union", "degree", "survey"))-> ces
ces
ces.list %>% 
  map(function(x) glm(union~degree, data=x)) %>% 
  map(broom::tidy) %>% 
  bind_rows()
  
ces %>% 
  nest(data=c('union', 'degree')) %>% 
  mutate(mod=map(data, ~glm(.$union~.$degree, family="binomial"))) %>% 
mutate(results=map(mod, broom::tidy)) %>% 
  unnest(results)


#Replicating 'The public-private sector cleavage in North America (Blais et al 1990)'

#Run master file to load up data

#To model party voting we need to create party vote dummy variables
ces$ndp<-car::Recode(ces$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
ces$liberal<-car::Recode(ces$vote, "1=1; 2:5=0; NA=NA")
ces$conservative<-car::Recode(ces$vote, "0:1=0; 2=1; 3:5=0; NA=NA")

#Create other relevant dummy variables (Catholic, no_religion, ndp_id, low income x2, lower occupations)
ces$catholic<-car::Recode(ces$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces$no_religion<-car::Recode(ces$religion, "0=1; 1:3=0; NA=NA")
ces$ndp_id<-car::Recode(ces$party_id, "3=1; 0:2=0; 4:5=0; NA=NA")
ces$low_income<-car::Recode(ces$income, "1=1; 2:5=0; NA=NA")
ces$high_income<-car::Recode(ces$income, "1:4=0; 5=1; NA=NA")
ces$income_12<-car::Recode(ces$income, "1:2=1; 3:5=0; NA=NA")
ces$income_345<-car::Recode(ces$income, "3:5=1; 1:2=0; NA=NA")
ces$occupation_12<-car::Recode(ces$occupation, "1:2=1; 3:5=0; NA=NA")
ces$occupation_345<-car::Recode(ces$occupation, "3:5=1; 1:2=0; NA=NA")
ces$working_class<-car::Recode(ces$occupation, "5=1; 1:4=0; NA=NA")

#new variable checks
table(ces$election, ces$ndp)
table(ces$election, ces$liberal)
table(ces$election, ces$conservative)
table(ces$election, ces$catholic)
table(ces$election, ces$no_religion)
table(ces$election, ces$ndp_id)
table(ces$election, ces$low_income)
table(ces$election, ces$high_income)
table(ces$election, ces$income_12)
table(ces$election, ces$income_345)
table(ces$election, ces$occupation_12)
table(ces$election, ces$occupation_345)

### Creating regional dummy variables 
# Atlantic
library(tidyverse)
names(ces)
#start with data frame and pipe
ces %>% 
  #transforming a new variable so mutate  based on conditions in case_when
  mutate(atlantic=case_when(
    #quebec is the reference case for all and atlantic was 1
    quebec==0 &region== 1 ~1,
    #if quebec==1 then it doesn't matter what the region variable is, atlantic gets the value 0
    quebec==1 ~ 0,
    #all other cases are defined as missing
    TRUE ~ NA_real_
  ))->ces
#Check
table(ces$atlantic)#
#there are 7076 respondents in atlantic canada
table(ces$region, ces$atlantic)  
#this checks out. 7076 respondents have 1 on the atlantic dummy variable and 1 on the region variable
#there
table(ces$quebec, ces$atlantic)
#There are 12782 respondents who have 1 on the quebec variable and they have 0 on the atlantic variable
#One last Check
table(ces$quebec, ces$region)
#all respondents who have 0 on the quebec variable have various values on the other regions
table(ces$region)

# Ontario
ces %>% 
  #transforming a new variable so mutate  based on conditions in case_when
  mutate(ontario=case_when(
    quebec==0 &region== 2 ~1,
    quebec==1 ~ 0,
    TRUE ~ NA_real_
  ))->ces
#Check
table(ces$ontario)

# West
ces %>% 
  #transforming a new variable so mutate  based on conditions in case_when
  mutate(west=case_when(
    quebec==0 &region== 3 ~1,
    quebec==1 ~ 0,
    TRUE ~ NA_real_
  ))->ces
#Check
table(ces$west)


#Info: Missing variable in the following elections:
#Sector 1965, 68 and 72
#Occupation 2000 and 2019

#By election
head(ces)
tail(ces)
library(broom)

library(stargazer)
elections<-c(1965, 1968, 1972, 1974, 1979, 1980, 1984, 1988, 1993, 1997, 2000, 2004, 2006, 2008, 2011, 2015, 2019)
stargazer(models1, column.labels=elections)

#------------------------------------------------------------------------------------------------------------

###Model 1 - Blais replication (all elections with region)

#AS always start witht the data frame
ces %>% 
  #form the groups of interest
  group_by(election) %>% 
  #we need to filter out years where there are missing variables
  #filter(election!=1965 & election!=1968) %>%  
  #nest all the other data columns into "list columns", one for each election (group)
  nest(variables=-election) %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of the column data the function that follows 
  mutate(linear.models1=map(variables, function(x) lm(ndp~region+union_both+age+male+no_religion+catholic, data=x)),
         
         #Then we are using the tidy function applied to the new column mods to tidy up those models
         #and storing everything into an object called models
         
         tidied=map(linear.models1, tidy)
         )->models1

#take a look at models
head(models1)
models1$linear.models1
models1$tidied

#as always start with the data frame and pipe
models1 %>% 
  #unnest takes the tidied column and spreads it out for viewing
  unnest(tidied) %>% 
  #filter only the union_both coefficients
  filter(term=="union_both") %>% 
  #plot
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="M1: Linear Coefficients of voting NDP vote by union_both")

#we can save that plot 
ggsave(here("Plots", "M1_union_both&ndp_coefficients.png"))
##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(models1$linear.models1, column.labels=elections, type="text")
#Can also output models as an html file
stargazer(models1$linear.models1, column.labels="elections", type="html", out=here("Tables", "M1_Blais_replication.html"))

#------------------------------------------------------------------------------------------------------------
###Model 2 - Blais replication (all elections with Quebec)

#AS always start witht the data frame
ces %>% 
  #form the groups of interest
  group_by(election) %>% 
  #we need to filter out years where there are missing variables
  #filter(election!=1965 & election!=1968) %>%  
  #nest all the other data columns into "list columns", one for each election (group)
  nest(variables=-election) %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of the column data the function that follows 
  mutate(linear.models2=map(variables, function(x) lm(ndp~quebec+union_both+age+male+no_religion+catholic, data=x)),
         
         #Then we are using the tidy function applied to the new column mods to tidy up those models
         #and storing everything into an object called models
         
         tidied=map(linear.models2, tidy)
  )->models2

#take a look at models
head(models2)
models2$linear.models2
models2$tidied

#as always start with the data frame and pipe
models2 %>% 
  #unnest takes the tidied column and spreads it out for viewing
  unnest(tidied) %>% 
  #filter only the union_both coefficients
  filter(term=="union_both") %>% 
  #plot
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="M2: Linear Coefficients of voting NDP vote by union_both")

#we can save that plot 
ggsave(here("Plots", "M2_union_both&ndp_coefficients.png"))
##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(models2$linear.models2, type="text")
#Can also output models as an html file
stargazer(models2$linear.models2, type="html", out=here("Tables", "M2_Blais_replication.html"))

#------------------------------------------------------------------------------------------------------------

###Model 3 - Blais replication (all years minus 1965-72 due to missing Sector)

#AS always start witht the data frame
ces %>% 
  #form the groups of interest
  group_by(election) %>% 
  #we need to filter out years where there are missing variables
  filter(election!=1965 & election!=1968 & election!=1972) %>%  
  #nest all the other data columns into "list columns", one for each election (group)
  nest(variables=-election) %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of the column data the function that follows 
  mutate(linear.models3=map(variables, function(x) lm(ndp~region+union_both+age+male+no_religion+catholic+sector, data=x)),
         
         #Then we are using the tidy function applied to the new column mods to tidy up those models
         #and storing everything into an object called models
         
         tidied=map(linear.models3, tidy)
  )->models3

#take a look at models
head(models3)
models3$linear.models3
models3$tidied

#as always start with the data frame and pipe
models3 %>% 
  #unnest takes the tidied column and spreads it out for viewing
  unnest(tidied) %>% 
  #filter only the union_both coefficients
  filter(term=="union_both") %>% 
  #plot
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="M3: Linear Coefficients of voting NDP vote by union_both")

#we can save that plot 
ggsave(here("Plots", "M3_union_both&ndp_coefficients.png"))
##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(models3$linear.models3, type="text")
#Can also output models as an html file
stargazer(models3$linear.models3, type="html", out=here("Tables", "M3_Blais_replication.html"))

#------------------------------------------------------------------------------------------------------------

###Model 4 - Blais replication (all years minus 2000 and 2019 due to missing occupation)

#AS always start witht the data frame
ces %>% 
  #form the groups of interest
  group_by(election) %>% 
  #we need to filter out years where there are missing variables
  filter(election!=2000 & election!=2019) %>%  
  #nest all the other data columns into "list columns", one for each election (group)
  nest(variables=-election) %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of the column data the function that follows 
  mutate(linear.models4=map(variables, function(x) lm(ndp~region+union_both+age+male+no_religion+catholic+working_class, data=x)),
         
         #Then we are using the tidy function applied to the new column mods to tidy up those models
         #and storing everything into an object called models
         
         tidied=map(linear.models4, tidy)
  )->models4

#take a look at models
head(models4)
models3$linear.models4
models4$tidied

#as always start with the data frame and pipe
models4 %>% 
  #unnest takes the tidied column and spreads it out for viewing
  unnest(tidied) %>% 
  #filter only the union_both coefficients
  filter(term=="union_both") %>% 
  #plot
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="M4: Linear Coefficients of voting NDP vote by union_both")

#we can save that plot 
ggsave(here("Plots", "M4_union_both&ndp_coefficients.png"))
##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(models4$linear.models4, type="text")
#Can also output models as an html file
stargazer(models4$linear.models4, type="html", out=here("Tables", "M4_Blais_replication.html"))

#------------------------------------------------------------------------------------------------------------

###Model 5 - Region and all variables except sector and occupation

#AS always start witht the data frame
ces %>% 
  #form the groups of interest
  group_by(election) %>% 
  #we need to filter out years where there are missing variables
  #filter(election!=1965 & election!=1968) %>%  
  #nest all the other data columns into "list columns", one for each election (group)
  nest(variables=-election) %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of the column data the function that follows 
  mutate(linear.models5=map(variables, function(x) lm(ndp~region+union_both+age+male+no_religion+catholic+income+degree+employment+language, data=x)),
         
         #Then we are using the tidy function applied to the new column mods to tidy up those models
         #and storing everything into an object called models
         
         tidied=map(linear.models5, tidy)
  )->models5

#take a look at models
head(models5)
models5$linear.models5
models5$tidied

#as always start with the data frame and pipe
models5 %>% 
  #unnest takes the tidied column and spreads it out for viewing
  unnest(tidied) %>% 
  #filter only the union_both coefficients
  filter(term=="union_both") %>% 
  #plot
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="M5: Linear Coefficients of voting NDP vote by union_both")

#we can save that plot 
ggsave(here("Plots", "M5_union_both&ndp_coefficients.png"))
##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(models5$linear.models5, type="text")
#Can also output models as an html file
stargazer(models5$linear.models5, type="html", out=here("Tables", "M5_All_controls.html"))

#------------------------------------------------------------------------------------------------------------

###Model 5 - Quebec and all variables except sector and occupation

#AS always start witht the data frame
ces %>% 
  #form the groups of interest
  group_by(election) %>% 
  #we need to filter out years where there are missing variables
  #filter(election!=2000 & election!=2019) %>%  
  #nest all the other data columns into "list columns", one for each election (group)
  nest(variables=-election) %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of the column data the function that follows 
  mutate(linear.models6=map(variables, function(x) lm(ndp~quebec+union_both+age+male+no_religion+catholic+income+degree+employment+language, data=x)),
         
         #Then we are using the tidy function applied to the new column mods to tidy up those models
         #and storing everything into an object called models
         
         tidied=map(linear.models6, tidy)
  )->models6

#take a look at models
head(models6)
models6$linear.models6
models6$tidied

#as always start with the data frame and pipe
models6 %>% 
  #unnest takes the tidied column and spreads it out for viewing
  unnest(tidied) %>% 
  #filter only the union_both coefficients
  filter(term=="union_both") %>% 
  #plot
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="M6: Linear Coefficients of voting NDP vote by union_both")

#we can save that plot 
ggsave(here("Plots", "M6_union_both&ndp_coefficients.png"))
##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(models6$linear.models6, type="text")
#Can also output models as an html file
stargazer(models6$linear.models6, type="html", out=here("Tables", "M6_All_controls.html"))

#------------------------------------------------------------------------------------------------------------

###Model 7 - Region and all variables except occupation

#AS always start witht the data frame
ces %>% 
  #form the groups of interest
  group_by(election) %>% 
  #we need to filter out years where there are missing variables
  filter(election!=2000 & election!=2019) %>%  
  #nest all the other data columns into "list columns", one for each election (group)
  nest(variables=-election) %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of the column data the function that follows 
  mutate(linear.models7=map(variables, function(x) lm(ndp~region+union_both+age+male+no_religion+catholic+income+degree+employment+language+working_class, data=x)),
         
         #Then we are using the tidy function applied to the new column mods to tidy up those models
         #and storing everything into an object called models
         
         tidied=map(linear.models7, tidy)
  )->models7

#take a look at models
head(models7)
models7$linear.models7
models7$tidied

#as always start with the data frame and pipe
models7 %>% 
  #unnest takes the tidied column and spreads it out for viewing
  unnest(tidied) %>% 
  #filter only the union_both coefficients
  filter(term=="union_both") %>% 
  #plot
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="M7: Linear Coefficients of voting NDP vote by union_both")

#we can save that plot 
ggsave(here("Plots", "M7_union_both&ndp_coefficients.png"))
##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(models7$linear.models7, type="text")
#Can also output models as an html file
stargazer(models7$linear.models7, type="html", out=here("Tables", "M7_All_controls.html"))

#------------------------------------------------------------------------------------------------------------

###Model 7 - Region and all variables except sector

#AS always start witht the data frame
ces %>% 
  #form the groups of interest
  group_by(election) %>% 
  #we need to filter out years where there are missing variables
  filter(election!=1965 & election!=1968 & election!=1972) %>%  
  #nest all the other data columns into "list columns", one for each election (group)
  nest(variables=-election) %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of the column data the function that follows 
  mutate(linear.models8=map(variables, function(x) lm(ndp~region+union_both+age+male+no_religion+catholic+income+degree+employment+language+sector, data=x)),
         
         #Then we are using the tidy function applied to the new column mods to tidy up those models
         #and storing everything into an object called models
         
         tidied=map(linear.models8, tidy)
  )->models8

#take a look at models
head(models8)
models8$linear.models8
models8$tidied

#as always start with the data frame and pipe
models8 %>% 
  #unnest takes the tidied column and spreads it out for viewing
  unnest(tidied) %>% 
  #filter only the union_both coefficients
  filter(term=="union_both") %>% 
  #plot
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="M8: Linear Coefficients of voting NDP vote by union_both")

#we can save that plot 
ggsave(here("Plots", "M8_union_both&ndp_coefficients.png"))
##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(models8$linear.models8, type="text")
#Can also output models as an html file
stargazer(models8$linear.models8, type="html", out=here("Tables", "M8_All_controls.html"))

#------------------------------------------------------------------------------------------------------------

### Interaction Model 1 - union x sector (no controls)
ces %>% 
  group_by(election) %>% 
  #There are no sector variables for 1965-1972, so we have to delete those
  filter(election!=1965 & election!=1968 & election!=1972) %>%  
  nest() %>% 
  mutate(mods=map(data, function(x) lm(ndp~union_both+sector+union_both:sector, data=x)), 
         tidied=map(mods, tidy)) -> interaction_models1

interaction_models1
table(ces$election, ces$sector)

#take a look at models
head(interaction_models1)
interaction_models1$tidied

#We can look at specfic models in the list columns using square brackets
interaction_models1$tidied[1]

#We could also filter out the significant ones
interaction_models1 %>% 
  unnest(tidied) %>% 
  filter(p.value<0.05)
##
interaction_models1 %>% 
  unnest(tidied) %>% 
  filter(term=="union_both:sector") %>% 
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="IM1: Interaction Coefficients for sector and union_both")

#we can save that plot 
ggsave(here("Plots", "IM1_union_both&sector_ndp_coefficients.png"))

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(interaction_models1$mods, type="text")
#Can also output models as an html file
stargazer(interaction_models1$mods, type="html", out=here("Tables", "IM1_Blais_replication.html"))

#------------------------------------------------------------------------------------------------------------

### Interaction Model 2 - union x sector (all controls minus occupation)
ces %>% 
  group_by(election) %>% 
  #There are no sector variables for 1965-1972, so we have to delete those + religion for 1980
  filter(election!=1965 & election!=1968 & election!=1972) %>%  
  nest() %>% 
  mutate(mods=map(data, function(x) lm(ndp~union_both:sector+region+union_both+age+male+no_religion+catholic+sector+income+degree+employment+language, data=x)), 
         tidied=map(mods, tidy)) -> interaction_models2

interaction_models2
table(ces$election, ces$sector)

#take a look at models
head(interaction_models2)
interaction_models2$tidied

#We can look at specfic models in the list columns using square brackets
interaction_models2$tidied[1]

#We could also filter out the significant ones
interaction_models2 %>% 
  unnest(tidied) %>% 
  filter(p.value<0.05)
##
interaction_models2 %>% 
  unnest(tidied) %>% 
  filter(term=="union_both:sector") %>% 
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="IM2: Interaction Coefficients for sector and union")

#we can save that plot 
ggsave(here("Plots", "IM2_union_both&sector_ndp_coefficients.png"))

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(interaction_models2$mods, type="text")
#Can also output models as an html file
stargazer(interaction_models2$mods, type="html", out=here("Tables", "IM2_Blais_replication.html"))

#------------------------------------------------------------------------------------------------------------

### Interaction Model 3 - degree x income (no controls)
ces %>% 
  group_by(election) %>% 
  #There are no sector variables for 1965-1972, so we have to delete those
  #filter(election!=1965 & election!=1968 & election!=1972) %>%  
  nest() %>% 
  mutate(mods=map(data, function(x) lm(ndp~degree+income+degree:income, data=x)), 
         tidied=map(mods, tidy)) -> interaction_models3

interaction_models3
table(ces$election, ces$sector)

#take a look at models
head(interaction_models3)
interaction_models3$tidied

#We can look at specfic models in the list columns using square brackets
interaction_models3$tidied[1]

#We could also filter out the significant ones
interaction_models3 %>% 
  unnest(tidied) %>% 
  filter(p.value<0.05)
##
interaction_models3 %>% 
  unnest(tidied) %>% 
  filter(term=="degree:income") %>% 
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="IM3: Interaction Coefficients for degree and income")

#we can save that plot 
ggsave(here("Plots", "IM3_degree&income_ndp_coefficients.png"))

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(interaction_models3$mods, type="text")
#Can also output models as an html file
stargazer(interaction_models3$mods, type="html", out=here("Tables", "IM3_Blais_replication.html"))

#------------------------------------------------------------------------------------------------------------

### Interaction Model 4 - degree x income (all controls minus occupation)
ces %>% 
  group_by(election) %>% 
  #There are no sector variables for 1965-1972, so we have to delete those
  filter(election!=1965 & election!=1968 & election!=1972) %>%  
  nest() %>% 
  mutate(mods=map(data, function(x) lm(ndp~degree+income+degree:income+region+union_both+age+male+no_religion+catholic+sector+employment+language, data=x)), 
         tidied=map(mods, tidy)) -> interaction_models4

interaction_models4
table(ces$election, ces$sector)

#take a look at models
head(interaction_models4)
interaction_models4$tidied

#We can look at specfic models in the list columns using square brackets
interaction_models4$tidied[1]

#We could also filter out the significant ones
interaction_models4 %>% 
  unnest(tidied) %>% 
  filter(p.value<0.05)
##
interaction_models4 %>% 
  unnest(tidied) %>% 
  filter(term=="degree:income") %>% 
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="IM4: Interaction Coefficients for degree and income")

#we can save that plot 
ggsave(here("Plots", "IM3_degree&income_ndp_coefficients.png"))

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(interaction_models4$mods, type="text")
#Can also output models as an html file
stargazer(interaction_models4$mods, type="html", out=here("Tables", "IM4_Blais_replication.html"))

#------------------------------------------------------------------------------------------------------------

###Subset Model 1 - Poor model (all basic controls maximizing elections)

### How to subset groups (i.e. income)
ces %>% 
  filter(income==1) %>% 
  group_by(election) %>% 
  nest() %>% 
  mutate(mods=map(data, function(x) lm(ndp~region+union_both+age+male+income, data=x)))->poor_models1

ces %>% 
  #filter(election!=1965& election!=1968) %>% 
  group_by(election) %>% 
  nest() %>% 
  #Remember income is stored as a number, need to turn it into a factor on the fly
  mutate(mods=map(data, function(x) lm(ndp~region+union_both+age+male+no_religion+catholic+as.factor(income), data=x)))->poor_models1

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(poor_models1$mods, type="text")
#Can also output models as an html file
stargazer(poor_models1$mods, type="html", out=here("Tables", "poor_models1.html"))

#------------------------------------------------------------------------------------------------------------

###Subset Model 2 - Poor model (all controls minus occupation)

### How to subset groups (i.e. income)
ces %>% 
  filter(income==1 & election!=1965&election!=1968&election!=1972) %>% 
  group_by(election) %>% 
  nest() %>% 
  mutate(mods=map(data, function(x) lm(ndp~degree+region+union_both+age+male+no_religion+catholic+sector+employment+language+income, data=x)))->poor_models2

ces %>% 
  filter(election!=1965& election!=1968&election!=1972) %>% 
  group_by(election) %>% 
  nest() %>% 
  #Remember income is stored as a number, need to turn it into a factor on the fly
  mutate(mods=map(data, function(x) lm(ndp~degree+region+union_both+age+male+no_religion+catholic+sector+employment+language+as.factor(income), data=x)))->poor_models2

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(poor_models2$mods, type="text")
#Can also output models as an html file
stargazer(poor_models2$mods, type="html", out=here("Tables", "poor_models2.html"))

#------------------------------------------------------------------------------------------------------------

###Subset Model 3 - Class model (all basic controls maximizing elections)

### How to subset groups (i.e. occupation)
ces %>% 
  filter(occupation==1 & election!=2000 & election!=2019) %>% 
  group_by(election) %>% 
  nest() %>% 
  mutate(mods=map(data, function(x) lm(ndp~region+union_both+age+male+occupation, data=x)))->class_models1

ces %>% 
  filter(election!=2000& election!=2019) %>% 
  group_by(election) %>% 
  nest() %>% 
  #Remember income is stored as a number, need to turn it into a factor on the fly
  mutate(mods=map(data, function(x) lm(ndp~region+union_both+age+male+no_religion+catholic+as.factor(occupation), data=x)))->class_models1

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(class_models1$mods, type="text")
#Can also output models as an html file
stargazer(class_models1$mods, type="html", out=here("Tables", "class_models1.html"))

#------------------------------------------------------------------------------------------------------------

###Subset Model 4 - Class model (all controls)
### How to subset groups (i.e. occupation)
ces %>% 
  filter(occupation==1 & election!=1965 & election!=1968 & election!=1972 & election!=2000 & election!=2019) %>% 
  group_by(election) %>% 
  nest() %>% 
  mutate(mods=map(data, function(x) lm(ndp~degree+region+union_both+age+male+no_religion+catholic+sector+employment+language++income+occupation, data=x)))->class_models2

ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=2000 & election!=2019) %>% 
  group_by(election) %>% 
  nest() %>% 
  #Remember income is stored as a number, need to turn it into a factor on the fly
  mutate(mods=map(data, function(x) lm(ndp~degree+region+union_both+age+male+no_religion+catholic+sector+employment+language+income+as.factor(occupation), data=x)))->class_models2

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(class_models2$mods, type="text")
#Can also output models as an html file
stargazer(class_models2$mods, type="html", out=here("Tables", "class_models2.html"))

#------------------------------------------------------------------------------------------------------------

###Subset Model 5 - Poor model (quintiles 1+2 - all basic controls maximizing elections)

### How to subset groups (i.e. income)
ces %>% 
  #filter(election!=1965& election!=1968) %>%
  group_by(election) %>% 
  nest() %>% 
  mutate(mods=map(data, function(x) lm(ndp~region+union_both+age+male+no_religion+catholic+income_12, data=x)))->poor_models3


##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(poor_models3$mods, type="text")
#Can also output models as an html file
stargazer(poor_models3$mods, type="html", out=here("Tables", "poor_models3.html"))

#------------------------------------------------------------------------------------------------------------

###Subset Model 6 - Poor model (all controls minus occupation)

### How to subset groups (i.e. income)
ces %>% 
  filter(election!=1965 & election!=1968 & election!=1972 & election!=2000 & election!=2019) %>% 
  group_by(election) %>% 
  nest() %>% 
  #Remember income is stored as a number, need to turn it into a factor on the fly
  mutate(mods=map(data, function(x) lm(ndp~degree+region+union_both+age+male+no_religion+catholic+sector+employment+language+income+occupation_345, data=x)))->class_models3

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(class_models3$mods, type="text")
#Can also output models as an html file
stargazer(class_models3$mods, type="html", out=here("Tables", "class_models3.html"))

#------------------------------------------------------------------------------------------------------------


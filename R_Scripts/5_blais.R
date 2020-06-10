#Replicating 'The public-private sector cleavage in North America (Blais et al 1990)'

#Run master file to load up data
#Instead of typing Recode everytime, we can just load the car library here
library(car)

#To model party voting we need to create party vote dummy variables
ces$ndp<-Recode(ces$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
ces$liberal<-Recode(ces$vote, "1=1; 2:5=0; NA=NA")
ces$conservative<-Recode(ces$vote, "0:1=0; 2=1; 3:5=0; NA=NA")

#Create other relevant dummy variables (Catholic, no_religion, ndp_id, low income x2, lower occupations)
ces$catholic<-Recode(ces$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces$no_religion<-Recode(ces$religion, "0=1; 1:3=0; NA=NA")
ces$ndp_id<-Recode(ces$party_id, "3=1; 0:2=0; 4:5=0; NA=NA")
ces$low_income<-Recode(ces$income, "1=1; 2:5=0; NA=NA")
ces$high_income<-Recode(ces$income, "1:4=0; 5=1; NA=NA")
ces$income_12<-Recode(ces$income, "1:2=1; 3:5=0; NA=NA")
ces$income_345<-Recode(ces$income, "3:5=1; 1:2=0; NA=NA")
ces$occupation_12<-Recode(ces$occupation, "1:2=1; 3:5=0; NA=NA")
ces$occupation_345<-Recode(ces$occupation, "3:5=1; 1:2=0; NA=NA")
#ces$working_class<-Recode(ces$occupation, "5=1; 1:4=0; else=NA")

ces$working_class<-Recode(ces$occupation, "5=1; else=0")

#new variable checks
#check for case counts 
table(ces$election, ces$ndp, useNA = "ifany")
table(ces$election, ces$liberal, useNA = "ifany")
table(ces$election, ces$conservative, useNA = "ifany")
table(ces$election, ces$catholic, useNA = "ifany")
table(ces$election, ces$no_religion, useNA = "ifany")
table(ces$election, ces$ndp_id, useNA = "ifany")
table(ces$election, ces$low_income, useNA = "ifany")
table(ces$election, ces$high_income, useNA = "ifany")
table(ces$election, ces$income_12, useNA = "ifany")
table(ces$election, ces$income_345, useNA = "ifany")
table(ces$election, ces$occupation_12, useNA = "ifany")
table(ces$election, ces$occupation_345, useNA = "ifany")
table(ces$election, ces$working_class, useNA = "ifany")
table(ces$election, ces$size, useNA = "ifany")
table(ces$election, ces$sector, useNA = "ifany")


#Info: Missing variable in the following elections:
#Sector 1965 and 1972
#Occupation 2000 and 2019

#By election
head(ces)
tail(ces)
summary(ces)
#Load broom
library(broom)

#------------------------------------------------------------------------------------------------------------
### Count cases
ces %>% 
  group_by(election) %>% 
  filter(election==1968|
           election==1974|
           election==1979|
           election==1980|
           election==1984) %>% 
select(non_charter_language, working_class, no_religion, sector, catholic, union_both, size) %>% 
summary()
ces %>% 
  group_by(election) %>% 
  summarise_all(function(x) sum(is.na(x))) %>% 
  View()

  filter(election==1984) %>% 
  summary()
###Model 1 - Blais replication (all elections with region)
##NDP
table(ces$sector, ces$election)
ces %>% 
  group_by(election) %>% 
  filter(election==1968|
           election==1974|
           election==1979|
           election==1980|
           election==1984) %>% 
  nest(variables=-election) %>% 
  mutate(mods=map(variables, function(x) lm(ndp~as.factor(region2)+catholic+no_religion+non_charter_language+working_class+union_both+age+female+size+sector, data=x)),
         tidied=map(mods, tidy))->blais_models
library(stargazer)
#stargazer(blais_models$mods, type="text")
stargazer(blais_models$mods, 
          type="html", 
          out=here("Tables", "M1_Blais_replication.html"), 
          column.labels=c("1968", "1974", "1979", "1980", "1984"), 
          star.cutoffs=c(0.05))


# 
# table(ces$election, ces$sector)
# #AS always start witht the data frame
# ces %>% 
#   #form the groups of interest
#   group_by(election) %>% 
#   #we need to filter out years where there are missing variables
#   filter(election!=1965 & election!=1972 & election!=2000 & election!=2019) %>%  
#   #nest all the other data columns into "list columns", one for each election (group)
#   nest(variables=-election) %>% 
#   #mutate adds a new column called models
#   #To create that we are mapping onto each instance of the column data the function that follows 
#   mutate(linear.models1=map(variables, function(x) lm(ndp~region2+catholic+no_religion+working_class+union_both+age+female+sector, data=x)),
#          
#          #Then we are using the tidy function applied to the new column mods to tidy up those models
#          #and storing everything into an object called models
#          
#          tidied=map(linear.models1, tidy)
#          )->models1
# ces$region2
# table(ces$sector)
# #take a look at models
# head(models1)
# models1$linear.models1
# models1$tidied
# 
# #as always start with the data frame and pipe
# models1 %>% 
#   #unnest takes the tidied column and spreads it out for viewing
#   unnest(tidied) %>% 
#   #filter only the union_both coefficients
#   filter(term=="sector") %>% 
#   #plot
#   ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="M1: Linear Coefficients of voting NDP by sector")
# 
# #we can save that plot 
# ggsave(here("Plots", "M1_ndp_by_sector.png"))
# ##Lots of functions to print regression tables
# 
# library(stargazer)
# elections<-c( '1968', '1974', '1979', '1980', '1984', '1988', '1993', '1997', '2004', '2006', '2008', '2011', '2015')
# elections
# ##stargazer works best with the untidied models
# stargazer(models1$linear.models1, column.labels=elections, type="text")
# #Can also output models as an html file
# stargazer(models1$linear.models1, column.labels=elections, type="html", out=here("Tables", "M1_Blais_extension.html"), digits=2)
# 
# #
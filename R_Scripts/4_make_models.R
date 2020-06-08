#### Here's how we can make some basic proportions. 
##Start with the data frame and pipe
ces %>% 
  #Form groups of the variables you're trying to tabulate
  group_by(election, vote, union) %>% 
  #summarize those groups by counting the cases n()
  summarize(n=n()) %>% 
  ##In dplyr: each time you summarize, one level of the grouping variables is "peeled off"
  #In this case, the summarized dataframe is now grouped only by election and by party vote
  #Make a new variable called pct that takes each value of n and divides by the sum(n)
  #the key here is that sum(n) is calculated at the group level (election * vote)
  mutate(pct=n/sum(n)) %>%
  #we are only interested in NDP votes and union percentages
  filter(vote==3& union==1) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percentage of NDP Voters Who Are Union Members")

##Saving plots
#ggsave always saves the plot that was just made in the script.
#Run here() to see where here() is. 
here()
#Combine it with "Plots"
here("Plots")
ggsave(filename=here("Plots", "percent_ndp_union.png"))

#Going the other way
# What share of union membership has voted NDP
ces %>% 
  group_by(election, union, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(union==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Union Members Voting NDP")

## What is the public/private share of NDP voters
ces %>% 
  group_by(election, union, sector,ndp) %>% 
  summarize(n=n()) %>% 
  filter(is.na(sector)==F) %>% 
  filter(is.na(union)==F) %>% 
  filter(is.na(ndp)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(union==1) %>% 
  filter(ndp==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(sector)))+geom_col(position="dodge")+labs(title="Share of Public and PRivate sector union respondents voting NDP")

## Let's check the private / public sector share
ces %>% 
  group_by(election, vote, sector) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(sector==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of NDP Voters in publid sector")

ces %>% 
  group_by(election, sector, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(sector==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Public Sector Employees voting NDP")

####  Degree status
#What is the degree status of the Canadian electorate
ces %>% 
  group_by(election,degree) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
ggplot(., aes(x=election, y=pct, fill=as.factor(degree)))+geom_col()
ggsave(here("PLots", "share_degree_ces.png"))
#Voting Party by degree

#start with data frame and pipe
ces %>% 
  #form groups of interest; we want to examine the share of people voting for parties, by degree status for each election
  group_by(election, degree, vote) %>% 
  #summrize each group calculating a variable n using the function n()
  summarize(n=n()) %>% 
  #mutate the dataframe created above by calculating a new variable called pct which takes the value of each n and dividing it by the sum of n which
  mutate(pct=n/sum(n)) %>%
  #filter(degree==1& vote==3) %>% 
  #filter out non-degree holders and include only Liberal, Conservative and NDP voters
  filter(degree==1 & (vote<4 & vote>0)) %>% 
  #ggplot
  #Note that election has to be turned numeric to support the smoothing
  #x-axis is the election, y is the pct variable created above
  ggplot(.,aes(x=as.numeric(election), y=pct))+
  #add points
  geom_point()+
  #add the smooth, method=regular linear model, y on x
  #se=F so that standard error bands are not included
  geom_smooth(method="lm", se=F)+
  #make a panel for each party, use as_factor to use the party labels
  facet_grid(~as_factor(vote))+
  #Add a title 
  labs(title="Piketty Plot: Share of Degree holders voting for political parties over time")
ggsave(here("Plots", "piketty_party_vote_by_degree.png"))


#Voting NDP by Party ID
ces %>% 
  group_by(election, party_id, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(party_id==3& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Party ID Voting NDP")

#Voting NDP by income groups
ces %>% 
  group_by(election, income, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(income==5& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of High Icome Voting NDP")

ces %>% 
  group_by(election, income, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(income==3& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Middle Income Voting NDP")

ces %>% 
  group_by(election, income, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(income==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Low Income Voting NDP")


##Let's make some models
## Let's just fit a binomial logistic regression of voting NDP by union membership
#To model the NDPO we need to create a dichotomous NDP variable 
# I would do it this way, but I am curious where you found the way that you did it. 
ces$ndp<-car::Recode(ces$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
ndp_model<-glm(ndp~union, data=ces, family="binomial")

summary(ndp_model)
str(ndp_model)

##broom is a package that aids in the reporting of models
## https://cran.r-project.org/web/packages/broom/vignettes/broom.html

library(broom)
#AS always start witht hte data frame
ces %>% 
  #form the groups of interest
  group_by(election) %>% 
  #we need to filter out years where there are missing variables
  filter(election!=1965 & election!=1968) %>%  
  #nest all the other data columns into "list columns", one for each election (group)
  nest() %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of the column data the function that follows 
  mutate(mods=map(data, function(x) glm(ndp~union, data=x, family="binomial")), 
         #Then we are using the tidy function applied to the new column mods to tidy up those models
         #and storing everything into an object called models
         tidied=map(mods, tidy)) -> models
#take a look at models
head(models)
#as always start with the data frame and pipe
models %>% 
  #unnest takes the tidied column and spreads it out for viewing
  unnest(tidied) %>% 
  #filter only the union coefficients
  filter(term=="union") %>% 
  #plot
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by union")

#we can save that plot 
ggsave(here("Plots", "union_ndp_coefficients.png"))
##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(models$mods, type="text")
#Can also output models as an html file
stargazer(models$mods, type="html", out=here("Tables", "union_models.html"))

#How to print the exponentiated
#The tidy function has a lot of useful functions
#Because we have the object models that has the models already stored, we can just run tidy on them
#As always start with the data frame
models %>% 
  #mutate adds a new column
  mutate(odds=map(mods, function(x) tidy(x, exponentiate=T)))->models
head(models)

#compare
models$tidied[1]
models$odds[1]
### How to add an interaction
ces %>% 
  group_by(election) %>% 
  #There are no sector variables for 1972, so we have to delete those
  filter(election!=1965 & election!=1968 & election!=1972) %>%  
  nest() %>% 
  mutate(mods=map(data, function(x) glm(ndp~union+sector+union:sector, data=x, family="binomial")), 
         tidied=map(mods, tidy)) -> interaction_models
interaction_models
table(ces$election, ces$sector)
#take a look at models
head(interaction_models)
#We can look at specfic models in the list columns using square brackets
interaction_models$tidied[1]

#We could also filter out the significant ones
interaction_models %>% 
  unnest(tidied) %>% 
  filter(p.value<0.05)
##
interaction_models %>% 
  unnest(tidied) %>% 
  filter(term=="union:sector") %>% 
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="Interaction Coefficients for sector and union")


### How to subset groups (i.e. income)
ces %>% 
  filter(income==1 & election!=1965&election!=1968) %>% 
  group_by(election) %>% 
  nest() %>% 
  mutate(mods=map(data, function(x) glm(ndp~union+income, data=x, family="binomial")))->poor_models

ces %>% 
  filter(election!=1965& election!=1968) %>% 
  group_by(election) %>% 
  nest() %>% 
  #Remember income is stored as a number, need to turn it into a factor on the fly
  mutate(mods=map(data, function(x) glm(ndp~union+as.factor(income), data=x, family="binomial")))->poor_models

##Lots of functions to print regression tables
library(stargazer)
##stargazer works best with the untidied models
stargazer(models$mods, type="text")
#Can also output models as an html file
stargazer(models$mods, type="html", out=here("Tables", "union_models.html"))
##The other way would be to turn the election variable into a series of dichotommous variables. 

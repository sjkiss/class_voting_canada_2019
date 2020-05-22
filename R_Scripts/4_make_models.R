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

#Going the other way
# What share of union membership has voted NDP
ces %>% 
  group_by(election, union, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(union==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Union Members Voting NDP")

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

#Voting NDP by degree
ces %>% 
  group_by(election, degree, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(degree==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Degree Holders Voting NDP")

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

## Let's just fit a binomial logistic regression of voting NDP by income, union membership and degree

model2<-glm(vote==3~ income, data=ces, family="binomial")
summary(model2)

model3<-glm(vote==3~ union, data=ces, family="binomial")
summary(model3)

model4<-glm(vote==3~ degree, data=ces, family="binomial")
summary(model4)

## Let's just fit a binomial logistic regression of being a union member by degree

model1<-glm(union~ degree, data=ces, family="binomial")
summary(model1)

## There are two ways to check the impact of time.
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

#Going the other way
# What share of union membership has voted NDP
ces %>% 
  group_by(election, union, vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(union==1& vote==3) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percent of Union Members Voting NDP")

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


##Let's make some models

## Let's just fit a binomial logistic regression of being a union member by degree

model1<-glm(union~ degree, data=ces, family="binomial")
summary(model1)

## There are two ways to check the impact of time.

## 

ces %>% 
  group_by(election, degree,union) %>% 
  summarize(n=n())
library(broom)
ces %>% 
  filter(election!=1965 & election!=1968) %>% group_by(election) %>% 
  nest() %>% 
  #mutate adds a new column called models
  #To create that we are mapping onto each instance of data the function that follows 
  mutate(mods=map(data, function(x) glm(union~degree, data=x, family="binomial")), 
         tidied=map(mods, tidy)) -> models

models %>% 
  unnest(tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=election,y=estimate ))+geom_point()+labs(title="Logit Coefficients of being a union member by degree status")


##The other way would be to turn the election variable into a series of dichotommous variables. 

## we can do this with pivot_wider
## I found this crazy code here https://stackoverflow.com/questions/35663580/using-tidyr-spread-function-to-create-columns-with-binary-value

ces %>% 
pivot_wider(names_from="election", 
            values_from="election", 
            values_fill=list(election=0),
            values_fn = list(election = ~+(as.logical(length(.))))) ->ces.wide

#### Here's how we can make some basic proportions. 
ces %>% 
  group_by(election, vote, union) %>% 
  summarize(n=n()) %>% 
  group_by(election, vote) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(vote==3& union==1) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percentage of Union members voting for the NDP by election")

#### Here's how we can make some basic proportions. 
ces %>% 
  select(election, vote, union) %>% 
  group_by(election, union) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>%
  filter(union==1) %>% 
  ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Percentage of Union members voting for the NDP by election")
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

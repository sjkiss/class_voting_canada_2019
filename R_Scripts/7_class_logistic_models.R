#### Class logistic models 1979-2019 ####

#Run master file to load up data

library(stargazer)
library(broom)
library(nnet)

ces$ndp_vs_right<-Recode(ces$vote, "3=1; 2=0; else=NA")
ces$ndp_vs_liberal<-Recode(ces$vote, "3=1; 1=0; else=NA")
table(ces$ndp_vs_right)
table(ces$ndp_vs_liberal)

#CREATE WORKING CLASS DICHOTOMOUS VARIABLE; NOTE HERE ONLY EMPLOYED AND SELF-EMPLOYED PEOPLE ARE SET TO 0 OR 1; ELSE = NA
ces$working_class<-Recode(ces$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
#This collapses the two labour categories into one working class
ces$occupation2<-Recode(as.factor(ces$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))
#This collapses the two labour categories into one working class; maintaining self-employed as a unique distinction
ces$occupation4<-Recode(as.factor(ces$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual', 'Self-Employed'))
ces$working_class3<-Recode(ces$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
ces$working_class4<-Recode(ces$occupation3, "4:5=1; else=0")

#this is the NDP vote variable
ces$ndp<-Recode(ces$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
table(ces$working_class, ces$election)
table(ces$ndp)
table(ces$working_class3, ces$election)
table(ces$working_class4)

#Let's put the Occupation variables in order
ces$occupation2<-fct_relevel(ces$occupation2, "Managers", "Professionals", "Routine_Nonmanual", 'Working_Class')
ces$occupation4<-fct_relevel(ces$occupation4, "Managers", "Self-Employed", "Professionals", "Routine_Nonmanual", 'Working_Class')
table(ces$occupation2, ces$election)
table(ces$occupation4, ces$election)

#Turn region into factor with East as reference case
ces$region3<-Recode(as.factor(ces$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces$region3)
table(ces$region3)

#By election
summary(ces)

#Count missing values
ces %>% 
  group_by(election) %>% 
  summarise_all(function(x) sum(is.na(x))) %>% 
  View()

#------------------------------------------------------------------------------------------------

#M1 NDP vs Right ROC
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp_vs_right~income+degree+sector+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_vs_right_ROC_models_1

#M1 NDP vs Right QC
ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp_vs_right~income+degree+sector+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_vs_right_QC_models_1

#M1 NDP vs Liberal ROC
ces %>% 
  filter(quebec!=1 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp_vs_liberal~income+degree+sector+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_vs_liberal_ROC_models_1

#M1 NDP vs Liberal QC
ces %>% 
  filter(quebec!=0 & election!=1965 & election!=1968 & election!=1972 & election!=1974 & election!=2000) %>%
  nest(variables=-election) %>% 
  mutate(model=map(variables, function(x) glm(ndp_vs_liberal~income+degree+sector+occupation4, data=x, family="binomial")),
         tidied=map(model, tidy), 
         vote=rep('NDP', nrow(.)))->ndp_vs_liberal_QC_models_1

#Print models
stargazer(ndp_vs_right_ROC_models_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_vs_right_ROC_models_1.html"))
stargazer(ndp_vs_right_QC_models_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_vs_right_QC_models_1.html"))
stargazer(ndp_vs_liberal_ROC_models_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_vs_liberal_ROC_models_1.html"))
stargazer(ndp_vs_liberal_QC_models_1$model, column.labels=c("1979", "1980", "1984", "1988", "1993", "1997", "2004", "2006", "2008", "2011", "2015", "2019"), type="html", out=here("Tables", "NDP_vs_liberal_QC_models_1.html"))

#Plot coefficients
ndp_vs_right_ROC_models_1 %>% 
  unnest(tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by degree")+geom_smooth(method="loess", se=F)
ggsave(here("Plots", "ROC_degree_ndp_vs_right_coefficients.png"))

ndp_vs_right_QC_models_1 %>% 
  unnest(tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by degree")+geom_smooth(method="loess", se=F)
ggsave(here("Plots", "QC_degree_ndp_vs_right_coefficients.png"))

ndp_vs_liberal_ROC_models_1 %>% 
  unnest(tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by degree")+geom_smooth(method="loess", se=F)
ggsave(here("Plots", "ROC_degree_ndp_vs_liberal_coefficients.png"))

ndp_vs_liberal_QC_models_1 %>% 
  unnest(tidied) %>% 
  filter(term=="degree") %>% 
  ggplot(., aes(x=as.numeric(election),y=estimate ))+geom_point()+labs(title="Logit Coefficients of voting NDP vote by degree")+geom_smooth(method="loess", se=F)
ggsave(here("Plots", "QC_degree_ndp_vs_liberal_coefficients.png"))



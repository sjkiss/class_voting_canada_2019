#### Various models exploring ces19 and working class interactions 
#Run master file to load up data

library(stargazer)
library(broom)
library(nnet)

#By election
summary(ces)

#Recodes
#CREATE WORKING CLASS DICHOTOMOUS VARIABLE; NOTE HERE ONLY EMPLOYED AND SELF-EMPLOYED PEOPLE ARE SET TO 0 OR 1; ELSE = NA
ces19phone$working_class<-Recode(ces19phone$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
#This collapses the two labour categories into one working class
ces19phone$occupation2<-Recode(as.factor(ces19phone$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))
#This collapses the two labour categories into one working class; maintaining self-employed as a unique distinction
ces19phone$occupation4<-Recode(as.factor(ces19phone$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual', 'Self-Employed'))
#this is the NDP vote variable
ces19phone$ndp<-car::Recode(ces19phone$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
table(ces19phone$working_class)
table(ces19phone$ndp)
#Let's put the working class variables in order
ces19phone$occupation2<-fct_relevel(ces19phone$occupation2, "Managers", "Professionals", "Routine_Nonmanual", 'Working_Class')

ces19phone$occupation4<-fct_relevel(ces19phone$occupation4, "Managers", "Self-Employed", "Professionals", "Routine_Nonmanual", 'Working_Class')

#Turn region into factor with East as reference case
ces19phone$region3<-Recode(as.factor(ces19phone$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces19phone$region3)
table(ces19phone$region3)



#### 2019 Models ####
#Model basic with controls

modelROC<-glm(ndp~region3+working_class+union_both+age+male+sector, data=ces19phone, family="binomial")

ces19phone %>% 
  filter(quebec==1)->ces.out

modelQC<-glm(ndp~working_class+union_both+age+male+sector, data=ces.out, family="binomial")
summary(modelROC)
summary(modelQC)

#M1 with leadership (Jagmeet_Singh)
model1ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+Jagmeet_Singh, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model1QC<-glm(ndp~working_class+union_both+age+male+sector+Jagmeet_Singh, data=ces.out, family="binomial")
summary(model1ROC)
summary(model1QC)

#M2 with redistribution
model2ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+redistribution, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model2QC<-glm(ndp~working_class+union_both+age+male+sector+redistribution, data=ces.out, family="binomial")
summary(model2ROC)
summary(model2QC)

#M3 with environment
model3ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+environment, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model3QC<-glm(ndp~working_class+union_both+age+male+sector+environment, data=ces.out, family="binomial")
summary(model3ROC)
summary(model3QC)

#M4 with immigration
model4ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model4QC<-glm(ndp~working_class+union_both+age+male+sector+immigration, data=ces.out, family="binomial")
summary(model4ROC)
summary(model4QC)

#M17 with minority
model17ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+minorities_help, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model17QC<-glm(ndp~working_class+union_both+age+male+sector+minorities_help, data=ces.out, family="binomial")
summary(model17ROC)
summary(model17QC)

#M18 with immigration2
model18ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration2, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model18QC<-glm(ndp~working_class+union_both+age+male+sector+immigration2, data=ces.out, family="binomial")
summary(model18ROC)
summary(model18QC)

#Combine 5 models into one table
stargazer(modelROC, model1ROC, model2ROC, model3ROC, model4ROC, model17ROC, model18ROC, type="html", out=here("Tables", "ROC_ces19_attitudinal_variables.html"))
stargazer(modelQC, model1QC, model2QC, model3QC, model4QC, model17QC, model18QC, type="html", out=here("Tables", "QC_ces19_attitudinal_variables.html"))

#### Leadership interactions ####
#M5 leadership:working class interaction
model5ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+Jagmeet_Singh+working_class:Jagmeet_Singh, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model5QC<-glm(ndp~working_class+union_both+age+male+sector+Jagmeet_Singh+working_class:Jagmeet_Singh, data=ces.out, family="binomial")
summary(model5ROC)
summary(model5QC)

#M6 leadership:union interaction
model6ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+Jagmeet_Singh+union_both:Jagmeet_Singh, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model6QC<-glm(ndp~working_class+union_both+age+male+sector+Jagmeet_Singh+union_both:Jagmeet_Singh, data=ces.out, family="binomial")
summary(model6ROC)
summary(model6QC)

#### Redistribution interactions ####
#M7 redistribution:working class interaction
model7ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+redistribution+working_class:redistribution, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model7QC<-glm(ndp~working_class+union_both+age+male+sector+redistribution+working_class:redistribution, data=ces.out, family="binomial")
summary(model7ROC)
summary(model7QC)

#M8 redistribution:union interaction
model8ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+redistribution+union_both:redistribution, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model8QC<-glm(ndp~working_class+union_both+age+male+sector+redistribution+union_both:redistribution, data=ces.out, family="binomial")
summary(model8ROC)
summary(model8QC)

#### Environment interactions ####
#M9 environment:working class interaction
model9ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+environment+working_class:environment, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model9QC<-glm(ndp~working_class+union_both+age+male+sector+environment+working_class:environment, data=ces.out, family="binomial")
summary(model9ROC)
summary(model9QC)

#M10 environment:union interaction
model10ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+environment+union_both:environment, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model10QC<-glm(ndp~working_class+union_both+age+male+sector+environment+union_both:environment, data=ces.out, family="binomial")
summary(model10ROC)
summary(model10QC)

#### Immigration interactions ####
#M11 immigration:working class interaction
model11ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration+working_class:immigration, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model11QC<-glm(ndp~working_class+union_both+age+male+sector+immigration+working_class:immigration, data=ces.out, family="binomial")
summary(model11ROC)
summary(model11QC)

#M12 immigration:union interaction
model12ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration+union_both:immigration, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model12QC<-glm(ndp~working_class+union_both+age+male+sector+immigration+union_both:immigration, data=ces.out, family="binomial")
summary(model12ROC)
summary(model12QC)

#M13 minority:working class interaction
model13ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+minorities_help+working_class:minorities_help, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model13QC<-glm(ndp~working_class+union_both+age+male+sector+minorities_help+working_class:minorities_help, data=ces.out, family="binomial")
summary(model13ROC)
summary(model13QC)

#M14 minority:union interaction
model14ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+minorities_help+union_both:minorities_help, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model14QC<-glm(ndp~working_class+union_both+age+male+sector+minorities_help+union_both:minorities_help, data=ces.out, family="binomial")
summary(model14ROC)
summary(model14QC)

#M15 immigration2:working class interaction
model15ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration2+working_class:immigration2, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model15QC<-glm(ndp~working_class+union_both+age+male+sector+immigration2+working_class:immigration2, data=ces.out, family="binomial")
summary(model15ROC)
summary(model15QC)

#M16 immigration2:union interaction
model16ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration2+union_both:immigration2, data=ces19phone, family="binomial")
ces19phone %>% 
  filter(quebec==1)->ces.out
model16QC<-glm(ndp~working_class+union_both+age+male+sector+immigration2+union_both:immigration2, data=ces.out, family="binomial")
summary(model16ROC)
summary(model16QC)

#Combine interaction models into one table
stargazer(modelROC, model5ROC, model7ROC, model9ROC, model11ROC, model13ROC, model15ROC, type="html", out=here("Tables", "ROC_ces19_attitudinal_workingclass_interactions.html"))
stargazer(modelQC, model5QC, model7QC, model9QC, model11QC, model13QC, model15QC, type="html", out=here("Tables", "QC_ces19_attitudinal_workingclass_interactions.html"))
stargazer(modelROC, model6ROC, model8ROC, model10ROC, model12ROC, model14ROC, model16ROC, type="html", out=here("Tables", "ROC_ces19_attitudinal_union_interactions.html"))
stargazer(modelQC, model6QC, model8QC, model10QC, model12QC, model14QC, model16QC, type="html", out=here("Tables", "QC_ces19_attitudinal_union_interactions.html"))

#--------------------------------------------------------------------------------------------------------

#### Summarizing ####
#Redistribution by Class
ces19phone$immigration

ces19phone %>%
  #It's actually maybe useful to keep the mssing values in for a while; it tells us where those marginal to the labour market are. 
#  filter(!is.na(occupation4)) %>%
  select(occupation4, Jagmeet_Singh, immigration, redistribution) %>% 
  group_by(occupation4) %>%
  summarise_at(vars(Jagmeet_Singh, immigration, redistribution), mean, na.rm=T)


ces19phone %>%
  #It's actually maybe useful to keep the mssing values in for a while; it tells us where those marginal to the labour market are. 
#  filter(!is.na(occupation4)) %>%
  select(occupation4, Jagmeet_Singh, immigration, redistribution) %>% 
  group_by(occupation4) %>%
  summarise_at(vars(Jagmeet_Singh, immigration, redistribution), mean, na.rm=T) %>% 
  knitr::kable(., "html") %>% 
  cat(., file=here('Tables', 'class_attitudes_2019.html'))
## This is maybe useful, but ideally, I find it more useful to always graph this stuff. 

ces19phone %>%
  #It's actually maybe useful to keep the mssing values in for a while; it tells us where those marginal to the labour market are. 
#  filter(!is.na(occupation4)) %>%
  select(occupation4, Jagmeet_Singh, immigration, redistribution) %>%
  gather(Variable, Value, -occupation4) %>% 
  group_by(occupation4, Variable) %>% 
  summarize(Average=mean(Value, na.rm=T), n=n(), sd=sd(Value, na.rm=T), se=sqrt(sd)) %>% 
  ggplot(., aes(x=Variable, y=Average, col=occupation4))+geom_point()+geom_jitter()


ces19phone %>%
  filter(!is.na(occupation4)) %>%
  group_by(occupation4) %>%
  summarize(mean_redistribution = mean(redistribution, na.rm=T))
# 
# ces19phone %>%
#   filter(!is.na(redistribution)) %>%
#   group_by(occupation2) %>%
#   summarize(mean_redistribution = mean(redistribution))

#Redistribution by Class and Union
ces19phone %>%
  filter(!is.na(redistribution)) %>%
  group_by(occupation, union_both) %>%
  summarize(mean_redistribution = mean(redistribution))

ces19phone %>%
  filter(!is.na(redistribution)) %>%
  group_by(occupation2, union_both) %>%
  summarize(mean_redistribution = mean(redistribution))

#Immigration by Class
ces19phone %>%
  filter(!is.na(immigration)) %>%
  group_by(occupation) %>%
  summarize(mean_immigration = mean(immigration))

ces19phone %>%
  filter(!is.na(immigration)) %>%
  group_by(occupation2) %>%
  summarize(mean_immigration = mean(immigration))

#Immigration by Class and Union
ces19phone %>%
  filter(!is.na(immigration)) %>%
  group_by(occupation, union_both) %>%
  summarize(mean_immigration = mean(immigration))

ces19phone %>%
  filter(!is.na(immigration)) %>%
  group_by(occupation2, union_both) %>%
  summarize(mean_immigration = mean(immigration))

####Leadership by Class
ces19phone %>%
  filter(!is.na(Jagmeet_Singh)) %>%
  group_by(occupation) %>%
  summarize(mean_leadership = mean(Jagmeet_Singh))

ces19phone %>%
  filter(!is.na(Jagmeet_Singh)) %>%
  group_by(occupation2) %>%
  summarize(mean_leadership = mean(Jagmeet_Singh))

#Leadership by Class and Union
ces19phone %>%
  filter(!is.na(Jagmeet_Singh)) %>%
  group_by(occupation, union_both) %>%
  summarize(mean_leadership = mean(Jagmeet_Singh))

ces19phone %>%
  filter(!is.na(Jagmeet_Singh)) %>%
  group_by(occupation2, union_both) %>%
  summarize(mean_leadership = mean(Jagmeet_Singh))

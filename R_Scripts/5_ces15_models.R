#### Various models exploring ces15 and working class interactions 
#Run master file to load up data

library(stargazer)
library(broom)
library(nnet)

#By election
summary(ces)

#Recodes
#CREATE WORKING CLASS DICHOTOMOUS VARIABLE; NOTE HERE ONLY EMPLOYED AND SELF-EMPLOYED PEOPLE ARE SET TO 0 OR 1; ELSE = NA
ces15phone$working_class<-Recode(ces15phone$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
#This collapses the two labour categories into one working class
ces15phone$occupation2<-Recode(as.factor(ces15phone$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))
#This collapses the two labour categories into one working class; maintaining self-employed as a unique distinction
ces15phone$occupation4<-Recode(as.factor(ces15phone$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual', 'Self-Employed'))
#this is the NDP vote variable
ces15phone$ndp<-car::Recode(ces15phone$vote, "3=1; 0:2=0; 4:5=0; NA=NA")
table(ces15phone$working_class)
table(ces15phone$ndp)
#Let's put the working class variables in order
ces15phone$occupation2<-fct_relevel(ces15phone$occupation2, "Managers", "Professionals", "Routine_Nonmanual", 'Working_Class')

ces15phone$occupation4<-fct_relevel(ces15phone$occupation4, "Managers", "Self-Employed", "Professionals", "Routine_Nonmanual", 'Working_Class')

#Turn region into factor with East as reference case
ces15phone$region3<-Recode(as.factor(ces15phone$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces15phone$region3)
table(ces15phone$region3)


#### 2019 Models ####
#Model basic with controls

modelsROC<-glm(ndp~region3+working_class+union_both+age+male+sector, data=ces15phone, family="binomial")

ces15phone %>% 
  filter(quebec==1)->ces.out

modelsQC<-glm(ndp~working_class+union_both+age+male+sector, data=ces.out, family="binomial")
summary(modelsROC)
summary(modelsQC)

#M1 with leadership (Tom_Mulcair)
models1ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+Tom_Mulcair, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models1QC<-glm(ndp~working_class+union_both+age+male+sector+Tom_Mulcair, data=ces.out, family="binomial")
summary(models1ROC)
summary(models1QC)

#M2 with redistribution
models2ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+redistribution, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models2QC<-glm(ndp~working_class+union_both+age+male+sector+redistribution, data=ces.out, family="binomial")
summary(models2ROC)
summary(models2QC)

#M3 with environment
models3ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+environment, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models3QC<-glm(ndp~working_class+union_both+age+male+sector+environment, data=ces.out, family="binomial")
summary(models3ROC)
summary(models3QC)

#M4 with immigration
models4ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models4QC<-glm(ndp~working_class+union_both+age+male+sector+immigration, data=ces.out, family="binomial")
summary(models4ROC)
summary(models4QC)

#M17 with minorities
models17ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+minorities, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models17QC<-glm(ndp~working_class+union_both+age+male+sector+minorities, data=ces.out, family="binomial")
summary(models17ROC)
summary(models17QC)

#M18 with immigration2
models18ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration2, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models18QC<-glm(ndp~working_class+union_both+age+male+sector+immigration2, data=ces.out, family="binomial")
summary(models18ROC)
summary(models18QC)

#M19 with immigration jobs
models19ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration_jobs, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models19QC<-glm(ndp~working_class+union_both+age+male+sector+immigration_jobs, data=ces.out, family="binomial")
summary(models19ROC)
summary(models19QC)

#M20 with immigration feel
models20ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration_feel, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models20QC<-glm(ndp~working_class+union_both+age+male+sector+immigration_feel, data=ces.out, family="binomial")
summary(models20ROC)
summary(models20QC)

#M21 with immigration rate
models21ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration_rate, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models21QC<-glm(ndp~working_class+union_both+age+male+sector+immigration_rate, data=ces.out, family="binomial")
summary(models21ROC)
summary(models21QC)

#M22 with minorities feel
models22ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+minorities_feel, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models22QC<-glm(ndp~working_class+union_both+age+male+sector+minorities_feel, data=ces.out, family="binomial")
summary(models22ROC)
summary(models22QC)

#M23 with minorities help
models23ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+minorities_help, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models23QC<-glm(ndp~working_class+union_both+age+male+sector+minorities_help, data=ces.out, family="binomial")
summary(models23ROC)
summary(models23QC)

#M24 basic model with degree
models24ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+degree, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models24QC<-glm(ndp~working_class+union_both+age+male+sector+degree, data=ces.out, family="binomial")
summary(models24ROC)
summary(models24QC)

#M25 basic model with income
models25ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+income, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models25QC<-glm(ndp~working_class+union_both+age+male+sector+income, data=ces.out, family="binomial")
summary(models25ROC)
summary(models25QC)

#Combine 5 models into one table
#stargazer(modelsROC, models24ROC, models25ROC, models1ROC, models2ROC, models3ROC, models4ROC, models17ROC, models18ROC, type="html", out=here("Tables", "ROC_ces15_attitudinal_variables.html"))
#stargazer(modelsQC, models24QC, models25QC, models1QC, models2QC, models3QC, models4QC, models17QC, models18QC, type="html", out=here("Tables", "QC_ces15_attitudinal_variables.html"))
stargazer(modelsROC, modelsQC, models24ROC, models24QC, models25ROC, models25QC, models1ROC, models1QC, models2ROC, models2QC, models3ROC, models3QC, models4ROC, models4QC, models17ROC, models17QC, models18ROC, models18QC, type="html", out=here("Tables", "ces15_attitudinal_variables.html"))

#Combine immigration/racial minority into one table 
#stargazer(modelsROC, models4ROC, models17ROC, models18ROC, models19ROC, models20ROC, models21ROC, models22ROC, models23ROC, type="html", out=here("Tables", "ROC_ces15_immigration_variables.html"))
#stargazer(modelsQC, models4QC, models17QC, models18QC, models19QC, models20QC, models21QC, models22QC, models23QC, type="html", out=here("Tables", "QC_ces15_immigration_variables.html"))
stargazer(modelsROC, modelsQC, models4ROC, models4QC, models17ROC, models17QC, models18ROC, models18QC, models19ROC, models19QC, models20ROC, models20QC, models21ROC, models21QC, models22ROC, models22QC, models23ROC, models23QC, type="html", out=here("Tables", "ces15_immigration_variables.html"))

#### Leadership interactions ####
#M5 leadership:working class interaction
models5ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+Tom_Mulcair+working_class:Tom_Mulcair, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models5QC<-glm(ndp~working_class+union_both+age+male+sector+Tom_Mulcair+working_class:Tom_Mulcair, data=ces.out, family="binomial")
summary(models5ROC)
summary(models5QC)

#M6 leadership:union interaction
models6ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+Tom_Mulcair+union_both:Tom_Mulcair, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models6QC<-glm(ndp~working_class+union_both+age+male+sector+Tom_Mulcair+union_both:Tom_Mulcair, data=ces.out, family="binomial")
summary(models6ROC)
summary(models6QC)

#### Redistribution interactions ####
#M7 redistribution:working class interaction
models7ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+redistribution+working_class:redistribution, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models7QC<-glm(ndp~working_class+union_both+age+male+sector+redistribution+working_class:redistribution, data=ces.out, family="binomial")
summary(models7ROC)
summary(models7QC)

#M8 redistribution:union interaction
models8ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+redistribution+union_both:redistribution, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models8QC<-glm(ndp~working_class+union_both+age+male+sector+redistribution+union_both:redistribution, data=ces.out, family="binomial")
summary(models8ROC)
summary(models8QC)

#### Environment interactions ####
#M9 environment:working class interaction
models9ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+environment+working_class:environment, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models9QC<-glm(ndp~working_class+union_both+age+male+sector+environment+working_class:environment, data=ces.out, family="binomial")
summary(models9ROC)
summary(models9QC)

#M10 environment:union interaction
models10ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+environment+union_both:environment, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models10QC<-glm(ndp~working_class+union_both+age+male+sector+environment+union_both:environment, data=ces.out, family="binomial")
summary(models10ROC)
summary(models10QC)

#### Immigration interactions ####
#M11 immigration:working class interaction
models11ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration+working_class:immigration, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models11QC<-glm(ndp~working_class+union_both+age+male+sector+immigration+working_class:immigration, data=ces.out, family="binomial")
summary(models11ROC)
summary(models11QC)

#M12 immigration:union interaction
models12ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration+union_both:immigration, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models12QC<-glm(ndp~working_class+union_both+age+male+sector+immigration+union_both:immigration, data=ces.out, family="binomial")
summary(models12ROC)
summary(models12QC)

#M13 minority:working class interaction
models13ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+minorities_help+working_class:minorities, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models13QC<-glm(ndp~working_class+union_both+age+male+sector+minorities_help+working_class:minorities, data=ces.out, family="binomial")
summary(models13ROC)
summary(models13QC)

#M14 minority:union interaction
models14ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+minorities_help+union_both:minorities, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models14QC<-glm(ndp~working_class+union_both+age+male+sector+minorities_help+union_both:minorities, data=ces.out, family="binomial")
summary(models14ROC)
summary(models14QC)

#M15 immigration2:working class interaction
models15ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration2+working_class:immigration2, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models15QC<-glm(ndp~working_class+union_both+age+male+sector+immigration2+working_class:immigration2, data=ces.out, family="binomial")
summary(models15ROC)
summary(models15QC)

#M16 immigration2:union interaction
models16ROC<-glm(ndp~region3+working_class+union_both+age+male+sector+immigration2+union_both:immigration2, data=ces15phone, family="binomial")
ces15phone %>% 
  filter(quebec==1)->ces.out
models16QC<-glm(ndp~working_class+union_both+age+male+sector+immigration2+union_both:immigration2, data=ces.out, family="binomial")
summary(models16ROC)
summary(models16QC)

#Combine interaction models into one table
#stargazer(modelsROC, models5ROC, models7ROC, models9ROC, models11ROC, models13ROC, models15ROC, type="html", out=here("Tables", "ROC_ces15_attitudinal_workingclass_interactions.html"))
#stargazer(modelsQC, models5QC, models7QC, models9QC, models11QC, models13QC, models15QC, type="html", out=here("Tables", "QC_ces15_attitudinal_workingclass_interactions.html"))
#stargazer(modelsROC, models6ROC, models8ROC, models10ROC, models12ROC, models14ROC, models16ROC, type="html", out=here("Tables", "ROC_ces15_attitudinal_union_interactions.html"))
#stargazer(modelsQC, models6QC, models8QC, models10QC, models12QC, models14QC, models16QC, type="html", out=here("Tables", "QC_ces15_attitudinal_union_interactions.html"))
stargazer(modelsROC, modelsQC, models5ROC, models5QC, models7ROC, models7QC, models9ROC, models9QC, models11ROC, models11QC, models13ROC, models13QC, models15ROC, models15QC, type="html", out=here("Tables", "ces15_working_class_interactions.html"))
stargazer(modelsROC, modelsQC, models6ROC, models6QC, models8ROC, models8QC, models10ROC, models10QC, models12ROC, models12QC, models14ROC, models14QC, models16ROC, models16QC, type="html", out=here("Tables", "ces15_union_interactions.html"))

#--------------------------------------------------------------------------------------------------------

#### Summarizing ####

ces15phone$immigration

#### Attitudes by Class ####

ces15phone %>%
  #It's actually maybe useful to keep the mssing values in for a while; it tells us where those marginal to the labour market are. 
  #  filter(!is.na(occupation4)) %>%
  select(occupation4, Tom_Mulcair, immigration, redistribution, environment, minorities) %>% 
  group_by(occupation4) %>%
  summarise_at(vars(Tom_Mulcair, immigration, redistribution, environment), mean, na.rm=T)

library(knitr)
library(kableExtra)
ces15phone %>%
  #It's actually maybe useful to keep the mssing values in for a while; it tells us where those marginal to the labour market are. 
  #  filter(!is.na(occupation4)) %>%
  select(occupation4, Tom_Mulcair, immigration, redistribution, environment, minorities) %>% 
  group_by(occupation4) %>%
  summarise_at(vars(Tom_Mulcair, immigration, redistribution, environment, minorities), mean, na.rm=T) %>% 
  #Convert this to a data frame for printing
  as.data.frame() %>% 
  #summary=F tells stargazer to print the raw data, not summary statistics, digits=2 tells it to round to 2 digits
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Class attitudes 2015.html"))

## This is maybe useful, but ideally, I find it more useful to always graph this stuff. 
ces15phone %>%
  #It's actually maybe useful to keep the mssing values in for a while; it tells us where those marginal to the labour market are. 
  #  filter(!is.na(occupation4)) %>%
  #Select the variables for graphing
  select(occupation4, Tom_Mulcair, immigration, redistribution, environment, minorities) %>%
  #they are currently in a wide format, reduce it to long format with pivot_longer
  pivot_longer(-occupation4,values_to=c("Score"), names_to=c("Variable")) %>% 
  #Now form groups for each class category and each variable
  group_by(occupation4, Variable) %>% 
  #Now summarize those groups, creating the average score, the count of cases n(), the standard deviation, the standard error
  summarize(Average=mean(Score, na.rm=T), n=n(), sd=sd(Score, na.rm=T), se=sqrt(sd)/n) %>% 
  ggplot(., aes(x=Variable, y=Average, col=occupation4))+geom_jitter()

#### Attitudes by Union status ####

ces15phone %>%
  #  filter(!is.na(union_both)) %>%
  select(union_both, Tom_Mulcair, immigration, redistribution, environment, minorities) %>% 
  group_by(union_both) %>%
  summarise_at(vars(Tom_Mulcair, immigration, redistribution, environment, minorities), mean, na.rm=T)

ces15phone %>%
  #  filter(!is.na(union_both)) %>%
  select(union_both, Tom_Mulcair, immigration, redistribution, environment, minorities) %>% 
  group_by(union_both) %>%
  summarise_at(vars(Tom_Mulcair, immigration, redistribution, environment, minorities), mean, na.rm=T) %>% 
  as.data.frame() %>% 
  stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Union attitudes 2015.html"))

ces15phone %>%
  #  filter(!is.na(union_both)) %>%
  select(union_both, Tom_Mulcair, immigration, redistribution, environment, minorities) %>%
  pivot_longer(-union_both,values_to=c("Score"), names_to=c("Variable")) %>% 
  group_by(union_both, Variable) %>% 
  summarize(Average=mean(Score, na.rm=T), n=n(), sd=sd(Score, na.rm=T), se=sqrt(sd)/n) %>% 
  ggplot(., aes(x=Variable, y=Average, col=union_both))+geom_jitter()

# By Regions
ces15phone %>%
  select(quebec, Tom_Mulcair, immigration, redistribution, environment, minorities) %>% 
  group_by(quebec) %>%
  summarise_at(vars(Tom_Mulcair, immigration, redistribution, environment, minorities), mean, na.rm=T)

ces15phone %>%
  select(region, Tom_Mulcair, immigration, redistribution, environment, minorities) %>% 
  group_by(region) %>%
  summarise_at(vars(Tom_Mulcair, immigration, redistribution, environment, minorities), mean, na.rm=T)

# By Income
ces15phone %>%
  select(income, Tom_Mulcair, immigration, redistribution, environment, minorities) %>% 
  group_by(income) %>%
  summarise_at(vars(Tom_Mulcair, immigration, redistribution, environment, minorities), mean, na.rm=T)

# By Degree
ces15phone %>%
  select(degree, Tom_Mulcair, immigration, redistribution, environment, minorities) %>% 
  group_by(degree) %>%
  summarise_at(vars(Tom_Mulcair, immigration, redistribution, environment, minorities), mean, na.rm=T)

# By Gender
ces15phone %>%
  select(male, Tom_Mulcair, immigration, redistribution, environment, minorities) %>% 
  group_by(male) %>%
  summarise_at(vars(Tom_Mulcair, immigration, redistribution, environment, minorities), mean, na.rm=T)

## This is your code and it's great; I just showed some ways to aggregate it and present more information
ces15phone %>%
  filter(!is.na(occupation4)) %>%
  group_by(occupation4) %>%
  summarize(mean_redistribution = mean(redistribution, na.rm=T))
# 
# ces15phone %>%
#   filter(!is.na(redistribution)) %>%
#   group_by(occupation2) %>%
#   summarize(mean_redistribution = mean(redistribution))

#Redistribution by Class and Union
ces15phone %>%
  filter(!is.na(redistribution)) %>%
  group_by(occupation4, union_both) %>%
  summarize(mean_redistribution = mean(redistribution))

ces15phone %>%
  filter(!is.na(redistribution)) %>%
  group_by(occupation2, union_both) %>%
  summarize(mean_redistribution = mean(redistribution))

#Immigration by Class
ces15phone %>%
  filter(!is.na(immigration)) %>%
  group_by(occupation4) %>%
  summarize(mean_immigration = mean(immigration))

ces15phone %>%
  filter(!is.na(immigration)) %>%
  group_by(occupation2) %>%
  summarize(mean_immigration = mean(immigration))

#Immigration by Class and Union
ces15phone %>%
  filter(!is.na(immigration)) %>%
  group_by(occupation4, union_both) %>%
  summarize(mean_immigration = mean(immigration))

ces15phone %>%
  filter(!is.na(immigration)) %>%
  group_by(occupation2, union_both) %>%
  summarize(mean_immigration = mean(immigration))

####Leadership by Class
ces15phone %>%
  filter(!is.na(Tom_Mulcair)) %>%
  group_by(occupation4) %>%
  summarize(mean_leadership = mean(Tom_Mulcair))

ces15phone %>%
  filter(!is.na(Tom_Mulcair)) %>%
  group_by(occupation2) %>%
  summarize(mean_leadership = mean(Tom_Mulcair))

#Leadership by Class and Union
ces15phone %>%
  filter(!is.na(Tom_Mulcair)) %>%
  group_by(occupation4, union_both) %>%
  summarize(mean_leadership = mean(Tom_Mulcair))

ces15phone %>%
  filter(!is.na(Tom_Mulcair)) %>%
  group_by(occupation2, union_both) %>%
  summarize(mean_leadership = mean(Tom_Mulcair))

#Immigration by Class
ces15phone %>%
  filter(!is.na(minorities)) %>%
  group_by(occupation4) %>%
  summarize(mean_immigration = mean(minorities))

ces15phone %>%
  filter(!is.na(minorities)) %>%
  group_by(occupation2) %>%
  summarize(mean_immigration = mean(minorities))

#Immigration by Class and Union
ces15phone %>%
  filter(!is.na(minorities)) %>%
  group_by(occupation4, union_both) %>%
  summarize(mean_immigration = mean(minorities))

ces15phone %>%
  filter(!is.na(minorities)) %>%
  group_by(occupation2, union_both) %>%
  summarize(mean_minorities = mean(minorities))

####Leadership by Class
ces15phone %>%
  filter(!is.na(Tom_Mulcair)) %>%
  group_by(occupation4) %>%
  summarize(mean_leadership = mean(Tom_Mulcair))

ces15phone %>%
  filter(!is.na(Tom_Mulcair)) %>%
  group_by(occupation2) %>%
  summarize(mean_leadership = mean(Tom_Mulcair))

#Leadership by Class and Union
ces15phone %>%
  filter(!is.na(Tom_Mulcair)) %>%
  group_by(occupation4, union_both) %>%
  summarize(mean_leadership = mean(Tom_Mulcair))

ces15phone %>%
  filter(!is.na(Tom_Mulcair)) %>%
  group_by(occupation2, union_both) %>%
  summarize(mean_leadership = mean(Tom_Mulcair))

# Leadership by Class and Quebec
ces15phone %>%
  filter(!is.na(Tom_Mulcair)) %>%
  group_by(occupation4, quebec) %>%
  summarize(mean_leadership = mean(Tom_Mulcair))

ces15phone %>%
  filter(!is.na(Tom_Mulcair)) %>%
  group_by(occupation2, quebec) %>%
  summarize(mean_leadership = mean(Tom_Mulcair))
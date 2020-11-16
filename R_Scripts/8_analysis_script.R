# Primary Script for Class 2019 Paper
library(stargazer)
library(broom)
library(nnet)
library(purrr)
Recode

#CREATE WORKING CLASS DICHOTOMOUS VARIABLE; NOTE HERE ONLY EMPLOYED AND SELF-EMPLOYED PEOPLE ARE SET TO 0 OR 1; ELSE = NA
ces$working_class<-Recode(ces$occupation, "4:5=1; 3=0; 2=0; 1=0; else=NA")
####WHAT IS THE DIFFERENCE BEETWEEN WORKING_CLASS AND WORKING_CLASS2
ces$working_class2<-Recode(ces$occupation, "4:5=1; else=0")

#This collapses the two labour categories into one working class
ces$occupation2<-Recode(as.factor(ces$occupation), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual'))

#This collapses the two labour categories into one working class; maintaining self-employed as a unique distinction
ces$occupation4<-Recode(as.factor(ces$occupation3), "4:5='Working_Class' ; 3='Routine_Nonmanual' ; 2='Managers' ; 1='Professionals'; 6='Self-Employed'", levels=c('Working_Class', 'Managers', 'Professionals', 'Routine_Nonmanual', 'Self-Employed'))
ces$working_class3<-Recode(ces$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
ces$working_class4<-Recode(ces$occupation3, "4:5=1; else=0")
##checks
table(ces$working_class, ces$election)
table(ces$working_class2, ces$election)
table(ces$working_class3, ces$election)
table(ces$working_class4, ces$election)

#### First Graph historical NDP vote ####
library(ggeffects)
#Get ROC Models to 2019
ces %>% 
  filter(election!=2000& vote!=0) %>% 
  nest(-election,-quebec ) %>% 
  mutate(model=map(data, function(x) multinom(as_factor(vote)~occupation2, data=x)),
         predicted=map(model, ggpredict)) ->class_vote_2019

#Start with where the predicted values are stored
class_vote_2019%>% 
  #unnest_wider is a new command; I just found out about it today;
  #unnest the predicted values objects
  unnest_wider(predicted) %>%
  #Now unnest occupation2
  unnest(occupation2) %>%
  #filter in only probability of voting for left
  filter(response.level!="Green"& response.level!="Other"& x=="Working_Class"& !is.na(quebec)) %>% 
  #PLot as line plot 
  ggplot(., aes(x=election, y=predicted, group=x))+geom_line()+facet_grid(as_factor(quebec)~response.level)+theme(axis.text.x=element_text(angle=90))+labs(x="Election", y="Marginal Effect")

ggsave(here("Plots", "class_voting_2019.png"), width=6, height=3)

#### Some 2015 recodes ####
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
table(ces15phone$occupation4)
ces15phone$working_class2<-Recode(ces15phone$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
table(ces15phone$working_class2)

#Turn region into factor with East as reference case
ces15phone$region3<-Recode(as.factor(ces15phone$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces15phone$region3)
table(ces15phone$region3)

#Other dummies
ces15phone$low_income<-Recode(ces15phone$income, "2:5=0; 1=1")
ces15phone$high_income<-Recode(ces15phone$income, "1:4=0; 5=1")
ces15phone$no_religion<-Recode(ces15phone$religion, "0=1; 1:3=0; NA=NA")
ces15phone$catholic<-Recode(ces15phone$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces15phone$young<-Recode(ces15phone$age, "35:115=0; 18:34=1")
ces15phone$old<-Recode(ces15phone$age, "55:115=1; 18:54=0")
ces15phone$foreign<-Recode(ces15phone$native, "1=0; 0=1")
val_labels(ces15phone$foreign)<-c(Foreign=1, Native=0)
#Dummies coded missing as 0
#ces15phone$low_income<-Recode(ces15phone$income, "else=0; 1=1")
#ces15phone$high_income<-Recode(ces15phone$income, "else=0; 5=1")
#ces15phone$no_religion<-Recode(ces15phone$religion, "0=1; else=0")
#ces15phone$catholic<-Recode(ces15phone$religion, "1=1; else=0")
#ces15phone$young<-Recode(ces15phone$age, "else=0; 18:34=1")
#ces15phone$old<-Recode(ces15phone$age, "55:100=1; else=0")
#ces15phone$foreign<-Recode(ces15phone$native, "else=0; 0=1")

ces15phone$working_class<-Recode(ces15phone$working_class, "1=1; else=0")
ces15phone$working_class2<-Recode(ces15phone$working_class2, "1=1; else=0")
#ces15phone$union_both<-Recode(ces15phone$union_both, "1=1; else=0")
#ces15phone$male<-Recode(ces15phone$male, "1=1; else=0")
#ces15phone$sector<-Recode(ces15phone$sector, "1=1; else=0")
#ces15phone$degree<-Recode(ces15phone$degree, "1=1; else=0")
#ces15phone$language<-Recode(ces15phone$language, "1=1; else=0")

# Party Id
#ces15phone$liberal_id<-Recode(ces15phone$party_id, "1=1; 0=0; 2:4=0; else=NA")
#ces15phone$conservative_id<-Recode(ces15phone$party_id, "2=1; 0:1=0; 3:4=0; else=NA")
#ces15phone$ndp_id<-Recode(ces15phone$party_id, "3=1; 0:2=0; 4=0; else=NA")
#ces15phone$bloc_id<-Recode(ces15phone$party_id, "4=1; 0:3=0; else=NA")
ces15phone$liberal_id<-Recode(ces15phone$party_id, "1=1; else=0")
ces15phone$conservative_id<-Recode(ces15phone$party_id, "2=1; else=0")
ces15phone$ndp_id<-Recode(ces15phone$party_id, "3=1; else=0")
ces15phone$bloc_id<-Recode(ces15phone$party_id, "4=1; else=0")

# Party vote
#ces15phone$liberal<-Recode(ces15phone$vote, "1=1; 0=0; 2:5=0; else=NA")
#ces15phone$conservative<-Recode(ces15phone$vote, "2=1; 0:1=0; 3:5=0; else=NA")
#ces15phone$ndp<-Recode(ces15phone$vote, "3=1; 0:2=0; 4:5=0; else=NA")
#ces15phone$bloc<-Recode(ces15phone$vote, "4=1; 0:3=0; 5=0; else=NA")
#ces15phone$green<-Recode(ces15phone$vote, "5=1; 0:4=0; else=NA")
ces15phone$liberal<-Recode(ces15phone$vote, "1=1; else=0")
ces15phone$conservative<-Recode(ces15phone$vote, "2=1; else=0")
ces15phone$ndp<-Recode(ces15phone$vote, "3=1; else=0")
ces15phone$bloc<-Recode(ces15phone$vote, "4=1; else=0")
ces15phone$green<-Recode(ces15phone$vote, "5=1; else=0")


#### 2019 Recodes ####

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
table(ces19phone$occupation4)
ces19phone$working_class2<-Recode(ces19phone$occupation3, "4:5=1; 3=0; 2=0; 1=0; 6=0; else=NA")
#Turn region into all of Canada (4 regions)
ces19phone %>% 
  mutate(region2=case_when(
    region==1 ~ "Atlantic",
    region==2 ~ "Ontario",
    region==3 ~"West",
    quebec==1 ~ "Quebec"
  ))->ces19phone
table(ces19phone$region2)
ces19phone$region4<-factor(ces19phone$region2, levels=c("Atlantic", "Quebec", "Ontario", "West"))
levels(ces19phone$region4)
table(ces19phone$region4)
ces19phone$region4<-factor(ces19phone$region2, levels=c("Atlantic", "Quebec", "Ontario", "West"))
table(ces19phone$working_class2)

#Turn region into factor with East as reference case
ces19phone$region3<-Recode(as.factor(ces19phone$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces19phone$region3)
table(ces19phone$region3)

#Other dummies
ces19phone$low_income<-Recode(ces19phone$income, "2:5=0; 1=1")
ces19phone$high_income<-Recode(ces19phone$income, "1:4=0; 5=1")
ces19phone$no_religion<-Recode(ces19phone$religion, "0=1; 1:3=0; NA=NA")
ces19phone$catholic<-Recode(ces19phone$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces19phone$young<-Recode(ces19phone$age, "35:100=0; 18:34=1")
ces19phone$old<-Recode(ces19phone$age, "55:100=1; 18:54=0")
ces19phone$foreign<-Recode(ces19phone$native, "1=0; 0=1")
val_labels(ces19phone$foreign)<-c(Foreign=1, Native=0)
#Dummies coded missing as 0
#ces19phone$low_income<-Recode(ces19phone$income, "else=0; 1=1")
#ces19phone$high_income<-Recode(ces19phone$income, "else=0; 5=1")
#ces19phone$no_religion<-Recode(ces19phone$religion, "0=1; else=0")
#ces19phone$catholic<-Recode(ces19phone$religion, "1=1; else=0")
#ces19phone$young<-Recode(ces19phone$age, "else=0; 18:34=1")
#ces19phone$old<-Recode(ces19phone$age, "55:100=1; else=0")
#ces19phone$foreign<-Recode(ces19phone$native, "else=0; 0=1")

ces19phone$working_class<-Recode(ces19phone$working_class, "1=1; else=0")
ces19phone$working_class2<-Recode(ces19phone$working_class2, "1=1; else=0")
ces19phone$working_class3<-Recode(ces19phone$working_class2, "1=1; else=0")
#ces19phone$union_both<-Recode(ces19phone$union_both, "1=1; else=0")
#ces19phone$male<-Recode(ces19phone$male, "1=1; else=0")
#ces19phone$sector<-Recode(ces19phone$sector, "1=1; else=0")
#ces19phone$degree<-Recode(ces19phone$degree, "1=1; else=0")
#ces19phone$language<-Recode(ces19phone$language, "1=1; else=0")

# Party Id
#ces19phone$liberal_id<-Recode(ces19phone$party_id, "1=1; 0=0; 2:4=0; else=NA")
#ces19phone$conservative_id<-Recode(ces19phone$party_id, "2=1; 0:1=0; 3:4=0; else=NA")
#ces19phone$ndp_id<-Recode(ces19phone$party_id, "3=1; 0:2=0; 4=0; else=NA")
#ces19phone$bloc_id<-Recode(ces19phone$party_id, "4=1; 0:3=0; else=NA")
ces19phone$liberal_id<-Recode(ces19phone$party_id, "1=1; else=0")
ces19phone$conservative_id<-Recode(ces19phone$party_id, "2=1; else=0")
ces19phone$ndp_id<-Recode(ces19phone$party_id, "3=1; else=0")
ces19phone$bloc_id<-Recode(ces19phone$party_id, "4=1; else=0")
table(ces19phone$liberal_id)
table(ces19phone$conservative_id)
table(ces19phone$ndp_id)
table(ces19phone$bloc_id)

# Party vote
#ces19phone$liberal<-Recode(ces19phone$vote, "1=1; 0=0; 2:5=0; else=NA")
#ces19phone$conservative<-Recode(ces19phone$vote, "2=1; 0:1=0; 3:5=0; else=NA")
#ces19phone$ndp<-Recode(ces19phone$vote, "3=1; 0:2=0; 4:5=0; else=NA")
#ces19phone$bloc<-Recode(ces19phone$vote, "4=1; 0:3=0; 5=0; else=NA")
#ces19phone$green<-Recode(ces19phone$vote, "5=1; 0:4=0; else=NA")
ces19phone$liberal<-Recode(ces19phone$vote, "1=1; else=0")
ces19phone$conservative<-Recode(ces19phone$vote, "2=1; else=0")
ces19phone$ndp<-Recode(ces19phone$vote, "3=1; else=0")
ces19phone$bloc<-Recode(ces19phone$vote, "4=1; else=0")
ces19phone$green<-Recode(ces19phone$vote, "5=1; else=0")
table(ces19phone$liberal)
table(ces19phone$conservative)
table(ces19phone$ndp)
table(ces19phone$bloc)
table(ces19phone$green)

#### Logistic models ####
#Split QC out into ces19.qc
ces19phone %>% 
  filter(quebec==1)->ces19.qc
ces19phone %>% 
  filter(quebec!=1)->ces19.roc

#Logistic Regression Models for Class Variables
model36ALL<-glm(ndp~working_class3+union_both+income+degree+sector, data=ces19phone, family="binomial")
model36ROC<-glm(ndp~working_class3+union_both+income+degree+sector, data=ces19.roc, family="binomial")
model36QC<-glm(ndp~working_class3+union_both+income+degree+sector, data=ces19.qc, family="binomial")
summary(model36ALL)
summary(model36ROC)
summary(model36QC)


#Logistic Regression MOdels with Demographic Controls
model38ALL<-glm(ndp~working_class3+union_both+income+degree+sector+age+male+as.factor(region4), data=ces19phone, family="binomial")
model38ROC<-glm(ndp~working_class3+union_both+income+degree+sector+age+male+region3, data=ces19.roc, family="binomial")
model38QC<-glm(ndp~working_class3+union_both+income+degree+sector+age+male, data=ces19.qc, family="binomial")
summary(model38ALL)
summary(model38ROC)
summary(model38QC)

stargazer(model36ALL, model36ROC, model36QC, model38ALL, model38ROC, model38QC, type="html", out=here("Tables", "basic_class_models1.html"), column.labels = rep(c("CAN", "ROC", "QC"),2))

#### Comparing ces15 and ces19 Block Recursive Models ####

#First make a ces15 roc data frame
ces15phone %>% 
  select(ndp, liberal, conservative, bloc, region3, working_class2, union_both, young, old, male, sector, catholic, no_religion, degree, foreign, low_income, high_income, language, 
         market_liberalism, moral_traditionalism, political_disaffection, continentalism, quebec_sovereignty, ndp_id, liberal_id, conservative_id, bloc_id, personal_retrospective, 
         national_retrospective, immigration_rate, environment, redistribution, defence, liberal_leader, conservative_leader, ndp_leader, bloc_leader, quebec, occupation4, minorities, immigration, immigration2, immigration_rate, minorities_help, mip, vismin)->out15
#Now an ces19data frame
ces19phone %>% 
#  filter(quebec!=1) %>% 
  select(ndp, liberal, conservative, bloc, region3, working_class2, union_both, young, old, male, sector, catholic, no_religion, degree, foreign, low_income, high_income, language, 
         market_liberalism, moral_traditionalism, political_disaffection, continentalism, quebec_sovereignty, ndp_id, liberal_id, conservative_id, bloc_id, personal_retrospective, 
         national_retrospective, immigration_rate, environment, redistribution, defence, liberal_leader, conservative_leader, ndp_leader, bloc_leader, quebec, occupation4, minorities, immigration, immigration2, immigration_rate, minorities_help, mip, vismin)->out19
#### Build out combining ces2015 and 2019 ####
out15$survey<-rep(0, nrow(out15))
out19$survey<-rep(1, nrow(out19))
out15 %>% 
  bind_rows(., out19)->out
#Split out into ROC 
roc<-out %>% 
  filter(quebec!=1)
#Split out into Quebec
qc<-out %>% 
  filter(quebec==1)

#### NDP ROC ####

block1<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=roc)
block6<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)

#Turn into a list
roc_ndp<-list(block1, block2, block3, block4, block5, block6)
names(roc_ndp)<-c("block1", "block2", "block3", "block4", "block5", "block6")



library(kableExtra)
library(knitr)
## This code  pulls it all nicely together. 
roc_ndp %>% 
  #Tidy each of the models 
  map(., tidy) %>% 
  #bind them; the argument .id="Block" makes a new variable called Block filling it with the names of the list items from line 235
  bind_rows(., .id="Block") %>% 
  #filter in only the interaction terms; they all contain :survey
  filter(str_detect(term,":survey")) %>% 
  #Keep only the first instance of each interaction term
  #First group by term; this forms groups of the terms
  group_by(term) %>% 
  #this picks only the first term
  slice(1) %>% 
  #get rid of the :survey
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  #arrange them by block so that block1 variables appear first
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->roc_ndp_table


#### NDP QC ####

block1<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+foreign)*survey, family="binomial", data=qc)
block2<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=qc)
block6<-glm(ndp~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
#Turn into a list
qc_ndp<-list(block1, block2, block3, block4, block5, block6)
names(qc_ndp)<-c("block1", "block2", "block3", "block4", "block5", "block6")

qc_ndp %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->qc_ndp_table



#### Conservative ROC####

block1<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=roc)
block6<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)
#Turn into a list
roc_conservative<-list(block1, block2, block3, block4, block5, block6)
names(roc_conservative)<-c("block1", "block2", "block3", "block4", "block5", "block6")

roc_conservative %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->roc_conservative_table

#### Conservative QC ####
block1<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+foreign)*survey, family="binomial", data=qc)
block2<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=qc)
block6<-glm(conservative~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
#Turn into a list
qc_conservative<-list(block1, block2, block3, block4, block5, block6)
names(qc_conservative)<-c("block1", "block2", "block3", "block4", "block5", "block6")

qc_conservative %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->qc_conservative_table

#### Liberal ROC Interation ####
block1<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=roc)
block6<-glm(liberal~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)
#Turn into a list
roc_liberal<-list(block1, block2, block3, block4, block5, block6)
names(roc_liberal)<-c("block1", "block2", "block3", "block4", "block5", "block6")

roc_liberal %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->roc_liberal_table

#### Liberal QC Interation ####
block1<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+foreign)*survey, family="binomial", data=qc)
block2<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=qc)
block6<-glm(liberal~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
#Turn into a list
qc_liberal<-list(block1, block2, block3, block4, block5, block6)
names(qc_liberal)<-c("block1", "block2", "block3", "block4", "block5", "block6")

qc_liberal %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->qc_liberal_table

#### Bloc QC Interation ####
block1<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+foreign)*survey, family="binomial", data=qc)
block2<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty)*survey, family="binomial", data=qc)
block3<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id)*survey, family="binomial", data=qc)
block4<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=qc)
block5<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=qc)

block6<-glm(bloc~(working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader)*survey, family="binomial", data=qc)
#Turn into a list
qc_bloc<-list(block1, block2, block3, block4, block5, block6)
names(qc_bloc)<-c("block1", "block2", "block3", "block4", "block5", "block6")

qc_bloc %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->qc_bloc_table


#### Format Nice comprehensive QC and ROC Tables
#Step 1 combine all the parties' tables into roc and qc
roc_table<-cbind(roc_ndp_table, roc_liberal_table, roc_conservative_table)

qc_table<-cbind(qc_ndp_table, qc_bloc_table, qc_liberal_table, qc_conservative_table)
library(flextable)
#### C
#Drop out terms we don't need.
names(roc_table)
#Keep the first Block and the first term
#So drop columns 5 and 9, 6 and 10
roc_table %>% 
  select(-5, -9, -6, -10) %>% 
  #Rename them just proper names
rename(., Block=1, term=2, NDP=3, sig_ndp=4, Liberal=5,sig_liberal=6, Conservative=7, sig_con=8) %>% 
  #Turn this object into a flextable object. See https://davidgohel.github.io/flextable/
  flextable(.) %>% 
  #format the flextable to two digits
  colformat_num(digits=2) %>% 
  #bold function bolds rows i that meet conditions and column j
  #So here, it bolds rows i where sig_ndp < 0.05 and only bolds columns j
  #note that it uses formula notation ~
bold(., i=~sig_ndp< 0.05, j=~NDP+sig_ndp) %>% 
  #Repeat for LIberals
  bold(., i=~sig_liberal< 0.05, j=~Liberal+sig_liberal) %>% 
  #conservatives
bold(., i=~sig_con< 0.05, j=~Conservative+sig_con) %>% 
  #This sets the background colour conditional on the term
  #So if it is block1, 3 or 5, grey it out. 
  bg(., i=~str_detect(Block, "block1|block3|block5"), bg="grey") %>% 
    add_header_lines(values=c("ROC Block Recursive Model Coefficients, 2015 and 2019")) %>% 
#  add_footer_row(pseudos, colwidths=c(4,4))
save_as_html("Tables/roc_block_recursive_table.html")

####Combine Quebec Table ####
#Drop out terms we don't need.
#First chekc the names
names(qc_table)
#Keep the first Block and the first term
#So drop columns 5, 6 and 9, 10 and 13 and 14
qc_table %>% 
  select(-5,  -6, -9,-10, -13, -14) %>% 
rename(., Block=1, term=2, NDP=3, sig_ndp=4, Liberal=5,sig_liberal=6, Conservative=7, sig_con=8, BQ=9, sig_bq=10) %>% 
  flextable(.) %>% 
  colformat_num(digits=2) %>% 
bold(., i=~sig_ndp< 0.05, j=~NDP+sig_ndp) %>% 
  bold(., i=~sig_liberal< 0.05, j=~Liberal+sig_liberal) %>% 
bold(., i=~sig_con< 0.05, j=~Conservative+sig_con) %>% 
bg(., i=~str_detect(Block, "block1|block3|block5"), bg="grey") %>% 
  add_header_lines(values=c("Quebec Block Recursive Model Coefficients, 2015 and 2019")) %>% 
  save_as_html(., "Tables/qc_block_recursive_model.html")
#save_as_docx(., path="Tables/qc_block_recursive_model.docx")


#### Run some checks with what appears itn the table####
  #Could you please just check a few coefficients randomly to be sure they are correct
  #EAch model is stored in either roc_ndp, qc_ndp etc. etc. etc. followed by $block1, $block2, #Just pick four or five randomly in different blocks and in qc, roc. Just enough to be sure we are not making a mistake. 
summary(roc_ndp$block1)#Interaction coefficient for male:survey is -0.38; I have confirmed visually it is -0.39 in the file roc_block_recursive_table. 
summary(roc_liberal$block1)
summary(roc_conservative$block3)
#### Check on union variable ####

## Did the union movement really go down for all parties?
roc_ndp_table %>% 
  filter(term=="union_both")
roc_liberal_table %>% 
  filter(term=="union_both")
roc_conservative_table %>% 
  filter(term=="union_both")

#### Vote Flow ####

ces19phone %>% 
  select(past_vote, vote, occupation4) %>% 
  filter(past_vote!=0 & vote!=0) %>% 
    group_by(vote, past_vote) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(perc=n/sum(n)) %>% 
  mutate(vote19=case_when(
    past_vote==1&vote==1 ~ "Stay Liberal",
    past_vote==2&vote==2 ~ "Stay Conservative",
    past_vote==3&vote==3 ~ "Stay NDP",
    past_vote==4 & vote==4 ~ "Stay BQ",
    past_vote==5 & vote==5 ~ "Stay Green",
   vote==1 & past_vote!=1 ~ "To Liberal",
   vote==2 &past_vote!=2 ~ "To Conservative",
   vote==3 & past_vote!=3 ~ "To NDP",
   vote==4 & past_vote!=4 ~ "To BQ",
   vote==5 & past_vote!=5 ~ "To Green"
  )) %>% 
  mutate(vote19=factor(vote19, levels=c("Stay Liberal","To Liberal" ,"Stay Conservative","To Conservative", "Stay NDP", "To NDP", "Stay BQ", "To BQ", "Stay Green", "To Green"))) %>% 
  mutate(past_vote=fct_relevel(as_factor(past_vote),"Green", "Bloc", "NDP", "Conservative", "Liberal"), vote=as_factor(vote))%>%
  ggplot(., aes(x=vote19, y=perc, fill=past_vote))+geom_col()+coord_flip()+labs(y="Percent", x="Past Vote")+scale_fill_grey(start=0.8, end=0.2, name="Past Vote")

#### STatus of Race####

#Are the working classes more racialized?

out %>% 
select(survey, vismin, occupation4) %>% 
  as_factor() %>% 
  group_by(survey, occupation4, vismin) %>% 
  filter(!is.na(vismin)) %>% 
  filter(!is.na(occupation4)) %>% 
  summarize(n=n()) %>% 
  mutate(perc=n/sum(n)) %>% 
  print(n=100)

  table(out15$vismin)
table(out19$vismin, out19$occupation4)

mod1<-glm(conservative~working_class2*vismin, data=out19, family="binomial")
mod2<-glm(ndp~working_class2*vismin, data=out19, family="binomial")
mod2<-glm(ndp~working_class2+vismin, data=out15, family="binomial")
mod3<-glm(ndp~working_class2+vismin, data=out19, family="binomial")
summary(mod3)
summary(mod2)
summary(mod2)
summary(vismin_model)
#### Policy variation change between 2015-19####

## Currently all policy variables are coded 0 to 1 such that 1 is *more* of the variable name
library(psych)
out %>% 
  select(survey, market_liberalism, moral_traditionalism, defence, environment) %>% 
  summary()
## This works well for the purposes of fitting models, because a positive coefficient reflects an inclination that greater support for that variable is positively linked to whatever DV you ahve. 
  out %>% 
  select(survey, market_liberalism, moral_traditionalism, continentalism, defence, environment) %>% 
pivot_longer(cols=-survey) %>% 
  group_by(survey, name) %>% 
  summarize(avg=mean(value, na.rm=T))

#Howevr, for th gtraph that we want, we need this to look different.
#We want a positive number to reflect a *right-ward* shift from 2015 to 2019 and a negative number to reflect a left-ward shift. This is just purely visual. 
  
#We know from the above there was a shift *away* from market liberalism, *toward* moral_traditionalism, *toward* environme ntalism and *toward* defence spending, *awa* from continentalism

test1<-t.test(market_liberalism~survey, data=out)
test2<-t.test(moral_traditionalism~survey, data=out)
#The term estimate reports the difference
tidy(test1)
tidy(test2)

#So the term estimate reports the value for 2015-2019. When 2015-2019 is a right-ward shift, we want that number to be positive; when 2015-2019 is a *left-ward* shift, we need it to be negative. 

#So right now, there is a positive sign for the shift in market_liberalism, because the 2015 score was higher than in 2019
#But it actually was a *left-ward shift*. 
#So, if we reverse the sign, on those variables where 1 = a right-wing position, we'll get the results we want. 
#I am storing the negative-coded variables in _x
out$moral_traditionalism_x<-out$moral_traditionalism*-1
out$market_liberalism_x<-out$market_liberalism*-1
out$continentalism_x<-out$continentalism*-1
out$defence_x<-out$defence*-1
out$immigration_rate<-out$immigration_rate*-1
#Now check:
test3<-t.test(market_liberalism_x~survey, data=out)
tidy(test3)
tidy(test1)
#The sign for market liberalism is just reversed, so it works. 
rm(test1, test2, test3)

#Policy rating changes
#Start with tghe combined 15 and 19 data frame
out %>% 
  #select the variables we need for the attitudinal shifts
  #pay attention to selecting the variables that have been recoded i.e. they end with _x
  select(ends_with("_x"),  environment,redistribution, immigration, immigration_rate, minorities_help, survey, quebec, occupation4) %>% 
  #Rename the variables for aesthetic purposes
rename(., `Moral Traditionalism`=1, `Market Liberalism`=2, `Continentalism`=3, Defence=4, Environment=5, Redistribution=6, Immigration=7, `Immigration Rate`=8, `Help Minorities`=9) %>% 
  #Pivot these down into one single variable
  pivot_longer(cols=1:9) %>% 
  #There are a few cases missing on this variable
    filter(!is.na(quebec)) %>% 
 # form groups by variable name, occupation and quebec
  nest_by(name, occupation4, quebec=as_factor(quebec)) %>% 
#Tidy the t.test results for value ~ survey
  mutate(mod=tidy(t.test(value~survey, data=data))) %>% 
  #Plot with occupation4 on the x, relevelled to put workers at the bottom and mamnagers at the top; y is the estimate
 ggplot(., aes(x=fct_relevel(occupation4, "Working_Class", "Routine_Nonmanual", "Self-Employed", "Professionals", "Managers"), y=mod$estimate))+labs(x="Class", y="Difference (2019-2015)", subtitle="CES 2015 and 2019")+geom_point(aes(col=quebec), size=0.5,position=position_dodge(width=0.5))+facet_wrap(~name)+coord_flip(expand=T, clip="off")+geom_hline(aes(yintercept=0), linetype=2)+scale_color_grey(start=0.2 ,end=0.5)+geom_linerange(aes(ymin=mod$conf.low, ymax=mod$conf.high, col=quebec), position=position_dodge(width=0.5))
ggsave(here("Plots", "attitudinal_differences_2015_2019.png"), width=6, height=4)

#Leader rating changes
out %>% 
  select(`Liberal`=liberal_leader, `Conservative`=conservative_leader, `NDP`=ndp_leader, `BQ`=bloc_leader, Survey=survey, Class=occupation4, Quebec=quebec) %>%
  filter(!is.na(Quebec)) %>% 
  pivot_longer(cols=1:4, names_to=c("Party")) %>% 
    filter(!is.na(value)) %>% 
  nest_by(Class, Quebec=as_factor(Quebec), Party) %>% 
  mutate(mod=tidy(t.test(data=data, value~Survey))) %>% 
  ggplot(., aes(x=fct_relevel(Class, "Working_Class", "Routine_Nonmanual", "Self-Employed", "Professionals", "Managers"), y=mod$estimate*-1, col=Quebec))+geom_point()+ylim(-0.2,0.2)+labs(x="Class", y="Difference (2019-2015)")+coord_flip()+scale_color_grey()+facet_wrap(~Party)+geom_hline(yintercept=0, linetype=2)
ggsave(here("Plots", "leader_approval_ratings.png"), width=6, height=4)
#### Most Important Issue ####
out %>% 
group_by(Survey=as_factor(survey), `Most Important Problem`=as_factor(mip), Quebec=as_factor(quebec), occupation4) %>%
  summarise(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  mutate(Election=car::Recode(Survey, "0=2015; 1=2019", as.factor=T, levels=c("2019", "2015")))%>% 
  filter(!is.na(`Most Important Problem`)) %>% 
  ggplot(., aes(y=reorder(`Most Important Problem`,n), x=n))+geom_col(position=position_dodge(preserve="single"))+scale_fill_grey()+labs(y="Most Important Problem")+facet_wrap(~Election)
ggsave("Plots/mip_2015_2019.png")
#------------------------------------------------------------------------------------------------
#### Redistribution descriptives ####

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average redistribution of respondents in ces studies")

ces %>% 
  group_by(election, working_class) %>% 
  filter(!is.na(working_class)) %>%
  summarize(avg_age=mean(redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average redistribution of WC respondents in ces studies")

ces %>% 
  group_by(election) %>% 
  summarize(avg_age=mean(pro_redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average pro-redistribution of respondents in ces studies")

ces %>% 
  group_by(election, working_class) %>% 
  filter(!is.na(working_class)) %>%
  summarize(avg_age=mean(pro_redistribution, na.rm=T)) %>% 
  ggplot(., aes(x=election, y=avg_age))+geom_point()+labs(title="Average pro-redistribution of WC respondents in ces studies")

ces %>%
  filter(!is.na(redistribution)) %>%
  group_by(working_class, election) %>%
  summarize(mean_redistribution = mean(redistribution))

ces %>%
  filter(!is.na(pro_redistribution)) %>%
  group_by(working_class, election) %>%
  summarize(mean_pro_redistribution = mean(pro_redistribution))

## Share of Pro-redistribution Working Class members voting NDP
ces %>% 
  group_by(election, pro_redistribution, working_class, ndp) %>% 
  summarize(n=n()) %>% 
  filter(is.na(pro_redistribution)==F) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(ndp)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(ndp==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(pro_redistribution)))+geom_col(position="dodge")+labs(title="Share of Pro-redistribution Working Class members voting NDP")
ggsave(here("Plots", "Pro_redistribution_working_class_vote_NDP.png"))

## Share of Pro-redistribution Working Class members voting Conservative
ces %>% 
  group_by(election, pro_redistribution, working_class, conservative) %>% 
  summarize(n=n()) %>% 
  filter(is.na(pro_redistribution)==F) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(conservative)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(conservative==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(pro_redistribution)))+geom_col(position="dodge")+labs(title="Share of Pro-redistribution Working Class members voting Conservative")
ggsave(here("Plots", "Pro_redistribution_working_class_vote_Conservative.png"))

## Share of Pro-redistribution Working Class members voting Liberal
ces %>% 
  group_by(election, pro_redistribution, working_class, liberal) %>% 
  summarize(n=n()) %>% 
  filter(is.na(pro_redistribution)==F) %>% 
  filter(is.na(working_class)==F) %>% 
  filter(is.na(liberal)==F) %>% 
  mutate(percent=n/sum(n)) %>% 
  filter(working_class==1) %>% 
  filter(liberal==1) %>% 
  ggplot(., aes(x=election, y=percent, fill=as_factor(pro_redistribution)))+geom_col(position="dodge")+labs(title="Share of Pro-redistribution Working Class members voting Liberal")
ggsave(here("Plots", "Pro_redistribution_working_class_vote_Liberal.png"))

# Working Class redistribution by year
### I couldn't get your code below to work. This next section starts getting at it. 
ces %>% 
  select(election, occupation2, redistribution) %>% 
  group_by(election, occupation2) %>% 
  summarize(support_redistribution=mean(redistribution, na.rm=T)) %>% 
  filter(occupation2=="Working_Class"& election> 1988)
  

ces93 %>%
  select(occupation, redistribution, pro_redistribution) %>% 
  group_by(occupation) %>%
  summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)
# 
# ces97 %>%
#   select(working_class, redistribution, pro_redistribution) %>% 
#   group_by(working_class) %>%
#   summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)
# 
# ces0411 %>%
#   select(working_class04, redistribution04, pro_redistribution04) %>% 
#   group_by(working_class04) %>%
#   summarise_at(vars(redistribution04, pro_redistribution04), mean, na.rm=T)
# 
# ces0411 %>%
#   select(working_class06, redistribution06, pro_redistribution06) %>% 
#   group_by(working_class06) %>%
#   summarise_at(vars(redistribution06, pro_redistribution06), mean, na.rm=T)
# 
# ces0411 %>%
#   select(working_class08, redistribution08, pro_redistribution08) %>% 
#   group_by(working_class08) %>%
#   summarise_at(vars(redistribution08, pro_redistribution08), mean, na.rm=T)
# 
# ces0411 %>%
#   select(working_class11, redistribution11, pro_redistribution11) %>% 
#   group_by(working_class11) %>%
#   summarise_at(vars(redistribution11, pro_redistribution11), mean, na.rm=T)
# 
# ces15phone %>%
#   select(working_class, redistribution, pro_redistribution) %>% 
#   group_by(working_class) %>%
#   summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)
# 
# ces19phone %>%
#   select(working_class, redistribution, pro_redistribution) %>% 
#   group_by(working_class) %>%
#   summarise_at(vars(redistribution, pro_redistribution), mean, na.rm=T)

# Working Class voting by pro-redistribution
# ces19phone %>%
#   select(working_class, pro_redistribution, liberal, conservative, ndp, bloc, green) %>% 
#   group_by(working_class, pro_redistribution) %>%
#   summarise_at(vars(liberal, conservative, ndp, bloc, green), mean, na.rm=T) %>% 
#   as.data.frame() %>% 
#   stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Pro_redistribution_Working_Class_Vote_2019.html"))
# 
# # Working Class voting pro-redistribution by election
# ces %>%
#   select(election, working_class, pro_redistribution, liberal, conservative, ndp) %>% 
#   group_by(election, working_class, pro_redistribution) %>%
#   summarise_at(vars(liberal, conservative, ndp), mean, na.rm=T) %>% 
#   as.data.frame() %>%
#   filter(!is.na(working_class)) %>% 
#   stargazer(., type="html", summary=F, digits=2, out=here("Tables", "Pro_redistribution_Working_Class_Vote_by_election.html"))
# 
# #------------------------------------------------------------------------------------------------
# #### Working Class descriptives ####
# 
# #Share of Working class voting NDP
# ces %>% 
#   group_by(election, working_class, ndp) %>% 
#   summarize(n=n()) %>% 
#   filter(is.na(working_class)==F) %>% 
#   filter(is.na(ndp)==F) %>% 
#   mutate(percent=n/sum(n)) %>% 
#   filter(working_class==1) %>% 
#   filter(ndp==1) %>% 
#   ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting NDP")
# ggsave(here("Plots", "NDP_working_class_vote.png"))
# 
# #Share of Working class voting Liberal
# ces %>% 
#   group_by(election, working_class, liberal) %>% 
#   summarize(n=n()) %>% 
#   filter(is.na(working_class)==F) %>% 
#   filter(is.na(liberal)==F) %>% 
#   mutate(percent=n/sum(n)) %>% 
#   filter(working_class==1) %>% 
#   filter(liberal==1) %>% 
#   ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting Liberal")
# ggsave(here("Plots", "Liberal_working_class_vote.png"))
# 
# #Share of Working class voting Conservative
# ces %>% 
#   group_by(election, working_class, conservative) %>% 
#   summarize(n=n()) %>% 
#   filter(is.na(working_class)==F) %>% 
#   filter(is.na(conservative)==F) %>% 
#   mutate(percent=n/sum(n)) %>% 
#   filter(working_class==1) %>% 
#   filter(conservative==1) %>% 
#   ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting Conservative")
# ggsave(here("Plots", "Conservative_working_class_vote.png"))
# 
# #Share of Working class voting Other
# ces %>% 
#   group_by(election, working_class, other) %>% 
#   summarize(n=n()) %>% 
#   filter(is.na(working_class)==F) %>% 
#   filter(is.na(other)==F) %>% 
#   mutate(percent=n/sum(n)) %>% 
#   filter(working_class==1) %>% 
#   filter(other==1) %>% 
#   ggplot(., aes(x=election, y=percent, fill=as_factor(working_class)))+geom_col(position="dodge")+labs(title="Share of Working Class respondents voting Other")
# ggsave(here("Plots", "Other_working_class_vote.png"))
# 
# #Party Vote Shares of Working Class
# #This was your code
# 
# # ces %>% 
# #   group_by(election, working_class, vote) %>% 
# #   summarize(n=n()) %>% 
# #   mutate(pct=n/sum(n)) %>%
# #   filter(working_class==1 & (vote<4 & vote>0)) %>% 
# #   ggplot(.,aes(x=as.numeric(election), y=pct))+
# #   geom_point()+
# #   geom_smooth(method="lm", se=F)+
# #   facet_grid(~as_factor(vote))+
# #   labs(title="Share of Working Class voting for political parties over time")
# # ggsave(here("Plots", "Party_shares_working_class_vote.png"))
# 
# # #My modifications
# # ces %>% 
# #   group_by(election, working_class, vote) %>% 
# #   summarize(n=n()) %>% 
# #   mutate(pct=n/sum(n)*100) %>%
# #   filter(working_class==1 & (vote<4 & vote>0)) %>% 
# #   ggplot(.,aes(x=as.numeric(election), y=pct, col=as_factor(vote)))+
# #   geom_line()+
# #   geom_point()+
# #   scale_color_manual(values=c("red", "blue", "orange"), name="Party")+
# #   labs(title="Share of Working Class voting for political parties over time", x="Year", y="Percent")
# # ggsave(here("Plots", "Party_shares_working_class_vote.png"))
# # #Percent of NDP Voters Working Class
# # ces %>% 
# #   group_by(election, vote, working_class) %>% 
# #   summarize(n=n()) %>% 
# #   mutate(pct=n/sum(n)) %>%
# #   filter(working_class==1 & vote==3) %>% 
# #   ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="NDP Voter % that are Working Class")
# # ggsave(here("Plots", "NDP_Voters_Working_Class_Percent.png"))
# # 
# # #Percent of Liberal Voters Working Class
# # ces %>% 
# #   group_by(election, vote, working_class) %>% 
# #   summarize(n=n()) %>% 
# #   mutate(pct=n/sum(n)) %>%
# #   filter(working_class==1 & vote==1) %>% 
# #   ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Liberal Voter % that are Working Class")
# # ggsave(here("Plots", "Lib_Voters_Working_Class_Percent.png"))
# # 
# # #Percent of Conservative Voters Working Class
# # ces %>% 
# #   group_by(election, vote, working_class) %>% 
# #   summarize(n=n()) %>% 
# #   mutate(pct=n/sum(n)) %>%
# #   filter(working_class==1 & vote==2) %>% 
# #   ggplot(., aes(x=election, y=pct))+geom_point()+labs(title="Conservative Voter % that are Working Class")
# # ggsave(here("Plots", "Con_Voters_Working_Class_Percent.png"))
# 
# 

#### Performance Models ####

#ROC NDP
table(roc$quebec)
table(qc$quebec)
#Start with ROC
roc%>%
  #nest by everything survey to fit one model per survey year
  nest(-survey)%>%
#Createacolumn called mod that is the result of fitting binomial m odel; data=x i.e. data is each separate survety
  mutate(ndp_demographics=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, family="binomial", data=x))) %>%
  #Tidy each model for nice use 
 #mutate(tidied=map(demographics, tidy)) %>%
  #add column of PseudoR2 for each 
  mutate(ndp_demographics_r2=map(ndp_demographics, PseudoR2))%>%
  unnest(ndp_demographics_r2)


# Repeat for QC


#for subset just start with out
#out
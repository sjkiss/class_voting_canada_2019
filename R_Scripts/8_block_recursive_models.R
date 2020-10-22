#Comparing ces15 and ces19 Block Recursive Models
library(nnet)
library(broom)
library(purrr)
#### Some 2015 recodes ####
###These could be cleaned up
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

#Turn income into factor with Middle as reference
ces15phone$income3<-Recode(as.factor(ces15phone$income), "1='Low_Income' ; 2:4='Middle_Income' ; 5='High_Income'", levels=c('Low_Income', 'Middle_Income', 'High_Income'))
levels(ces15phone$income3)
table(ces15phone$income3)

#Other dummies
ces15phone$low_income<-Recode(ces15phone$income, "2:5=0; 1=1")
ces15phone$high_income<-Recode(ces15phone$income, "1:4=0; 5=1")
ces15phone$no_religion<-Recode(ces15phone$religion, "0=1; 1:3=0; NA=NA")
ces15phone$catholic<-Recode(ces15phone$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces15phone$young<-Recode(ces15phone$age, "35:115=0; 18:34=1")
ces15phone$old<-Recode(ces15phone$age, "55:115=1; 18:54=0")
ces15phone$foreign<-Recode(ces15phone$native, "1=0; 0=1")

#Dummies coded missing as 0
#ces15phone$low_income<-Recode(ces15phone$income, "else=0; 1=1")
#ces15phone$high_income<-Recode(ces15phone$income, "else=0; 5=1")
#ces15phone$no_religion<-Recode(ces15phone$religion, "0=1; else=0")
#ces15phone$catholic<-Recode(ces15phone$religion, "1=1; else=0")
#ces15phone$young<-Recode(ces15phone$age, "else=0; 18:34=1")
#ces15phone$old<-Recode(ces15phone$age, "55:100=1; else=0")
#ces15phone$foreign<-Recode(ces15phone$native, "else=0; 0=1")
table(ces15phone$low_income)
table(ces15phone$high_income)
table(ces15phone$no_religion)
table(ces15phone$catholic)
table(ces15phone$young)
table(ces15phone$old)
table(ces15phone$foreign)
ces15phone$working_class<-Recode(ces15phone$working_class, "1=1; else=0")
ces15phone$working_class2<-Recode(ces15phone$working_class2, "1=1; else=0")
#ces15phone$union_both<-Recode(ces15phone$union_both, "1=1; else=0")
#ces15phone$male<-Recode(ces15phone$male, "1=1; else=0")
#ces15phone$sector<-Recode(ces15phone$sector, "1=1; else=0")
#ces15phone$degree<-Recode(ces15phone$degree, "1=1; else=0")
#ces15phone$language<-Recode(ces15phone$language, "1=1; else=0")
table(ces15phone$working_class)
table(ces15phone$working_class2)
table(ces15phone$union_both)
table(ces15phone$male)
table(ces15phone$sector)
table(ces15phone$degree)
table(ces15phone$language)

# Party Id
#ces15phone$liberal_id<-Recode(ces15phone$party_id, "1=1; 0=0; 2:4=0; else=NA")
#ces15phone$conservative_id<-Recode(ces15phone$party_id, "2=1; 0:1=0; 3:4=0; else=NA")
#ces15phone$ndp_id<-Recode(ces15phone$party_id, "3=1; 0:2=0; 4=0; else=NA")
#ces15phone$bloc_id<-Recode(ces15phone$party_id, "4=1; 0:3=0; else=NA")
ces15phone$liberal_id<-Recode(ces15phone$party_id, "1=1; else=0")
ces15phone$conservative_id<-Recode(ces15phone$party_id, "2=1; else=0")
ces15phone$ndp_id<-Recode(ces15phone$party_id, "3=1; else=0")
ces15phone$bloc_id<-Recode(ces15phone$party_id, "4=1; else=0")
table(ces15phone$liberal_id)
table(ces15phone$conservative_id)
table(ces15phone$ndp_id)
table(ces15phone$bloc_id)

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
table(ces15phone$liberal)
table(ces15phone$conservative)
table(ces15phone$ndp)
table(ces15phone$bloc)
table(ces15phone$green)

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
table(ces19phone$working_class2)

#Turn region into factor with East as reference case
ces19phone$region3<-Recode(as.factor(ces19phone$region), "1='East' ; 2='Ontario' ; 3='West'", levels=c('East', 'Ontario', 'West'))
levels(ces19phone$region3)
table(ces19phone$region3)

#Turn income into factor with Middle as reference
ces19phone$income3<-Recode(as.factor(ces19phone$income), "1='Low_Income' ; 2:4='Middle_Income' ; 5='High_Income'", levels=c('Low_Income', 'Middle_Income', 'High_Income'))
levels(ces19phone$income3)
table(ces19phone$income3)

#Other dummies
ces19phone$low_income<-Recode(ces19phone$income, "2:5=0; 1=1")
ces19phone$high_income<-Recode(ces19phone$income, "1:4=0; 5=1")
ces19phone$no_religion<-Recode(ces19phone$religion, "0=1; 1:3=0; NA=NA")
ces19phone$catholic<-Recode(ces19phone$religion, "1=1; 2:3=0; 0=0; NA=NA")
ces19phone$young<-Recode(ces19phone$age, "35:100=0; 18:34=1")
ces19phone$old<-Recode(ces19phone$age, "55:100=1; 18:54=0")
ces19phone$foreign<-Recode(ces19phone$native, "1=0; 0=1")

#Dummies coded missing as 0
#ces19phone$low_income<-Recode(ces19phone$income, "else=0; 1=1")
#ces19phone$high_income<-Recode(ces19phone$income, "else=0; 5=1")
#ces19phone$no_religion<-Recode(ces19phone$religion, "0=1; else=0")
#ces19phone$catholic<-Recode(ces19phone$religion, "1=1; else=0")
#ces19phone$young<-Recode(ces19phone$age, "else=0; 18:34=1")
#ces19phone$old<-Recode(ces19phone$age, "55:100=1; else=0")
#ces19phone$foreign<-Recode(ces19phone$native, "else=0; 0=1")
table(ces19phone$low_income)
table(ces19phone$high_income)
table(ces19phone$no_religion)
table(ces19phone$catholic)
table(ces19phone$young)
table(ces19phone$old)
table(ces19phone$foreign)
ces19phone$working_class<-Recode(ces19phone$working_class, "1=1; else=0")
ces19phone$working_class2<-Recode(ces19phone$working_class2, "1=1; else=0")
#ces19phone$union_both<-Recode(ces19phone$union_both, "1=1; else=0")
#ces19phone$male<-Recode(ces19phone$male, "1=1; else=0")
#ces19phone$sector<-Recode(ces19phone$sector, "1=1; else=0")
#ces19phone$degree<-Recode(ces19phone$degree, "1=1; else=0")
#ces19phone$language<-Recode(ces19phone$language, "1=1; else=0")
table(ces19phone$working_class)
table(ces19phone$working_class2)
table(ces19phone$union_both)
table(ces19phone$male)
table(ces19phone$sector)
table(ces19phone$degree)
table(ces19phone$language)

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

#First make a ces15 roc data frame
ces15phone %>% 
  select(ndp, liberal, conservative, bloc, region3, working_class2, union_both, young, old, male, sector, catholic, no_religion, degree, foreign, low_income, high_income, language, 
         market_liberalism, moral_traditionalism, political_disaffection, continentalism, quebec_sovereignty, ndp_id, liberal_id, conservative_id, bloc_id, personal_retrospective, 
         national_retrospective, immigration_rate, environment, redistribution, defence, liberal_leader, conservative_leader, ndp_leader, bloc_leader, quebec)->out15
#Now an ces19data frame
ces19phone %>% 
#  filter(quebec!=1) %>% 
  select(ndp, liberal, conservative, bloc, region3, working_class2, union_both, young, old, male, sector, catholic, no_religion, degree, foreign, low_income, high_income, language, 
         market_liberalism, moral_traditionalism, political_disaffection, continentalism, quebec_sovereignty, ndp_id, liberal_id, conservative_id, bloc_id, personal_retrospective, 
         national_retrospective, immigration_rate, environment, redistribution, defence, liberal_leader, conservative_leader, ndp_leader, bloc_leader, quebec)->out19
out15$survey<-rep(0, nrow(out15))
out19$survey<-rep(1, nrow(out19))
out15 %>% 
  bind_rows(., out19)->out
roc<-out %>% 
  filter(quebec!=1)
qc<-out %>% 
  filter(quebec==1)

#Model 1
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

#Save the values to be bolded here
#This requires changing which object is used e.g. roc_ndp_table might become qc_ndp_table etc. 
to_bold<-roc_ndp_table$p.value<0.05
roc_ndp_table %>% 
  kable(., digits=2) %>% 
  #bold the third and fourth columns
  column_spec(3:4, bold=to_bold) %>% 
  save_kable(file="Tables/ndp_roc_interaction.html")

#### NDP QC Interation ####

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

#Save the values to be bolded here
to_bold<-qc_ndp_table$p.value<0.05
qc_ndp_table %>% 
  kable(., digits=2) %>% 
  column_spec(3:4, bold=to_bold) %>% 
  save_kable(file="Tables/ndp_qc_interaction.html")

#### Conservative ROC Interation ####

block1<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income)*survey, family="binomial", data=roc)
block2<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=roc)
block3<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=roc)
block4<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=roc)
block5<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=roc)
block6<-glm(conservative~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=roc)
#Turn into a list
conservative_ndp<-list(block1, block2, block3, block4, block5, block6)
names(conservative_ndp)<-c("block1", "block2", "block3", "block4", "block5", "block6")

conservative_ndp %>% 
  map(., tidy) %>% 
  bind_rows(., .id="Block") %>% 
  filter(str_detect(term,":survey")) %>% 
  group_by(term) %>% 
  slice(1) %>% 
  mutate(term=str_replace_all(term, ":survey", "")) %>% 
  arrange(Block) %>% 
  select(Block, term, estimate,p.value)->roc_conservative_table

#Save the values to be bolded here
to_bold<-roc_conservative_table$p.value<0.05
roc_conservative_table %>% 
  kable(., digits=2) %>% 
  column_spec(3:4, bold=to_bold) %>% 
  save_kable(file="Tables/conservative_roc_interaction.html")

#### Conservative QC Interation ####
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

#Save the values to be bolded here
to_bold<-qc_conservative_table$p.value<0.05
qc_conservative_table %>% 
  kable(., digits=2) %>% 
  column_spec(3:4, bold=to_bold) %>% 
  save_kable(file="Tables/conservative_qc_interaction.html")

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

#Save the values to be bolded here
to_bold<-roc_liberal_table$p.value<0.05
roc_liberal_table %>% 
  kable(., digits=2) %>% 
  column_spec(3:4, bold=to_bold) %>% 
  save_kable(file="Tables/liberal_roc_interaction.html")

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

#Save the values to be bolded here
to_bold<-qc_liberal_table$p.value<0.05
qc_liberal_table %>% 
  kable(., digits=2) %>% 
  column_spec(3:4, bold=to_bold) %>% 
  save_kable(file="Tables/liberal_qc_interaction.html")

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

#Save the values to be bolded here
to_bold<-qc_bloc_table$p.value<0.05
qc_bloc_table %>% 
  kable(., digits=2) %>% 
  column_spec(3:4, bold=to_bold) %>% 
  save_kable(file="Tables/bloc_qc_interaction.html")

# out %>% 
#   #this is how we filter for quebec / roc
#   filter(quebec!=1) %>% 
#   #group_by(survey) %>% 
# nest() %>%
#   mutate(
#     block1=map(data, function(x) glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income)*survey, family="binomial", data=.)), 
#     block2=map(data, function(x) glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism)*survey, family="binomial", data=x)),
#     block3=map(data, function(x) glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id)*survey, family="binomial", data=x)),
#     block4=map(data, function(x) glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective)*survey, family="binomial", data=x)),
#     block5=map(data, function(x) glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence)*survey, family="binomial", data=x)),
#     block6=map(data, function(x) glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader)*survey, family="binomial", data=x))
#     #Get the interaction models
#     #Note I found a quick way to do an interaction for all the terms. 
#     #Group all the main effects in parenthesess, then multiply by the interaction variable
# ) ->ndp_models_ROC
# 
# ndp_models_ROC
#     ndp_models_ROC %>% 
#   mutate(block1=map(block1, tidy),
#          block2=map(block2, tidy), 
#          block3=map(block3, tidy),
#          block4=map(block4, tidy),
#          block5=map(block5, tidy),
#          block6=map(block6, tidy)) ->ndp_models_ROC
# 
# #Get block 1
# ndp_models_ROC %>% 
#   unnest(block1) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   mutate(block=rep("block 1", nrow(.))) %>% 
#   select(term, difference, block)->block1
# 
# #Get block 2
# ndp_models_ROC %>% 
#   #Be sure to get the right block here 
#   unnest(block2) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 2", nrow(.))) %>% 
#     select(term, difference, block)->block2
# 
# #Get block 3
# ndp_models_ROC %>% 
#   unnest(block3) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#     #Name the correct block here 
#   mutate(block=rep("block 3", nrow(.))) %>% 
#   select(term, difference, block)->block3
# 
# #Get block 4
# ndp_models_ROC %>% 
#   unnest(block4) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 4", nrow(.))) %>% 
#   select(term, difference, block)->block4
# 
# #Get block 5
# ndp_models_ROC %>% 
#   unnest(block5) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 5", nrow(.))) %>% 
#   select(term, difference, block)->block5
# 
# #Get block 6
# ndp_models_ROC %>% 
#   unnest(block6) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 6", nrow(.))) %>% 
#   select(term, difference, block)->block6
# 
# #Bind them together
# library(knitr)
# block1 %>% 
#   #bind all the things together
#   #the dot here just means what came before it (e.g. block 1)
#   #then it binds all the following arguments to block 1 in successive order 
#   bind_rows(., block2, block3, block4, block5, block6) %>% 
#   #This next bit is to pick out the first occurrence of each term. 
#   #We want to do this only to get the first time a variable is entered
#   #Group by the term
#   #So this creates a grouped data frame where each group is made up of all the variables with the same name (e.g. all the variables with the name 'young' ) are in one gtroup
#   group_by(term)%>%
#   #pick out the first occurrence of each term
#   slice(1)%>%
#   #Then we need to arrange these by block. 
#   #I ran this without this and it seemed like the kable() caommand below was printing the table with the variables sorted alphabetically. We want them sorted by block
#   #wE dumpt it out here into an object, because I think that was what was tripping you up.
#   arrange(block)->NDP_ROC_block_difference_model #Name it whatever is most meanginful.
# 
# #Check
# NDP_ROC_block_difference_model
# 
# #Fit a pooled model for the interactions
# ndp_roc_interaction_model<-glm(ndp~(region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+survey)*survey, family="binomial", data=subset(out, quebec!=1))
# summary(ndp_roc_interaction_model)
# 
# ###Note: there are almost no interaction effects. 
# ### Does this help our story? Note that market_liberalism is kind of significant, and positive. And union_both gets kind of close, which also helps our story. Is this a sample size issue? There are a lot of variables here. 
# 
# #Tidy it
# ndp_roc_interaction_model<-tidy(ndp_roc_interaction_model)
# #We just need to merge the NDP with the significance terms from the block model 
# #Because the interaction model contains terms for the main effects (e.g. "young", "old", "sector", we need to drop those because we only want the significance of the of the interaction terms, they contain the word survey, so we can drop any row that does not have the word survey)
# ndp_roc_interaction_model %>% 
#   filter(str_detect(term, "survey")) %>% 
#   #Now we need to delete the :survey from the term to match what is in the block difference model 
#   mutate(term=str_replace_all(term, ":survey", "")) %>% 
# #Now keep on only the term and the p.value
#   select(term, p.value) %>% 
#   #join in the results from the block_difference_model with the interaction p-values
#   left_join(NDP_ROC_block_difference_model, .) %>% 
#   ungroup() %>% 
# slice(-1)->ndp_roc_full_results
# 
# ndp_roc_full_results
# 
# #You may have to install these packages
# library(readr)
# library(kableExtra)
# library(knitr)
# #Define a bolding condition, here we're going to bold any term that has a p.value less than 0.1
# col_bold<-ndp_roc_full_results$p.value<0.1
# ndp_roc_full_results %>% 
#   kable(format="html", digits=2) %>% 
#   column_spec(column=2, bold=col_bold) %>% 
# write_file(.,"Tables/NDP_Difference_Table.html" )
# 
# 
# 
# #### NDP QC ####
# out %>% 
#       filter(quebec==1) %>% 
#   group_by(survey) %>% 
#   nest() %>% 
#   mutate(
#     block1=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign, family="binomial", data=x)), 
#     block2=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x)),
#     block3=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x)),
#     block4=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
#     block5=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
#     block6=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))
#   )->ndp_models_QC
# ndp_models_QC %>% 
#   mutate(block1=map(block1, tidy),
#          block2=map(block2, tidy), 
#          block3=map(block3, tidy),
#          block4=map(block4, tidy),
#          block5=map(block5, tidy),
#          block6=map(block6, tidy)) ->ndp_models_QC
# 
# #Get block 1
# ndp_models_QC %>% 
#   unnest(block1) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   mutate(block=rep("block 1", nrow(.))) %>% 
#   select(term, difference, block)->block1
# 
# #Get block 2
# ndp_models_QC %>% 
#   #Be sure to get the right block here 
#   unnest(block2) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 2", nrow(.))) %>% 
#   select(term, difference, block)->block2
# 
# #Get block 3
# ndp_models_QC %>% 
#   unnest(block3) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 3", nrow(.))) %>% 
#   select(term, difference, block)->block3
# 
# #Get block 4
# ndp_models_QC %>% 
#   unnest(block4) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 4", nrow(.))) %>% 
#   select(term, difference, block)->block4
# 
# #Get block 5
# ndp_models_QC %>% 
#   unnest(block5) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 5", nrow(.))) %>% 
#   select(term, difference, block)->block5
# 
# #Get block 6
# ndp_models_QC %>% 
#   unnest(block6) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 6", nrow(.))) %>% 
#   select(term, difference, block)->block6
# 
# #Bind them together
# block1 %>% 
#   bind_rows(., block2, block3, block4, block5, block6) %>% 
#   group_by(term)%>%
#   slice(1)%>%
#   arrange(block)->NDP_QC_block_difference_model
# 
# NDP_QC_block_difference_model %>%
#   kable()
# 
# #Model 2
# #### Liberal ROC ####
# out %>% 
#   filter(quebec!=1) %>% 
#   group_by(survey) %>% 
#   nest() %>% 
#   mutate(
#     block1=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, family="binomial", data=x)), 
#     block2=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x)),
#     block3=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x)),
#     block4=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
#     block5=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
#     block6=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))
#   )->liberal_models_ROC
# liberal_models_ROC %>% 
#   mutate(block1=map(block1, tidy),
#          block2=map(block2, tidy), 
#          block3=map(block3, tidy),
#          block4=map(block4, tidy),
#          block5=map(block5, tidy),
#          block6=map(block6, tidy)) ->liberal_models_ROC
# 
# #Get block 1
# liberal_models_ROC %>% 
#   unnest(block1) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   mutate(block=rep("block 1", nrow(.))) %>% 
#   select(term, difference, block)->block1
# 
# #Get block 2
# liberal_models_ROC %>% 
#   #Be sure to get the right block here 
#   unnest(block2) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 2", nrow(.))) %>% 
#   select(term, difference, block)->block2
# 
# #Get block 3
# liberal_models_ROC %>% 
#   unnest(block3) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 3", nrow(.))) %>% 
#   select(term, difference, block)->block3
# 
# #Get block 4
# liberal_models_ROC %>% 
#   unnest(block4) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 4", nrow(.))) %>% 
#   select(term, difference, block)->block4
# 
# #Get block 5
# liberal_models_ROC %>% 
#   unnest(block5) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 5", nrow(.))) %>% 
#   select(term, difference, block)->block5
# 
# #Get block 6
# liberal_models_ROC %>% 
#   unnest(block6) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 6", nrow(.))) %>% 
#   select(term, difference, block)->block6
# 
# #Bind them together
# block1 %>% 
#   bind_rows(., block2, block3, block4, block5, block6) %>% 
#   group_by(term)%>%
#   slice(1)%>%
#   arrange(block)->Liberal_ROC_block_difference_model
# 
# Liberal_ROC_block_difference_model %>%
#   kable()
# 
# #### Liberal QC ####
# out %>% 
#       filter(quebec==1) %>% 
#   group_by(survey) %>% 
#   nest() %>% 
#   mutate(
#     block1=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign, family="binomial", data=x)), 
#     block2=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x)),
#     block3=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x)),
#     block4=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
#     block5=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
#     block6=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))
#   )->liberal_models_QC
# liberal_models_QC %>% 
#   mutate(block1=map(block1, tidy),
#          block2=map(block2, tidy), 
#          block3=map(block3, tidy),
#          block4=map(block4, tidy),
#          block5=map(block5, tidy),
#          block6=map(block6, tidy)) ->liberal_models_QC
# 
# #Get block 1
# liberal_models_QC %>% 
#   unnest(block1) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   mutate(block=rep("block 1", nrow(.))) %>% 
#   select(term, difference, block)->block1
# 
# #Get block 2
# liberal_models_QC %>% 
#   #Be sure to get the right block here 
#   unnest(block2) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 2", nrow(.))) %>% 
#   select(term, difference, block)->block2
# 
# #Get block 3
# liberal_models_QC %>% 
#   unnest(block3) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 3", nrow(.))) %>% 
#   select(term, difference, block)->block3
# 
# #Get block 4
# liberal_models_QC %>% 
#   unnest(block4) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 4", nrow(.))) %>% 
#   select(term, difference, block)->block4
# 
# #Get block 5
# liberal_models_QC %>% 
#   unnest(block5) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 5", nrow(.))) %>% 
#   select(term, difference, block)->block5
# 
# #Get block 6
# liberal_models_QC %>% 
#   unnest(block6) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 6", nrow(.))) %>% 
#   select(term, difference, block)->block6
# 
# #Bind them together
# block1 %>% 
#   bind_rows(., block2, block3, block4, block5, block6) %>% 
#   group_by(term)%>%
#   slice(1)%>%
#   arrange(block)->Liberal_QC_block_difference_model
# 
# Liberal_QC_block_difference_model %>%
#   kable()
# 
# #Model 3
# #### Conservative ROC ####
# out %>% 
#     filter(quebec!=1) %>% 
#   group_by(survey) %>% 
#   nest() %>% 
#   mutate(
#     block1=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, family="binomial", data=x)), 
#     block2=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x)),
#     block3=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x)),
#     block4=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
#     block5=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
#     block6=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))
#   )->conservative_models_ROC
# conservative_models_ROC %>% 
#   mutate(block1=map(block1, tidy),
#          block2=map(block2, tidy), 
#          block3=map(block3, tidy),
#          block4=map(block4, tidy),
#          block5=map(block5, tidy),
#          block6=map(block6, tidy)) ->conservative_models_ROC
# 
# #Get block 1
# conservative_models_ROC %>% 
#   unnest(block1) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   mutate(block=rep("block 1", nrow(.))) %>% 
#   select(term, difference, block)->block1
# 
# #Get block 2
# conservative_models_ROC %>% 
#   #Be sure to get the right block here 
#   unnest(block2) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 2", nrow(.))) %>% 
#   select(term, difference, block)->block2
# 
# #Get block 3
# conservative_models_ROC %>% 
#   unnest(block3) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 3", nrow(.))) %>% 
#   select(term, difference, block)->block3
# 
# #Get block 4
# conservative_models_ROC %>% 
#   unnest(block4) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 4", nrow(.))) %>% 
#   select(term, difference, block)->block4
# 
# #Get block 5
# conservative_models_ROC %>% 
#   unnest(block5) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 5", nrow(.))) %>% 
#   select(term, difference, block)->block5
# 
# #Get block 6
# conservative_models_ROC %>% 
#   unnest(block6) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 6", nrow(.))) %>% 
#   select(term, difference, block)->block6
# 
# #Bind them together
# block1 %>% 
#   bind_rows(., block2, block3, block4, block5, block6) %>% 
#   group_by(term)%>%
#   slice(1)%>%
#   arrange(block)->Conservative_ROC_block_difference_model
# 
# Conservative_ROC_block_difference_model %>%
#   kable()
# 
# #### Conservative QC ####
# out %>% 
#     filter(quebec==1) %>% 
#   group_by(survey) %>% 
#   nest() %>% 
#   mutate(
#     block1=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign, family="binomial", data=x)), 
#     block2=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x)),
#     block3=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x)),
#     block4=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
#     block5=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
#     block6=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))
#   )->conservative_models_QC
# conservative_models_QC %>% 
#   mutate(block1=map(block1, tidy),
#          block2=map(block2, tidy), 
#          block3=map(block3, tidy),
#          block4=map(block4, tidy),
#          block5=map(block5, tidy),
#          block6=map(block6, tidy)) ->conservative_models_QC
# 
# #Get block 1
# conservative_models_QC %>% 
#   unnest(block1) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   mutate(block=rep("block 1", nrow(.))) %>% 
#   select(term, difference, block)->block1
# 
# #Get block 2
# conservative_models_QC %>% 
#   #Be sure to get the right block here 
#   unnest(block2) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 2", nrow(.))) %>% 
#   select(term, difference, block)->block2
# 
# #Get block 3
# conservative_models_QC %>% 
#   unnest(block3) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 3", nrow(.))) %>% 
#   select(term, difference, block)->block3
# 
# #Get block 4
# conservative_models_QC %>% 
#   unnest(block4) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 4", nrow(.))) %>% 
#   select(term, difference, block)->block4
# 
# #Get block 5
# conservative_models_QC %>% 
#   unnest(block5) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 5", nrow(.))) %>% 
#   select(term, difference, block)->block5
# 
# #Get block 6
# conservative_models_QC %>% 
#   unnest(block6) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 6", nrow(.))) %>% 
#   select(term, difference, block)->block6
# 
# #Bind them together
# block1 %>% 
#   bind_rows(., block2, block3, block4, block5, block6) %>% 
#   group_by(term)%>%
#   slice(1)%>%
#   arrange(block)->Conservative_QC_block_difference_model
# 
# Conservative_QC_block_difference_model %>%
#   kable()
# 
# #### Bloc QC ####
# out %>% 
#   group_by(survey) %>% 
#   nest() %>% 
#   mutate(
#     block1=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign, family="binomial", data=x)), 
#     block2=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x)),
#     block3=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x)),
#     block4=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
#     block5=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
#     block6=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))
#   )->bloc_models_QC
# bloc__models_QC %>% 
#   mutate(block1=map(block1, tidy),
#          block2=map(block2, tidy), 
#          block3=map(block3, tidy),
#          block4=map(block4, tidy),
#          block5=map(block5, tidy),
#          block6=map(block6, tidy)) ->bloc__models_QC
# 
# #Get block 1
# bloc_models_QC %>% 
#   unnest(block1) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   mutate(block=rep("block 1", nrow(.))) %>% 
#   select(term, difference, block)->block1
# 
# #Get block 2
# bloc_models_QC %>% 
#   #Be sure to get the right block here 
#   unnest(block2) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 2", nrow(.))) %>% 
#   select(term, difference, block)->block2
# 
# #Get block 3
# bloc_models_QC %>% 
#   unnest(block3) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 3", nrow(.))) %>% 
#   select(term, difference, block)->block3
# 
# #Get block 4
# bloc_models_QC %>% 
#   unnest(block4) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 4", nrow(.))) %>% 
#   select(term, difference, block)->block4
# 
# #Get block 5
# bloc_models_QC %>% 
#   unnest(block5) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 5", nrow(.))) %>% 
#   select(term, difference, block)->block5
# 
# #Get block 6
# bloc_models_QC %>% 
#   unnest(block6) %>% 
#   #Calculate the differences
#   group_by(term) %>% 
#   mutate(difference=estimate-lag(estimate)) %>% 
#   filter(survey==1) %>% 
#   ungroup() %>% 
#   #Name the correct block here 
#   mutate(block=rep("block 6", nrow(.))) %>% 
#   select(term, difference, block)->block6
# 
# #Bind them together
# block1 %>% 
#   bind_rows(., block2, block3, block4, block5, block6) %>% 
#   group_by(term)%>%
#   slice(1)%>%
#   arrange(block)->Bloc_QC_block_difference_model
# 
# Bloc_QC_block_difference_model %>%
#   kable()
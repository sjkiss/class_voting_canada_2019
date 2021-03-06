
#File to Recode 2019 CES Data 

#recode Gender (q3)
look_for(ces19phone, "gender")
ces19phone$male<-Recode(ces19phone$q3, "1=1; 2=0; else=NA")
val_labels(ces19phone$male)<-c(Female=0, Male=1)
#checks
val_labels(ces19phone$male)
table(ces19phone$male)

#recode Union Household (p51)
look_for(ces19phone, "union")
ces19phone$union<-Recode(ces19phone$p51, "1=1; 2=0; else=NA")
val_labels(ces19phone$union)<-c(None=0, Union=1)
#checks
val_labels(ces19phone$union)
table(ces19phone$union)

#Union Combined variable (identical copy of union) ### Respondent only
ces19phone$union_both<-ces19phone$union
#checks
val_labels(ces19phone$union_both)
table(ces19phone$union_both)

#recode Education (q61)
look_for(ces19phone, "education")
ces19phone$degree<-Recode(ces19phone$q61, "9:11=1; 1:8=0; else=NA")
val_labels(ces19phone$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces19phone$degree)
table(ces19phone$degree)

#recode Region (q4)
look_for(ces19phone, "province")
ces19phone$region<-Recode(ces19phone$q4, "1:4=1; 6=2; 7:10=3; 4=NA; else=NA")
val_labels(ces19phone$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces19phone$region)
table(ces19phone$region)

#recode Quebec (q4)
look_for(ces19phone, "province")
ces19phone$quebec<-Recode(ces19phone$q4, "1:4=0; 6:10=0; 5=1; else=NA")
val_labels(ces19phone$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces19phone$quebec)
table(ces19phone$quebec)

#recode Age (age)
##Just use underlying age variable
#no missing values apparent
look_for(ces19phone, "age")
# ces19phone$age<-ces19phone$age
# summary(ces19phone$age)
# #check
# table(ces19phone$age)

#recode Religion (q62)
look_for(ces19phone, "relig")
ces19phone$religion<-Recode(ces19phone$q62, "21=0; 6=1; 8=1; 1=2; 3:4=2; 5=2; 7=2; 9=2; 12:14=2; 2=3; 5=3; 10:11=3; 15=3; 22=3; 16:20=2; 21=0; else=NA")
val_labels(ces19phone$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces19phone$religion)
table(ces19phone$religion)

#recode Language (language_CES)
look_for(ces19phone, "language")
ces19phone$language<-Recode(ces19phone$language_CES, "2=0; 1=1; else=NA")
val_labels(ces19phone$language)<-c(French=0, English=1)
#checks
val_labels(ces19phone$language)
table(ces19phone$language)

#recode Non-charter Language (q67)
look_for(ces19phone, "language")
ces19phone$non_charter_language<-Recode(ces19phone$q67, "1=0; 2:3=1; 4=0; 5:31=1; else=NA")
val_labels(ces19phone$non_charter_language)<-c(Charter=0, Non_Charter=1)
table(as_factor(ces19phone$q67),ces19phone$non_charter_language )
#checks
val_labels(ces19phone$non_charter_language)
table(ces19phone$non_charter_language)

#recode Employment (q68)
look_for(ces19phone, "employment")
ces19phone$employment<-Recode(ces19phone$q68, "3:8=0; 1:2=1; 9:11=1; else=NA")
val_labels(ces19phone$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces19phone$employment)
table(ces19phone$employment)

#recode Sector (p53 & q68)
look_for(ces19phone, "public")
ces19phone %>% 
  mutate(sector=case_when(
    p53==1 ~1,
    p53==2 ~0,
    p53==3 ~0,
    p53==4 ~0,
    q68>2 & q68< 12 ~ 0,
    p53==-9 ~NA_real_ ,
    p53==-8 ~NA_real_ ,
  ))->ces19phone

val_labels(ces19phone$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces19phone$sector)
table(ces19phone$sector)

#recode Party ID (p47)
look_for(ces19phone, "closest")
ces19phone$party_id<-Recode(ces19phone$p47, "1=1; 2=2; 3=3; 4=4; 5=0; 7=0; 6=2; else=NA")
val_labels(ces19phone$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4)
#checks
val_labels(ces19phone$party_id)
table(ces19phone$party_id)

#recode Vote (p3)
look_for(ces19phone, "party did you vote")
ces19phone$vote<-Recode(ces19phone$p3, "1=1; 2=2; 3=3; 4=4; 5=5; 7=0; 6=2; else=NA")
val_labels(ces19phone$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19phone$vote)
table(ces19phone$vote)

#recode Occupation (p52)
look_for(ces19phone, "occupation")

ces19phone$occupation<-Recode(as.numeric(ces19phone$NOC), "0:1099=2; 
1100:1199=1;
2100:2199=1; 
 3000:3199=1;
 4000:4099=1; 
 4100:4199=1;
 5100:5199=1;
 1200:1599=3; 
 2200:2299=3;
 3200:3299=3;
 3400:3500=3; 
 4200:4499=3;
 5200:5299=3;
 6200:6399=3;
 6400:6799=3; 7200:7399=4; 
 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")

val_labels(ces19phone$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)

#checks
val_labels(ces19phone$occupation)
table(ces19phone$occupation)
ces19phone %>% 
  filter(is.na(NOC)==F&is.na(occupation)==T) %>% 
  select(NOC, occupation)

#recode Occupation3 as 6 class schema with self-employed (q68)
look_for(ces19phone, "employ")
ces19phone$occupation3<-ifelse(ces19phone$q68==3, 6, ces19phone$occupation)
val_labels(ces19phone$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces19phone$occupation3)
table(ces19phone$occupation3)

#recode Income (q70r)
look_for(ces19phone, "income")
ces19phone$income<-Recode(ces19phone$q70r, "1:2=1; 3=2; 4=3; 5:6=4; 7:8=5; else=NA")
val_labels(ces19phone$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces19phone$income)
table(ces19phone$income)

#recode Community Size (p57)
look_for(ces19phone, "live")
ces19phone$size<-Recode(ces19phone$p57, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
val_labels(ces19phone$size)<-c(Rural=1, Under_10K=2, Under_100K=3, Under_500K=4, City=5)
#checks
val_labels(ces19phone$size)
table(ces19phone$size)

#recode Native-born (q64)
ces19phone$q64
ces19phone$native<-Recode(ces19phone$q64, "1:2=1; 3:13=0; else=NA")
val_labels(ces19phone$native)<-c(Foreign=0, Native=1)

#checks
val_labels(ces19phone$native)
table(ces19phone$native)

#recode Ideology (p42)
look_for(ces19phone, "scale")
ces19phone$ideology<-Recode(ces19phone$p42, "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA")
val_labels(ces19phone$ideology)<-c(Left=0, Right=1)
#checks
val_labels(ces19phone$ideology)
table(ces19phone$ideology)

#recode Immigration sentiment (p22_a, p22_b, p22_c, q39) into an index 0-1
#1 = pro-immigration sentiment 0 = anti-immigration sentiment
look_for(ces19phone, "immigr")
ces19phone$immigration_economy<-Recode(as.numeric(ces19phone$p22_a), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$immigration_economy)<-c(Negative=0, Somewhat_negative=0.25, Neither=0.5, Somewhat_positive=0.75, Positive=1)
#checks
#val_labels(ces19phone$immigration_economy)
table(ces19phone$immigration_economy)

ces19phone$immigration_culture<-Recode(as.numeric(ces19phone$p22_b), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$immigration_culture)<-c(Negative=0, Somewhat_negative=0.25, Neither=0.5, Somewhat_positive=0.75, Positive=1)
#checks
#val_labels(ces19phone$immigration_culture)
table(ces19phone$immigration_culture)

ces19phone$immigration_crime<-Recode(as.numeric(ces19phone$p22_c), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$immigration_crime)<-c(Negative=0, Somewhat_negative=0.25, Neither=0.5, Somewhat_positive=0.75, Positive=1)
#checks
#val_labels(ces19phone$immigration_crime)
table(ces19phone$immigration_crime)

ces19phone$immigration_rate<-Recode(as.numeric(ces19phone$q39), "1=1; 2=0; 3=0.5; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$immigration_rate)<-c(Less=0, Same=0.5, More=1)
#checks
#val_labels(ces19phone$immigration_rate)
table(ces19phone$immigration_rate)

#recode Racial Minorities sentiment (p21_a)
#1 = pro-racial minority sentiment 0 = anti-racial minority sentiment
look_for(ces19phone, "minor")
ces19phone$minorities_culture<-Recode(as.numeric(ces19phone$p21_a), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$minorities_culture)<-c(Negative=0, Somewhat_negative=0.25, Neither=0.5, Somewhat_positive=0.75, Positive=1)
#checks
#val_labels(ces19phone$minorities_culture)
#table(ces19phone$minorities_culture)

ces19phone$minorities_help<-Recode(as.numeric(ces19phone$p35_a), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)

ces19phone %>% 
  rowwise() %>% 
  mutate(minorities=mean(c_across(c(minorities_culture, minorities_help)), na.rm=T))->ces19phone
#checks
#val_labels(ces19phone$minorities_help)
#table(ces19phone$minorities_help)

#Combine the 4 immigration variables and divide by 4
ces19phone %>% 
  rowwise() %>% 
  mutate(immigration=mean(c_across(c(immigration_economy, immigration_culture, immigration_crime, immigration_rate)), na.rm=T))->ces19phone

#Check distribution of immigration
qplot(ces19phone$immigration, geom="histogram")

#Calculate Cronbach's alpha
library(psych)

ces19phone %>% 
  select(immigration_economy, immigration_culture, immigration_crime, immigration_rate) %>% 
  alpha(.)

## Or Create a 5 variable immigration/racial minority sentiment index by dividing by 5
ces19phone %>% 
  rowwise() %>% 
  mutate(immigration2=mean(c_across(c(immigration_economy, immigration_culture, immigration_crime, immigration_rate, minorities_help))))->ces19phone


#Calculate Cronbach's alpha
library(psych)

ces19phone %>% 
  select(immigration_economy, immigration_culture, immigration_crime, immigration_rate, minorities_help) %>% 
  alpha(.)

#recode Previous Vote (q60)
look_for(ces19phone, "party did you vote")
ces19phone$past_vote<-Recode(ces19phone$q60, "1=1; 2=2; 3=3; 4=4; 5=5; 7=0; else=NA")
val_labels(ces19phone$past_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19phone$past_vote)
table(ces19phone$past_vote)

#recode Jagmeet Singh (q22)
look_for(ces19phone, "Singh")
ces19phone$Singh<-Recode(ces19phone$q22, "-6=NA; -8=NA; -9=NA")
#checks
table(ces19phone$Singh)
ces19phone$Jagmeet_Singh<-(ces19phone$Singh /100)
table(ces19phone$Jagmeet_Singh)

#recode Justin Trudeau (q20)
look_for(ces19phone, "Trudeau")
ces19phone$Trudeau<-Recode(ces19phone$q20, "-6=NA; -8=NA; -9=NA")
#checks
table(ces19phone$Trudeau)
ces19phone$Justin_Trudeau<-(ces19phone$Trudeau /100)
table(ces19phone$Justin_Trudeau)

#recode Andrew Scheer (q21)
look_for(ces19phone, "Scheer")
ces19phone$Scheer<-Recode(ces19phone$q21, "-6=NA; -8=NA; -9=NA")
#checks
table(ces19phone$Scheer)
ces19phone$Andrew_Scheer<-(ces19phone$Scheer /100)
table(ces19phone$Andrew_Scheer)

#recode Francois Blanchet (q23)
look_for(ces19phone, "Blanchet")
ces19phone$Blanchet<-Recode(ces19phone$q23, "-6=NA; -8=NA; -9=NA")
#checks
table(ces19phone$Blanchet)
ces19phone$Francois_Blanchet<-(ces19phone$Blanchet /100)
table(ces19phone$Francois_Blanchet)

#recode leaders including don't knows
#recode Jagmeet Singh (q22)
ces19phone$Singh19<-Recode(ces19phone$q22, "-6=50; -8=NA; -9=50")
#checks
table(ces19phone$Singh19)
ces19phone$ndp_leader<-(ces19phone$Singh19 /100)
table(ces19phone$ndp_leader)

#recode Justin Trudeau (q20)
ces19phone$Trudeau19<-Recode(ces19phone$q20, "-6=50; -8=NA; -9=50")
#checks
table(ces19phone$Trudeau19)
ces19phone$liberal_leader<-(ces19phone$Trudeau19 /100)
table(ces19phone$liberal_leader)

#recode Andrew Scheer (q21)
ces19phone$Scheer19<-Recode(ces19phone$q21, "-6=50; -8=NA; -9=50")
#checks
table(ces19phone$Scheer19)
ces19phone$conservative_leader<-(ces19phone$Scheer19 /100)
table(ces19phone$conservative_leader)

#recode Francois Blanchet (q23)
ces19phone$Blanchet19<-Recode(ces19phone$q23, "-6=50; -8=NA; -9=50")
#checks
table(ces19phone$Blanchet19)
ces19phone$bloc_leader<-(ces19phone$Blanchet19 /100)
table(ces19phone$bloc_leader)


#recode Environment (q27_b)
look_for(ces19phone, "enviro")
ces19phone$environment<-Recode(ces19phone$q27_b, "3=0.5; 1=1; 2=0; -9=0.5; else=NA")
val_labels(ces19phone$environment)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
val_labels(ces19phone$environment)
table(ces19phone$environment)

#recode Age2 (0-1 variable)
ces19phone$age2<-(ces19phone$age /100)
#checks
table(ces19phone$age2)

#recode Redistribution (p44)
look_for(ces19phone, "rich")
ces19phone$redistribution<-Recode(ces19phone$p44, "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$redistribution)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
val_labels(ces19phone$redistribution)
table(ces19phone$redistribution)

#recode Pro-Redistribution (p44)
ces19phone$pro_redistribution<-Recode(ces19phone$p44, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces19phone$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces19phone$pro_redistribution)
table(ces19phone$pro_redistribution)

#recode NDP rating (q16)
look_for(ces19phone, "NDP")
ces19phone$NDP_therm<-Recode(ces19phone$q16, "-6=NA; -8=NA; -9=NA")
#checks
table(ces19phone$NDP_therm)
ces19phone$NDP_rating<-(ces19phone$NDP_therm /100)
table(ces19phone$NDP_rating)

#recode Liberal rating (q14)
look_for(ces19phone, "Liberal")
ces19phone$Liberal_therm<-Recode(ces19phone$q14, "996=NA; 998=NA; 999=NA")
#checks
table(ces19phone$Liberal_therm)
ces19phone$Liberal_rating<-(ces19phone$Liberal_therm /100)
table(ces19phone$Liberal_rating)

#recode Conservative rating (q15)
look_for(ces19phone, "Conservative")
ces19phone$Conservative_therm<-Recode(ces19phone$q15, "996=NA; 998=NA; 999=NA")
#checks
table(ces19phone$Conservative_therm)
ces19phone$Conservative_rating<-(ces19phone$Conservative_therm /100)
table(ces19phone$Conservative_rating)

#recode Manage economy (q33)
look_for(ces19phone, "economy")
ces19phone$q33

ces19phone$manage_economy<-Recode(ces19phone$q33, "1=1; 2=2; 3=3; 4=4; 5=5; 7=NA; 6=2; else=NA")
val_labels(ces19phone$manage_economy)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19phone$manage_economy)
table(ces19phone$manage_economy)

#recode Manage environment (q34)
look_for(ces19phone, "environment")

ces19phone$manage_environment<-Recode(ces19phone$q34, "1=1; 2=2; 3=3; 4=4; 5=5; 7=NA; 6=2; else=NA")
val_labels(ces19phone$manage_environment)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19phone$manage_environment)
table(ces19phone$manage_environment)

#recode Addressing Main Issue (q8)
look_for(ces19phone, "issue")
ces19phone$address_issue<-Recode(ces19phone$q8, "1=1; 2=2; 3=3; 4=4; 5=5; 7=NA; 6=2; else=NA")
val_labels(ces19phone$address_issue)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19phone$address_issue)
table(ces19phone$address_issue)

#recode Market Liberalism (p20_a and p20_f)
look_for(ces19phone, "leave")
look_for(ces19phone, "blame")

ces19phone$market1<-Recode(ces19phone$p20_a, "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
ces19phone$market2<-Recode(ces19phone$p20_f, "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$market1)<-c(Strongly_disagree=0, Somewhat_disagree=0.25, Neither=0.5, Somewhat_agree=0.75, Strongly_agree=1)
#checks
table(ces19phone$market1)
table(ces19phone$market2)

ces19phone %>% 
  rowwise() %>% 
  mutate(market_liberalism=mean(
    c_across(market1:market2)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('market1', 'market2', 'market_liberalism')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<3)
#Scale Averaging 
ces19phone %>% 
  rowwise() %>% 
  mutate(market_liberalism=mean(
    c_across(c('market1', 'market2')), na.rm=T  
  )) %>% 
  ungroup()->ces19phone

ces19phone %>% 
  select(starts_with("market")) %>% 
  summary()
#Check distribution of market_liberalism
qplot(ces19phone$market_liberalism, geom="histogram")
table(ces19phone$market_liberalism, useNA="ifany")

#Calculate Cronbach's alpha
ces19phone %>% 
  select(market1, market2) %>% 
  alpha(.)
#For some reason the cronbach's alpha doesn't work here. 
#Check correlation
ces19phone %>% 
  select(market1, market2) %>% 
  cor(., use="complete.obs")
#Weakly correlated, but correlated

#recode Moral Traditionalism (p35_b, p20_e,  p35_c)
look_for(ces19phone, "women")
look_for(ces19phone, "gays")
ces19phone$moral1<-Recode(ces19phone$p20_e, "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
ces19phone$moral2<-Recode(ces19phone$p35_b, "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; -9=0.5; else=NA", as.numeric=T)
ces19phone$moral3<-Recode(as.numeric(ces19phone$p35_c), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$moral1)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
table(ces19phone$moral1)
table(ces19phone$moral2)
table(ces19phone$moral3)
ces19phone %>% 
  names()
ces19phone %>% 
  rowwise() %>% 
  mutate(moral_traditionalism=mean(
    c_across(moral1:moral3)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('moral1', 'moral2', 'moral3', 'moral_traditionalism')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<4)
#Scale Averaging 
ces19phone %>% 
  rowwise() %>% 
  mutate(moral_traditionalism=mean(
    c_across(c('moral1', 'moral2', 'moral3')), na.rm=T  
  )) %>% 
  ungroup()->ces19phone
ces19phone %>% 
  select(starts_with("moral")) %>% 
  summary()
#Check distribution of moral_traditionalism
qplot(ces19phone$moral_traditionalism, geom="histogram")
table(ces19phone$moral_traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
ces19phone %>% 
  select(moral1, moral2, moral3) %>% 
  alpha(.)

#recode Political Disaffection (p20_i and p20_n)
look_for(ces19phone, "politicians")
ces19phone$disaffection1<-Recode(ces19phone$p20_i, "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
ces19phone$disaffection2<-Recode(ces19phone$p20_n, "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$disaffection1)<-c(Strongly_disagree=0, Somewhat_disagree=0.25, Neither=0.5, Somewhat_agree=0.75, Strongly_agree=1)
#checks
table(ces19phone$disaffection1)
table(ces19phone$disaffection2)

ces19phone %>% 
  rowwise() %>% 
  mutate(political_disaffection=mean(
    c_across(disaffection1:disaffection2)
    , na.rm=T )) -> out
out %>% 
  ungroup() %>% 
  select(c('disaffection1', 'disaffection2', 'political_disaffection')) %>% 
  mutate(na=rowSums(is.na(.))) %>% 
  filter(na>0, na<3)
#Scale Averaging 
ces19phone %>% 
  rowwise() %>% 
  mutate(political_disaffection=mean(
    c_across(c('disaffection1', 'disaffection2')), na.rm=T  
  )) %>% 
  ungroup()->ces19phone
ces19phone %>% 
  select(starts_with("political_disaffection")) %>% 
  summary()
#Check distribution of disaffection
qplot(ces19phone$political_disaffection, geom="histogram")
table(ces19phone$political_disaffection, useNA="ifany")



#recode Continentalism (p43)
look_for(ces19phone, "united states")
ces19phone$continentalism<-Recode(ces19phone$p43, "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; 6=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$continentalism)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
val_labels(ces19phone$continentalism)
table(ces19phone$continentalism)

#recode Quebec Sovereignty (q43)
look_for(ces19phone, "quebec")
ces19phone$quebec_sovereignty<-Recode(ces19phone$q43, "1=1; 2=0.75; -9=0.5; 3=0.25; 4=0; else=NA", as.numeric=T)
#val_labels(ces19phone$quebec_sovereignty)<-c(Much_less=0, Somewhat_less=0.25, Dont_know=0.5, Somewhat_more=0.75, Much_more=1)
#checks
val_labels(ces19phone$quebec_sovereignty)
table(ces19phone$quebec_sovereignty)

#recode Personal Retrospective (q47)
look_for(ces19phone, "situation")
ces19phone$personal_retrospective<-Recode(ces19phone$q47, "1=1; 2=0; 3=0.5; -9=0.5; else=NA", as.numeric=T)
val_labels(ces19phone$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces19phone$personal_retrospective)
table(ces19phone$personal_retrospective)

#recode National Retrospective (q31)
look_for(ces19phone, "economy")
ces19phone$national_retrospective<-Recode(ces19phone$q31, "1=1; 2=0; 3=0.5; -9=0.5; else=NA", as.numeric=T)
val_labels(ces19phone$national_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces19phone$national_retrospective)
table(ces19phone$national_retrospective)

#recode Defence (q27_d)
look_for(ces19phone, "defence")
ces19phone$defence<-Recode(ces19phone$q27_d, "3=0.5; 1=1; 2=0; -9=0.5; else=NA")
val_labels(ces19phone$defence)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
val_labels(ces19phone$defence)
table(ces19phone$defence)

#recode Crime and Justice (q27_c)
look_for(ces19phone, "justice")
ces19phone$justice<-Recode(ces19phone$q27_c, "3=0.5; 1=1; 2=0; -9=0.5; else=NA")
val_labels(ces19phone$justice)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
val_labels(ces19phone$justice)
table(ces19phone$justice)

#recode Education (q27_a)
look_for(ces19phone, "education")
ces19phone$education<-Recode(ces19phone$q27_a, "3=0.5; 1=1; 2=0; -9=0.5; else=NA")
val_labels(ces19phone$education)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
val_labels(ces19phone$education)
table(ces19phone$education)


#### mip ####
#In another package I have turned added the MIP recodes to the level of the 2015 Recodes 

#The original response is ces19phone$q7
ces19phone$q7
#The lowered case response is 
ces19phone$q7_lower
#The variable that includes the recodes contained the Excel file is
ces19phone$q7_out
table(as_factor(ces19phone$q7_out))

#recode Most Important Question (q7_out)
ces19phone$mip<-Recode(ces19phone$q7_out, "75=1; 71=2; 77=2; 18=2; 5=3; 2=3; 90:91=3; 65:66=4; 13=5; 39=5; 10=6;  
                                          36=7; 15:16=7; 30=7; 29=7; 56:57=8; 14=9; 50=9; 20:26=10; 7=11; 83=11;  
                                          48=12; 79=12; 34=13; 55=14; 73:74=14; 76=14; 49=14; 60:64=15; 72=15; 
                                          80:82=0; 84=0; 92:97=0; 6=0; 8=0; 46=0; 31:33=0; 58:59=0; 35=0; 1=0; else=NA")
val_labels(ces19phone$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, 
                              Deficit_Debt=10, Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15)

table(ces19phone$mip, useNA="ifany")

#convert q7 to q7_out
# ces19phone$q7_out<-tolower(ces19phone$q7)
# #### Most important Problem for Environment ####
# #convert mip to mip_cat
# ces19phone$q7_out
# 
# ces19phone %>% 
#   mutate(mip=case_when(
#     str_detect(q7_out, "climat") ~ 1,
#         str_detect(q7_out, "environ") ~ 1,
#     str_detect(mip, "ecologie") ~1,
#     str_detect(q7_out, "health") ~ 8,
#        str_detect(q7_out, "tax") ~ 9,
#            str_detect(q7_out, "impots") ~ 9,
#                str_detect(q7_out, "santé") ~ 8,
#                    str_detect(q7_out, "crim[ei]") ~ 2,
#                        str_detect(q7_out, "job") ~ 6,
#                            str_detect(q7_out, "deficit") ~ 10,
#                                str_detect(q7_out, "debt") ~ 10,
#                                    str_detect(q7_out, "trudeau") ~ 0,
#                                    str_detect(q7_out, "ethic") ~ 3,
#                                        str_detect(q7_out, "immigr") ~ 13,
#                                            str_detect(q7_out, "pipeline") ~ 6,
#                                                str_detect(q7_out, "ecologie") ~ 1,
#                                                      str_detect(q7_out, "economy") ~ 6,
#                                                    str_detect(q7_out, "économie") ~ 6,
#                                                        str_detect(q7_out, "emploi") ~ 6,
#                                                               str_detect(q7_out, "oil") ~ 5,
#                                                               str_detect(q7_out, "gas") ~ 5,
#                                                                   str_detect(q7_out, "nuclear") ~ 5,
#     TRUE ~ NA_real_
#   )) ->ces19phone
# ces19phone$mip
# 
# val_labels(ces19phone$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs_Economy=6, Economy=7, Health=8, Taxes=9, 
#                               Deficit_Debt=10, Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Pipeline=20)
# 
# ces19phone %>% 
#   mutate(mip_enviro=case_when(
#     mip==1~1,
#     TRUE~0
#   ))->ces19phone
# table(as_factor(ces19phone$mip))
# Recode visible minority
source(here('R_Scripts/2_14_ces_19_vismin_recode.R'))


  
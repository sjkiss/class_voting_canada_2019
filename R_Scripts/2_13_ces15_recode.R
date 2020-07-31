#File to Recode 2015 CES Data 
#load data
data("ces15phone")

#recode Gender (RGENDER)
look_for(ces15phone, "gender")
ces15phone$male<-Recode(ces15phone$RGENDER, "1=1; 5=0")
val_labels(ces15phone$male)<-c(Female=0, Male=1)
#checks
val_labels(ces15phone$male)
table(ces15phone$male)

#recode Union Respondent (PES15_93)
look_for(ces15phone, "union")
ces15phone$union<-Recode(ces15phone$PES15_93, "1=1; 5=0; else=NA")
val_labels(ces15phone$union)<-c(None=0, Union=1)
#checks
val_labels(ces15phone$union)
table(ces15phone$union)

#recode Union Combined (PES15_93 and PES15_94)
ces15phone %>% 
  mutate(union_both=case_when(
    PES15_93==1 | PES15_94==1 ~ 1,
    PES15_93==5 & PES15_94==5 ~ 0,
    PES15_93==8 & PES15_94==8 ~ NA_real_,
    PES15_93==9 & PES15_94==9 ~ NA_real_,
    TRUE ~ 0,
  ))->ces15phone

val_labels(ces15phone$union_both)<-c(None=0, Union=1)
#checks
val_labels(ces15phone$union_both)
table(ces15phone$union_both, useNA="ifany")
table(ces15phone$PES15_93, ces15phone$union_both, useNA="ifany")
table(ces15phone$PES15_93, ces15phone$PES15_94, useNA="ifany")
table(ces15phone$union_both)
table(ces15phone$PES15_93, ces15phone$union_both, useNA="ifany")
table(ces15phone$PES15_94, ces15phone$union_both, useNA="ifany")

#recode Education (CPS15_79)
look_for(ces15phone, "education")
ces15phone$degree<-Recode(ces15phone$CPS15_79, "9:11=1; 1:8=0; else=NA")
val_labels(ces15phone$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces15phone$degree)
table(ces15phone$degree)

#recode Region (CPS15_PROVINCE)
look_for(ces15phone, "province")
ces15phone$region<-Recode(ces15phone$CPS15_PROVINCE, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces15phone$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces15phone$region)
table(ces15phone$region)

#recode Quebec (CPS15_PROVINCE)
look_for(ces15phone, "province")
ces15phone$quebec<-Recode(ces15phone$CPS15_PROVINCE, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces15phone$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces15phone$quebec)
table(ces15phone$quebec)

#recode Age (CPS15_78)
look_for(ces15phone, "age")
ces15phone$yob<-Recode(ces15phone$CPS15_78, "9998:9999=NA")
ces15phone$age<-2015-ces15phone$yob
#check
table(ces15phone$age)

#recode Religion (CPS15_80)
look_for(ces15phone, "relig")
ces15phone$religion<-Recode(ces15phone$CPS15_80, "0=0; 1:2=2; 4:5=1; 7=2; 9:10=2; 12:14=2; 16:20=2; 98:99=NA; 3=3; 6=3; 8=3; 11=3; 15=3; 97=3;")
val_labels(ces15phone$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces15phone$religion)
table(ces15phone$religion)

#recode Language (CPS15_INTLANG)
look_for(ces15phone, "language")
ces15phone$language<-Recode(ces15phone$CPS15_INTLANG, "5=0; 1=1; else=NA")
val_labels(ces15phone$language)<-c(French=0, English=1)
#checks
val_labels(ces15phone$language)
table(ces15phone$language)

#recode Non-charter Language (CPS15_90)
look_for(ces15phone, "language")
ces15phone$non_charter_language<-Recode(ces15phone$CPS15_90, "1:5=0; 8:64=1; 65=0; 95:97=1; else=NA")
val_labels(ces15phone$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces15phone$non_charter_language)
table(ces15phone$non_charter_language)

#recode Employment (CPS15_91)
look_for(ces15phone, "employment")
ces15phone$employment<-Recode(ces15phone$CPS15_91, "3:7=0; 1:2=1; 8:11=1; else=NA")
val_labels(ces15phone$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces15phone$employment)
table(ces15phone$employment)

#recode Sector (PES15_92 & CPS15_91)
look_for(ces15phone, "company")
look_for(ces15phone, "private")
ces15phone %>% 
  mutate(sector=case_when(
    PES15_92==5 ~1,
    PES15_92==1 ~0,
    PES15_92==0 ~0,
    CPS15_91==1 ~0,
    CPS15_91>2 & CPS15_91< 12 ~ 0,
    PES15_92==9 ~NA_real_ ,
    PES15_92==8 ~NA_real_ ,
  ))->ces15phone

val_labels(ces15phone$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces15phone$sector)
table(ces15phone$sector)

#recode Party ID (PES15_59a)
look_for(ces15phone, "identify")
ces15phone$party_id<-Recode(ces15phone$PES15_59a, "1=1; 2=2; 3=3; 4:6=0; 0=0; else=NA")
val_labels(ces15phone$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces15phone$party_id)
table(ces15phone$party_id)

#recode Vote (PES15_6)
look_for(ces15phone, "party did you vote")
ces15phone$vote<-Recode(ces15phone$PES15_6, "1=1; 2=2; 3=3; 4=4; 5=5; 0=0; else=NA")
val_labels(ces15phone$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces15phone$vote)
table(ces15phone$vote)

#recode Occupation (PES15_NOC)
look_for(ces15phone, "occupation")
ces15phone$occupation<-Recode(as.numeric(ces15phone$PES15_NOC), "0:1099=2; 
1100:1199=1;
2100:2199=1; 
 3000:3199=1;
 4000:4099=1; 
 4100:4199=1;
 5100:5199=1;
 1200:1599=3; 
 2200:2999=3;
 3200:3299=3;
 3400:3500=3; 
 4200:4499=3;
 5200:5999=3;
 6200:6399=3;
 6400:6799=3; 
 7200:7399=4; 
 7400:7700=5; 
 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
val_labels(ces15phone$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces15phone$occupation)
table(ces15phone$occupation)
#Count missing values
ces15phone %>% 
  select(PES15_NOC, occupation) %>% 
  group_by(occupation) %>% 
  filter(is.na(occupation)) %>% 
  count(PES15_NOC)

ces15phone %>% 
  select(occupation, CPS15_91) %>% 
  filter(is.na(occupation)) %>% 
  group_by(occupation, CPS15_91) %>% 
  count()
#Show occupations of those missing responses on NOC variable
ces15phone %>% 
  select(PES15_NOC, occupation) %>% 
  group_by(occupation) %>% 
  summarise(n=n())

#recode Occupation3 as 6 class schema with self-employed (CPS15_91)
look_for(ces15phone, "employ")
ces15phone$occupation3<-ifelse(ces15phone$CPS15_91==1, 6, ces15phone$occupation)
val_labels(ces15phone$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces15phone$occupation3)
table(ces15phone$occupation3)
table(is.na(ces15phone$occupation3))
table(is.na(ces15phone$occupation))

#recode Income (cpsm16 and cpsm16a)
look_for(ces15phone, "income")
ces15phone %>% 
  mutate(income=case_when(
    CPS15_93==1 | CPS15_92> -1 & CPS15_92 < 30 ~ 1,
    CPS15_93==2 | CPS15_92> 29 & CPS15_92 < 60 ~ 2,
    CPS15_93==3 | CPS15_92> 59 & CPS15_92 < 90 ~ 3,
    CPS15_93==4 | CPS15_92> 89 & CPS15_92 < 110 ~ 4,
    CPS15_93==5 | CPS15_92> 109 & CPS15_92 < 998 ~ 5,
  ))->ces15phone

val_labels(ces15phone$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces15phone$income)
table(ces15phone$income)

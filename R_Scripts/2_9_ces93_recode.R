#File to Recode 1993 CES Data 
#load data
data("ces93")

#recode Gender (CPSRGEN)
look_for(ces93, "gender")
ces93$male<-Recode(ces93$CPSRGEN, "1=1; 5=0")
val_labels(ces93$male)<-c(Female=0, Male=1)
#checks
val_labels(ces93$male)
table(ces93$male)

#recode Union Household (CPSJOB6)
look_for(ces93, "union")
ces93$union<-Recode(ces93$CPSJOB6, "1=1; 5=0; else=NA")
val_labels(ces93$union)<-c(None=0, Union=1)
#checks
val_labels(ces93$union)
table(ces93$union)

#Union Combined variable (identical copy of union)
ces93$union_both<-ces93$union
#checks
val_labels(ces93$union_both)
table(ces93$union_both)

#recode Education (CPSO3)
look_for(ces93, "education")
ces93 %>% 
  mutate(degree=case_when(
    RTYPE4==1 & (CPSO3==9 | REFN2==9)~ 1,
    RTYPE4==1 & (CPSO3==10 | REFN2==10)~ 1,
    RTYPE4==1 & (CPSO3==11 | REFN2==11)~ 1,
    RTYPE4==1 & (CPSO3==1 | REFN2==1)~ 0,
    RTYPE4==1 & (CPSO3==2 | REFN2==2)~ 0,
    RTYPE4==1 & (CPSO3==3 | REFN2==3)~ 0,
    RTYPE4==1 & (CPSO3==4 | REFN2==4)~ 0,
    RTYPE4==1 & (CPSO3==5 | REFN2==5)~ 0,
    RTYPE4==1 & (CPSO3==6 | REFN2==6)~ 0,
    RTYPE4==1 & (CPSO3==7 | REFN2==7)~ 0,
    RTYPE4==1 & (CPSO3==8 | REFN2==8)~ 0,
  ))->ces93

val_labels(ces93$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces93$degree)
table(ces93$degree)

#recode Region (CPSPROV)
look_for(ces93, "province")
ces93$region<-Recode(ces93$CPSPROV, "0:3=1; 5=2; 6:9=3; 4=NA")
val_labels(ces93$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces93$region)
table(ces93$region)

#recode Quebec (CPSPROV)
look_for(ces93, "province")
ces93$quebec<-Recode(ces93$CPSPROV, "0:3=0; 5:9=0; 4=1")
val_labels(ces93$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces93$quebec)
table(ces93$quebec)

#recode Age (CPSAGE)
look_for(ces93, "age")
ces93$yob<-Recode(ces93$CPSAGE, "9997:9999=NA")
ces93$age<-1993-ces93$yob
#check
table(ces93$age)

#recode Religion (CPSO9)
look_for(ces93, "relig")
ces93$religion<-Recode(ces93$CPSO9, "97=0; 2=1; 1=2; 3:5=3; else=NA")
val_labels(ces93$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces93$religion)
table(ces93$religion)

#recode Language (PESLANG)
look_for(ces93, "language")
ces93$language<-Recode(ces93$PESLANG, "'E'=1; 'F'=0; else=NA")
val_labels(ces93$language)<-c(French=0, English=1)
#checks
val_labels(ces93$language)
table(ces93$language)

#recode Non-charter Language (CPSO16 and REFN16)
look_for(ces93, "language")
ces93 %>% 
  mutate(non_charter_language=case_when(
    RTYPE4==1 & (CPSO15==5 | REFN16==5)~ 1,
    RTYPE4==1 & (CPSO15==3 | REFN16==3)~ 0,
    RTYPE4==1 & (CPSO15==1 | REFN16==1)~ 0,
  ))->ces93

val_labels(ces93$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces93$non_charter_language)
table(ces93$non_charter_language)

#recode Employment (CPSJOB1)
look_for(ces93, "employment")
ces93$employment<-Recode(ces93$CPSJOB1, "2:7=0; 1=1; else=NA")
val_labels(ces93$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces93$employment)
table(ces93$employment)

#recode Sector (CPSJOB5 & CPSJOB1)
look_for(ces93, "sector")
ces93 %>% 
  mutate(sector=case_when(
    CPSJOB5==3 ~1,
    CPSJOB5==5 ~1,
    CPSJOB5==7 ~1,
    CPSJOB5==1 ~0,
    CPSJOB1> 1 & CPSJOB1< 8 ~ 0,
    CPSJOB5==9 ~NA_real_ ,
    CPSJOB5==8 ~NA_real_ ,
  ))->ces93

val_labels(ces93$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces93$sector)
table(ces93$sector)

#recode Party ID (PESL1)
look_for(ces93, "identification")
ces93$party_id<-Recode(ces93$PESL1, "1=1; 2=2; 3=3; 4=2; 5:6=0; else=NA")
val_labels(ces93$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces93$party_id)
table(ces93$party_id)

#recode Vote (PESA4)
look_for(ces93, "vote")
ces93$vote<-Recode(ces93$PESA4, "1=2; 2=1; 3=3; 5=4; 4=2; else=NA")
val_labels(ces93$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces93$vote)
table(ces93$vote)

#recode Occupation (CPSPINPR)
look_for(ces93, "occupation")
look_for(ces93, "pinporr")
ces93$occupation<-Recode(ces93$CPSPINPR, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
val_labels(ces93$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces93$occupation)
table(ces93$occupation)

#recode Occupation3 as 6 class schema with self-employed (REFN5)
look_for(ces93, "employ")
ces93$CPSJOB3
ces93$CPSJOB1
ces93$occupation3<-ifelse(ces93$CPSJOB3==1, 6, ces93$occupation)
val_labels(ces93$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces93$occupation3)
table(ces93$occupation3)

#recode Income (CPSO18 and CPSO18A)
look_for(ces93, "income")
ces93 %>% 
  mutate(income=case_when(
    CPSO18A==1 | CPSO18> 0 & CPSO18 < 20 ~ 1,
    CPSO18A==2 | CPSO18> 19 & CPSO18 < 30 ~ 2,
    CPSO18A==3 | CPSO18> 29 & CPSO18 < 50 ~ 3,
    CPSO18A==4 | CPSO18> 29 & CPSO18 < 50 ~ 3,
    CPSO18A==5 | CPSO18> 49 & CPSO18 < 70 ~ 4,
    CPSO18A==6 | CPSO18> 49 & CPSO18 < 70 ~ 4,
    CPSO18A==7 | CPSO18> 69 & CPSO18 < 998 ~ 5,
    CPSO18A==8 | CPSO18> 69 & CPSO18 < 998 ~ 5,
    CPSO18A==9 | CPSO18> 69 & CPSO18 < 998 ~ 5,
    CPSO18A==10 | CPSO18> 69 & CPSO18 < 998 ~ 5,
  ))->ces93

val_labels(ces93$income)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces93$income)
table(ces93$income)

#recode Redistribution (MBSA8)
look_for(ces93, "rich")
val_labels(ces93$MBSA8)
ces93$redistribution<-Recode(ces93$MBSA8, "; 1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces93$redistribution)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces93$redistribution)
table(ces93$redistribution)

#recode Pro-Redistribution (MBSA8)
ces93$pro_redistribution<-Recode(ces93$MBSA8, "1:2=1; 3:4=0; else=NA", as.numeric=T)
val_labels(ces93$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces93$pro_redistribution)
table(ces93$pro_redistribution)

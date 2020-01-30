#this is how we will recode variables.
#basic principles
#1. The point is to get to the point where we can run regressions of various types (i.e. likely logistic or multinomial regression) over time
#2. We should be envisioning principles of communicating covariaes clearly in regression tables. A key here is recoding variables into meaningful names, i.e. sex should be recoded into a dichotomous variable with the name of "male" with 1 for those who are male and 0 for those who are not.
#3. the variable names we choose must be consistent from election to election to enable data merging.
#4. I think we should be trying to scale all variables from 0 to 1 wherever possible (excepting the vote variable which will require it's own treatment. )

#load an elecion data file.
library(cesdata)
data("ces88")

#load libraries necessary; if you don't have these installed, please uncomment the followin glines and and excecute
#install.packages(c('tidyverse', 'labelled', 'car'))

library(tidyverse)
library(labelled)
library(car)

#look_for gender
look_for(ces88, "sex")

#Do a check on the different responses. This step probably needs to be completed with an examination of the documentaiton to figure out what is going on.

ces88$zrsex
ces88$rsex
ces88$xrsex
#It is not clear to me what the difference between zrsex, rsex and xrsex is.This probably needs to be cleared up with a look in the technical documentation and this is the real value add of your work and time. Thank you!!

#For now, we will recode rsex

ces88$rsex
#I think we will use the Recode() command in the car library.
#If you do not have this library installed, please uncomment the following package and run
#install.packages(car)
#load the library
library(car)
#Recode(variable_name, "recodes_go_here ; separated by semi-colon")
#note how we are storing the results in a new variable ces88$male
ces88$male<-Recode(ces88$rsex, "1=1; 5=0")
#Check the value labels for each variable
val_labels(ces88$male)
#see the documentation for value labels
?val_labels
#We have to add a value label for 0
val_label(ces88$male, 0)<-'female'
#check the value labels
val_labels(ces88$male)
#Now remove the value label for 5
val_labels(ces88$male, 5)<-NULL

look_for(ces88, "free trade")
#Recoding education to degree
look_for(ces88, 'school')
look_for(ces88, "education")
#check the variable
ces88$n3
#So, 9 to 11 coded as a degree; 1-98 =0 and 99=missing
#You can always try before you buy
Recode(ces88$n3, "1:8=0; 98=0; 9:11=1; 99=NA")
#Then store the variable in the new variable
ces88$degree<-Recode(ces88$n3, "1:8=0; 98=0; 9:11=1; 99=NA")
#chekck val_labels
val_labels(ces88$degree)
#here it makes sense to remove all
val_labels(ces88$degree)<-NULL
val_label(ces88$degree, 0)<-'no degree'
val_label(ces88$degree, 1)<-'degree'


#Another way would be to use set_value_labels to set the value labels for multiple variables. 

#I am not sure what would be quicker; you can start with the first way and maybe work your way up to this, I'm not sure. 
ces88<-set_value_labels(ces88, 
                        degree=c('degree'=1,'no degree'=0),
                        male=c('male'=1, 'female'=0))
#check
ces88$degree
ces88$male

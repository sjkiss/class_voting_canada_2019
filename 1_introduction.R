#If you have not installed this package please run this command
#install.packages('devtools')
#Load this library
library(devtools)
#Run this command 
#This will install a package that i have created that has all of the ces data sets pre-loaded
install_github('sjkiss/cesdata')
#load the package
library(cesdata)
#you can find the names of all the different data files by visiting the package's source on github
#http://github.com/sjkiss/cesdata/data
#But they all fllow the same notation
#ces followed by a two-digit number for the eelction year. 
#There are some exceptions for ces72 which has three separate files, only two of which are panel (e.g. reinterviews)
data("ces72_jul")
#load 1974-1979-1980
data("ces74")
#load 1984
data("ces84")
#It looks like to complete the loading process, you have to click on the name of folder in the right window pane of RStudio. Very weird, it does work. 
#If you do not have this package installed, please uncomment the following line and run the following command
#install.packages('labelled')
#Load this library
library(labelled)
#This provides a stata-like command to search through datasets to with keyword searches. 

look_for(ces72_jul, "vote")
look_for(ces84, "vote")
look_for(ces84, "NDP")
#some of the older data files 

look_for(ces88, "federal politics")

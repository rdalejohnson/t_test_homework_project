library(plyr)

#Two Questions
#1 
#Whether the youths (n=100) in the NLYS97 had a statistically
#significant decrease or increase in their substance use index
#score between 1997 and 2000.
#
#2
#Whether this decrease or increase in substance use differs by race.  Specifically,
#you hypothesize that WHITE youths compared to non-white youths, will report higher
#substance abuse

#CLEANING AND REVIEW OF DATA SET####

nylsData =read.csv("NYLS97_Substance.csv",sep=",") 

count(nylsData, 'Race')

count(nylsData, 'Substance1997')

count(nylsData, 'Substance2000')

nylsData$Substance2000[nylsData$Substance2000 < 0] <- NA

count(nylsData, 'Race')

count(nylsData, 'Substance1997')

count(nylsData, 'Substance2000')

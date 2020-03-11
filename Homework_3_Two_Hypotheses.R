library(plyr)
library(tidyverse)

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

plyr::count(nylsData, 'Race')

plyr::count(nylsData, 'Substance1997')

plyr::count(nylsData, 'Substance2000')

nylsData$Substance2000[nylsData$Substance2000 < 0] <- NA

plyr::count(nylsData, 'Race')

plyr::count(nylsData, 'Substance1997')

plyr::count(nylsData, 'Substance2000')


nylsData <- nylsData %>% 
  mutate(SubAbuseDelta = (Substance2000-Substance1997))

plyr::count(nylsData, 'SubAbuseDelta')


hitsgram=hist(nylsData$SubAbuseDelta  , main="a histogram of the difference",) 

summary(nylsData$SubAbuseDelta)

subAbuseStDev = sd(nylsData$SubAbuseDelta  ,na.rm=TRUE)
subAbuseMean = mean(nylsData$SubAbuseDelta  ,na.rm=TRUE)

ks.test(x= nylsData$SubAbuseDelta,"pnorm",mean=subAbuseMean,sd=subAbuseStDev)

qqnorm(nylsData$SubAbuseDelta)
qqline(nylsData$SubAbuseDelta)

drinking_problem_score_plot <-
  boxplot(nylsData$SubAbuseDelta,
          main = "Drinking Problem Scores (10 item questionnaire)",
          xlab = "Summarized Scores for 1120 Students",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE)

drinking_problem_score_plot


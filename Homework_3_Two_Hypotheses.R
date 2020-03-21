library(plyr)
library(tidyverse)

#REFERENCES:
# https://www.sheffield.ac.uk/polopoly_fs/1.579191!/file/stcp-karadimitriou-normalR.pdf
# https://www.statsandr.com/blog/do-my-data-follow-a-normal-distribution-a-note-on-the-most-widely-used-distribution-and-how-to-test-for-normality-in-r/
# https://statistics.berkeley.edu/computing/r-t-tests
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3693611/


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


# Descriptive Stats for entire year 1997 
subAbuse1997Mean = mean(nylsData$Substance1997  ,na.rm=TRUE)
subAbuse1997StDev = sd(nylsData$Substance1997  ,na.rm=TRUE)
print (paste("1997 NLSY97 Substance Use Index Score Mean: ", subAbuse1997Mean))
print (paste("1997 NLSY97 Substance Use Index Score St. Dev: ", subAbuse1997StDev))
summary(nylsData$Substance1997)

# Descriptive Stats for entire year 2000 
subAbuse2000Mean = mean(nylsData$Substance2000  ,na.rm=TRUE)
subAbuse2000StDev = sd(nylsData$Substance2000  ,na.rm=TRUE)
print (paste("2000 NLSY97 Substance Use Index Score Mean: ", subAbuse2000Mean))
print (paste("2000 NLSY97 Substance Use Index Score St. Dev: ", subAbuse2000StDev))
summary(nylsData$Substance2000)


############# NON-WHITES SUBSET of Data Set
nonWhitesSubset = subset(nylsData, Race=="0")

subAbuse1997NonWhiteMean = mean(nonWhitesSubset$Substance1997  ,na.rm=TRUE)
subAbuse1997NonWhiteStDev = sd(nonWhitesSubset$Substance1997 ,na.rm=TRUE)
print (paste("1997 NLSY97 Substance Use Index Score Mean, Non-Whites only: ", subAbuse1997NonWhiteMean))
print (paste("1997 NLSY97 Substance Use Index Score St. Dev, Non-Whites only: ", subAbuse1997NonWhiteStDev))
summary(nonWhitesSubset$Substance1997)


subAbuse2000NonWhiteMean = mean(nonWhitesSubset$Substance2000  ,na.rm=TRUE)
subAbuse2000NonWhiteStDev = sd(nonWhitesSubset$Substance2000 ,na.rm=TRUE)
print (paste("2000 NLSY97 Substance Use Index Score Mean, Non-Whites only: ", subAbuse2000NonWhiteMean))
print (paste("2000 NLSY97 Substance Use Index Score St. Dev, Non-Whites only: ", subAbuse2000NonWhiteStDev))
summary(nonWhitesSubset$Substance2000)


############# WHITES SUBSET of Data Set
WhitesSubset = subset(nylsData, Race=="1")

subAbuse1997WhiteMean = mean(WhitesSubset$Substance1997  ,na.rm=TRUE)
subAbuse1997WhiteStDev = sd(WhitesSubset$Substance1997 ,na.rm=TRUE)
print (paste("1997 NLSY97 Substance Use Index Score Mean, Non-Whites only: ", subAbuse1997WhiteMean))
print (paste("1997 NLSY97 Substance Use Index Score St. Dev, Non-Whites only: ", subAbuse1997WhiteStDev))
summary(WhitesSubset$Substance1997)


subAbuse2000WhiteMean = mean(WhitesSubset$Substance2000  ,na.rm=TRUE)
subAbuse2000WhiteStDev = sd(WhitesSubset$Substance2000 ,na.rm=TRUE)
print (paste("2000 NLSY97 Substance Use Index Score Mean, Non-Whites only: ", subAbuse2000WhiteMean))
print (paste("2000 NLSY97 Substance Use Index Score St. Dev, Non-Whites only: ", subAbuse2000WhiteStDev))
summary(WhitesSubset$Substance2000)

nylsData <- nylsData %>% 
  mutate(SubAbuseDelta = (Substance2000-Substance1997))

plyr::count(nylsData, 'SubAbuseDelta')


histgram.delta=hist(nylsData$SubAbuseDelta  , main="a histogram of the difference") 

plot(density(nylsData$SubAbuseDelta, na.rm=TRUE))

summary(nylsData$SubAbuseDelta)

subAbuseStDev = sd(nylsData$SubAbuseDelta  ,na.rm=TRUE)
subAbuseMean = mean(nylsData$SubAbuseDelta  ,na.rm=TRUE)

ks.test(x= nylsData$SubAbuseDelta,"pnorm",mean=subAbuseMean,sd=subAbuseStDev)

shapiro.test(nylsData$SubAbuseDelta)

qqnorm(nylsData$SubAbuseDelta)
qqline(nylsData$SubAbuseDelta)

library(ggpubr)
ggqqplot(nylsData$SubAbuseDelta)

drinking_problem_score_plot <-
  boxplot(nylsData$SubAbuseDelta,
          main = "NLYS97 Score Differences between 1997 and 2000",
          xlab = "Score Differences (change between 1997 and 2000",
          col = "orange",
          border = "brown"
          #horizontal = TRUE,
          #notch = TRUE
          )

drinking_problem_score_plot


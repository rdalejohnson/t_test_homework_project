

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


library(plyr)
library(tidyverse)

############CLEANING AND REVIEW OF DATA SET#####################
############CLEANING AND REVIEW OF DATA SET#####################
############CLEANING AND REVIEW OF DATA SET#####################

nylsData =read.csv("NYLS97_Substance.csv",sep=",") 
plyr::count(nylsData, 'Race')
plyr::count(nylsData, 'Substance1997')
plyr::count(nylsData, 'Substance2000')

# ~~~~~~~~~~~ removal of invalid values below zero in Year 2000 data, replace with NA

nylsData$Substance2000[nylsData$Substance2000 < 0] <- NA
plyr::count(nylsData, 'Race')
plyr::count(nylsData, 'Substance1997')
plyr::count(nylsData, 'Substance2000')

###############  Descriptive Stats for entire year 1997 ####################

subAbuse1997Mean = mean(nylsData$Substance1997  ,na.rm=TRUE)
subAbuse1997StDev = sd(nylsData$Substance1997  ,na.rm=TRUE)
print (paste("1997 YEAR NLSY97 Score Mean for ALL respondents: ", subAbuse1997Mean))
print (paste("1997 YEAR NLSY97 Score SD.  for ALL respondents: ", subAbuse1997StDev))
summary(nylsData$Substance1997)
plyr::count(nylsData, 'Substance1997')

###############  Descriptive Stats for entire year 2000 ####################

#Exclude NAs in year 2000 data set
subAbuse2000Cleaned = subset(nylsData, !is.na(Substance2000))
###############   Descriptive Stats for entire year 2000 ###############  
subAbuse2000Mean = mean(subAbuse2000Cleaned$Substance2000, na.rm=TRUE)
subAbuse2000StDev = sd(subAbuse2000Cleaned$Substance2000  ,na.rm=TRUE)
print (paste("2000 NLSY97 Substance Use Index Score Mean: ", subAbuse2000Mean))
print (paste("2000 NLSY97 Substance Use Index Score St. Dev: ", subAbuse2000StDev))
summary(subAbuse2000Cleaned$Substance2000)
plyr::count(subAbuse2000Cleaned, 'Substance2000')


nonWhitesSubset = subset(nylsData, Race=="0")
#############   NON-WHITES SUBSET of Data Set, YEAR 1997  ###############  
subAbuse1997NonWhiteMean = mean(nonWhitesSubset$Substance1997  ,na.rm=TRUE)
subAbuse1997NonWhiteStDev = sd(nonWhitesSubset$Substance1997 ,na.rm=TRUE)
print (paste("1997 YEAR Score Mean for Non-Whites only: ", subAbuse1997NonWhiteMean))
print (paste("1997 YEAR Score SD.  for Non-Whites only: ", subAbuse1997NonWhiteStDev))
summary(nonWhitesSubset$Substance1997)
plyr::count(nonWhitesSubset, 'Substance1997')

#############   NON-WHITES SUBSET of Data Set, YEAR 2000  ###############  
subAbuseNonWhite2000Cleaned = subset(subAbuse2000Cleaned, Race=="0")
subAbuse2000NonWhiteMean = mean(subAbuseNonWhite2000Cleaned$Substance2000  ,na.rm=TRUE)
subAbuse2000NonWhiteStDev = sd(subAbuseNonWhite2000Cleaned$Substance2000 ,na.rm=TRUE)
print (paste("2000 NLSY97 Substance Use Index Score Mean, Non-Whites only: ", subAbuse2000NonWhiteMean))
print (paste("2000 NLSY97 Substance Use Index Score St. Dev, Non-Whites only: ", subAbuse2000NonWhiteStDev))
summary(subAbuseNonWhite2000Cleaned$Substance2000)
plyr::count(subAbuseNonWhite2000Cleaned, 'Substance2000')


WhitesSubset = subset(nylsData, Race=="1")
############# WHITES SUBSET of Data Set, YEAR 1997 
subAbuse1997WhiteMean = mean(WhitesSubset$Substance1997  ,na.rm=TRUE)
subAbuse1997WhiteStDev = sd(WhitesSubset$Substance1997 ,na.rm=TRUE)
print (paste("1997 NLSY97 Substance Use Index Score Mean, Whites only: ", subAbuse1997WhiteMean))
print (paste("1997 NLSY97 Substance Use Index Score St. Dev, Whites only: ", subAbuse1997WhiteStDev))
summary(WhitesSubset$Substance1997)
plyr::count(WhitesSubset, 'Substance1997')

subAbuseWhite2000Cleaned = subset(subAbuse2000Cleaned, Race=="1")
subAbuse2000WhiteMean = mean(subAbuseWhite2000Cleaned$Substance2000  ,na.rm=TRUE)
subAbuse2000WhiteStDev = sd(subAbuseWhite2000Cleaned$Substance2000 ,na.rm=TRUE)
print (paste("2000 NLSY97 Substance Use Index Score Mean, Whites only: ", subAbuse2000WhiteMean))
print (paste("2000 NLSY97 Substance Use Index Score St. Dev, Whites only: ", subAbuse2000WhiteStDev))
summary(subAbuseWhite2000Cleaned$Substance2000)
plyr::count(subAbuseWhite2000Cleaned, 'Substance2000')


############# COMPUTING AND SUMMARIZING DIFFERENCES BTWN 2000 and 1997 ###########

nylsDifferencesDataSet <- nylsData %>% 
  mutate(SubAbuseDelta = (Substance2000-Substance1997))

subAbuseDifferencesDataSetCleaned = subset(nylsDifferencesDataSet,  !is.na(SubAbuseDelta))
differencesScoresAllMean = mean(subAbuseDifferencesDataSetCleaned$SubAbuseDelta  ,na.rm=TRUE)
differencesScoresAllStDev = sd(subAbuseDifferencesDataSetCleaned$SubAbuseDelta   ,na.rm=TRUE)
print (paste("Differences Scores Mean for ALL respondents: ", differencesScoresAllMean))
print (paste("Differences Scores SD.  for ALL respondents: ", differencesScoresAllStDev))
summary(subAbuseDifferencesDataSetCleaned$SubAbuseDelta)
plyr::count(subAbuseDifferencesDataSetCleaned, 'SubAbuseDelta')

####### NORMALITY TESTING for DIFFERENCES DATA SET ############
histgram.differencesALL = hist(subAbuseDifferencesDataSetCleaned$SubAbuseDelta  , 
                               main="a histogram of the difference") 
ks.test(x=subAbuseDifferencesDataSetCleaned$SubAbuseDelta, "pnorm", 
        mean=differencesScoresAllMean, sd=differencesScoresAllStDev)
shapiro.test(subAbuseDifferencesDataSetCleaned$SubAbuseDelta)
qqnorm(subAbuseDifferencesDataSetCleaned$SubAbuseDelta)
qqline(subAbuseDifferencesDataSetCleaned$SubAbuseDelta)
library(ggpubr)
ggqqplot(subAbuseDifferencesDataSetCleaned$SubAbuseDelta)
#### NOT NORMAL using any of these tests; invoking the CLT for paired t-test.
t.test(subAbuseDifferencesDataSetCleaned$SubAbuseDelta, mu=0, alternative="two.sided")

##### HYPOTHESIS 2 **********************************************************************************
##### HYPOTHESIS 2 **********************************************************************************
##### HYPOTHESIS 2 **********************************************************************************
##### HYPOTHESIS 2 **********************************************************************************


#### NONWHITE SUBSET OF DIFFERENCES SCORES ####
subAbuseDifferencesNONWHITEDataSetCleaned = subset(subAbuseDifferencesDataSetCleaned, Race=="0")
differencesScoresNONWHITEMean = mean(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta  ,na.rm=TRUE)
differencesScoresNONWHITEStDev = sd(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta   ,na.rm=TRUE)
print (paste("Differences Scores Mean for NONWHITE respondents: ", differencesScoresNONWHITEMean))
print (paste("Differences Scores SD.  for NONWHITE respondents: ", differencesScoresNONWHITEStDev))
summary(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta)
plyr::count(subAbuseDifferencesNONWHITEDataSetCleaned, 'SubAbuseDelta')

##### NORMALITY TESTING for NONWHITE DIFFERENCES SCORES
histgram.differencesNONWHITE = hist(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta  , 
                               main="a histogram of the difference") 
ks.test(x=subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta, "pnorm", 
        mean=differencesScoresAllMean, sd=differencesScoresAllStDev)
shapiro.test(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta)
qqnorm(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta)
qqline(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta)
library(ggpubr)
ggqqplot(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta)


#### WHITE SUBSET OF DIFFERENCES SCORES ####
subAbuseDifferencesWHITEDataSetCleaned = subset(subAbuseDifferencesDataSetCleaned, Race=="1")
differencesScoresWHITEMean = mean(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta  ,na.rm=TRUE)
differencesScoresWHITEStDev = sd(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta   ,na.rm=TRUE)
print (paste("Differences Scores Mean for NONWHITE respondents: ", differencesScoresWHITEMean))
print (paste("Differences Scores SD.  for NONWHITE respondents: ", differencesScoresWHITEStDev))
summary(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta)
plyr::count(subAbuseDifferencesWHITEDataSetCleaned, 'SubAbuseDelta')

##### NORMALITY TESTING for NONWHITE DIFFERENCES SCORES
histgram.differencesWHITE = hist(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta  , 
                                    main="a histogram of the difference") 
ks.test(x=subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta, "pnorm", 
        mean=differencesScoresAllMean, sd=differencesScoresAllStDev)
shapiro.test(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta)
qqnorm(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta)
qqline(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta)
library(ggpubr)
ggqqplot(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta)

library(car)
bartlett.test(x=subAbuseDifferencesDataSetCleaned$SubAbuseDelta, subAbuseDifferencesDataSetCleaned$Race)
leveneTest(subAbuseDifferencesDataSetCleaned$SubAbuseDelta, subAbuseDifferencesDataSetCleaned$Race)
fligner.test(subAbuseDifferencesDataSetCleaned$SubAbuseDelta, subAbuseDifferencesDataSetCleaned$Race)

t.test(x=subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta, y=subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta, 
       mu=0, alternative="two.sided", var.equal=TRUE)


##############   CHECKING NONWHITES FOR SCORES DIFFERENCES ***********************************************

histgram.differencesALL = hist(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta  , 
                               main="a histogram of the difference") 
ks.test(x=subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta, "pnorm", 
        mean=differencesScoresAllMean, sd=differencesScoresAllStDev)
shapiro.test(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta)
qqnorm(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta)
qqline(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta)
library(ggpubr)
ggqqplot(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta)
##PAIRED T-TEST FOR NONWHITE Differences
t.test(subAbuseDifferencesNONWHITEDataSetCleaned$SubAbuseDelta, mu=0, alternative="two.sided")

##############   CHECKING WHITES FOR SCORES DIFFERENCES ***********************************************


histgram.differencesALL = hist(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta  , 
                               main="a histogram of the difference") 
ks.test(x=subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta, "pnorm", 
        mean=differencesScoresAllMean, sd=differencesScoresAllStDev)
shapiro.test(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta)
qqnorm(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta)
qqline(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta)
library(ggpubr)
ggqqplot(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta)
##PAIRED T-TEST FOR NONWHITE Differences
t.test(subAbuseDifferencesWHITEDataSetCleaned$SubAbuseDelta, mu=0, alternative="two.sided")



### TRASH BELOW THIS LINE ###

histgram.delta=hist(subAbuseDifferencesDataSetCleaned$SubAbuseDelta  , main="a histogram of the difference") 

plot(density(subAbuseDifferencesDataSetCleaned$SubAbuseDelta, na.rm=TRUE))

summary(subAbuseDifferencesDataSetCleaned$SubAbuseDelta)

subAbuseStDev = sd(subAbuseDifferencesDataSetCleaned$SubAbuseDelta  ,na.rm=TRUE)
subAbuseMean = mean(subAbuseDifferencesDataSetCleaned$SubAbuseDelta  ,na.rm=TRUE)

ks.test(x= subAbuseDifferencesDataSetCleaned$SubAbuseDelta,"pnorm",mean=subAbuseMean,sd=subAbuseStDev)

shapiro.test(subAbuseDifferencesDataSetCleaned$SubAbuseDelta)

qqnorm(subAbuseDifferencesDataSetCleaned$SubAbuseDelta)
qqline(subAbuseDifferencesDataSetCleaned$SubAbuseDelta)

library(ggpubr)
ggqqplot(subAbuseDifferencesDataSetCleaned$SubAbuseDelta)

differences1997to2000 <-
  boxplot(subAbuseDifferencesDataSetCleaned$SubAbuseDelta,
          main = "NLYS97 Score Differences between 1997 and 2000",
          xlab = "Score Differences (change between 1997 and 2000",
          col = "orange",
          border = "brown"
          #horizontal = TRUE,
          #notch = TRUE
          )

differences1997to2000


library(plyr)
library(tidyverse)

#REFERENCES:
# https://www.sheffield.ac.uk/polopoly_fs/1.579191!/file/stcp-karadimitriou-normalR.pdf
# https://www.statsandr.com/blog/do-my-data-follow-a-normal-distribution-a-note-on-the-most-widely-used-distribution-and-how-to-test-for-normality-in-r/
# https://statistics.berkeley.edu/computing/r-t-tests
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/


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


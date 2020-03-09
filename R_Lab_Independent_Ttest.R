# BST611 R Lab
# Independent t-Test
# Research Question: Is there a gender difference in pro-government attitudes?
# 
# Procedure:
#   1. Univariate Analyses: Descriptive Statistics and Cleaning the Data
# 2. Check the Assumptions
# 3. Run the Statistical Test
# 4. State a Conclusion
# Univariate Analyses: Descriptive Statistics and Cleaning the Data
# .	Descriptive Analyses:  Get a histogram of the Pro_government_Attitudes for Male
#############################################################################################

#directoryTofiles<-"\\\\Mac\\Home\\BST6111B\\" # change this will be different for each student

Government=read.csv("GovernmentAttitudes.csv",sep=",")


men = Government$Pro_government_Attitudes[Government$RESPONDENTS_SEX==1]
men.summary=summary(men)
men.summary
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     6.0    22.0    25.0    24.2    27.0    28.0     267

hitsgram=hist(men , main="the Pro_government Attitudes for Male")


# Get a histogram of the Pro_government_Attitudes for Female
women = Government$Pro_government_Attitudes[Government$RESPONDENTS_SEX==2]
women.summary=summary(women)
women.summary
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    9.00   22.00   24.00   23.84   26.00   28.00     303
hitsgram=hist(women , main="the Pro_government Attitudes for Female")

# .	Clean the Data:  Review Outputs.  Both distributions are not normal.  There are more females (N=645; 53.75%) than males (N=555; 46.25%).  Males range from 6 to 28 and females range from 9 to 28.
# .	Check Assumptions:  Normal Distribution
#Plot the PP plot to check normality for men
qqnorm(men)
qqline(men)

#Use the Kolmogorov-Smirnov test to test for normality for men
mean(men,na.rm=TRUE)
## [1] 24.20139
sd(men,na.rm=TRUE)
## [1] 3.37046
ks.test(x=men,"pnorm",mean=24.20139,sd=3.37046)
## Warning in ks.test(x = men, "pnorm", mean = 24.20139, sd = 3.37046): ties
## should not be present for the Kolmogorov-Smirnov test
## 
##  One-sample Kolmogorov-Smirnov test
## 
## data:  men
## D = 0.14284, p-value = 1.573e-05
## alternative hypothesis: two-sided
#Plot the PP plot to check normality for women
qqnorm(women)
qqline(women)

#Use the Kolmogorov-Smirnov test to test for normality for women
mean(women,na.rm=TRUE)
## [1] 23.83918
sd(women,na.rm=TRUE)
## [1] 3.361048

ks.test(x=women,"pnorm",mean=23.83918,sd=3.361048)

## Warning in ks.test(x = women, "pnorm", mean = 23.83918, sd = 3.361048):
## ties should not be present for the Kolmogorov-Smirnov test
## 
##  One-sample Kolmogorov-Smirnov test
## 
## data:  women
## D = 0.11755, p-value = 0.0001571
## alternative hypothesis: two-sided
#.	Check Assumptions:  Homogeneity of Variance
##Use Bartlett's test to test whether two groups have equal variances. Use the "bartlett.test" function

bartlett.test(x=Government$Pro_government_Attitudes, g=Government$RESPONDENTS_SEX)

## 
##  Bartlett test of homogeneity of variances
## 
## data:  Government$Pro_government_Attitudes and Government$RESPONDENTS_SEX
## Bartlett's K-squared = 0.0024337, df = 1, p-value = 0.9607
#.	Run Statistical Test:  Independent t-Test
#Use the t.test function to do two-sample t-tests.
 
t.test(x=men,y=women,mu=0,alternative="two.sided",var.equal=TRUE)

## 
##  Two Sample t-test
## 
## data:  men and women
## t = 1.3458, df = 628, p-value = 0.1789
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.1663319  0.8907471
## sample estimates:
## mean of x mean of y 
##  24.20139  23.83918



#.	State a Conclusion
#Based on the results of an independent samples t-test, we fail to reject the null hypothesis
#and conclude that the mean for females is not statistically different from the mean for
#males, therefore attitudes do not differ by gender [t(df=628)=1.35, p=0.1789].
#We interpret these results with caution, considering the large percentage of missing observations.

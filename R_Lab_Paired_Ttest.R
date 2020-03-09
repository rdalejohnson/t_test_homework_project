#################################################################################################################################
# BST611 R Lab
# Paired t-Test
# Research Question: Does taking a class on American values have an effect on pro-government
# attitudes one year later?
# 
# Procedure:
#   1. Univariate Analyses: Descriptive Statistics and Cleaning the Data
# 2. Check the Assumptions
# 3. Run the Statistical Test
# 4. State a Conclusion
# 1.  Univariate Analyses:  Descriptive Statistics and Cleaning the Data
# .	Create difference variable.  First, add a variable to the dataset that is the difference between the paired variables.
# Government$Difference  = (Government$Pro_government_Attitudes_12_mont - Government$Pro_government_Attitudes)
# .	Descriptive Analyses
# For continuous variables, we will use "summary" to obtain summary statistics and use "hist" to get a histogram
###############################################################################################################################


#directoryTofiles<-"\\\\Mac\\Home\\BST6111B\\" 

Government=read.csv("GovernmentAttitudes.csv",sep=",") 

Government$Difference  = (Government$Pro_government_Attitudes_12_mont - Government$Pro_government_Attitudes)

# .	Descriptive Analyses

diff.summary = summary(Government$Difference)
diff.summary
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   -5.00    7.00   10.00   10.06   13.00   24.00     570
# Get a histogram of the difference
# hitsgram=hist(Government$Difference  , main="a histogram of the difference") 
# 
# 
# .	Check Assumptions:  Normal Distribution.  We are checking normality on the Difference Variable.
# Kolmogorov-Smirnov normality test
# (Note: if your variable has missing (NA) values, include the "na.rm=TRUE" option in these functions)

mean(Government$Difference  ,na.rm=TRUE)
## [1] 10.06032
sd(Government$Difference  ,na.rm=TRUE)
## [1] 4.890162
ks.test(x= Government$Difference  ,"pnorm",mean=10.06032,sd=4.890162)
## Warning in ks.test(x = Government$Difference, "pnorm", mean = 10.06032, :
## ties should not be present for the Kolmogorov-Smirnov test
## 
##  One-sample Kolmogorov-Smirnov test
## 
## data:  Government$Difference
## D = 0.060099, p-value = 0.02111
## alternative hypothesis: two-sided
## Normal probability plot (P-P Plot) or the Q-Q plot.
qqnorm(Government$Difference)
qqline(Government$Difference)


# .	Run Statistical Test:  Paired t-Test 
# Recall, the null hypothesis is that the mean of the difference is 0 (or, no change or no difference). The alternative hypothesis is that the difference is statistically different from 0 (there is a change). This may be symbolically expressed as:
#   H0: ??difference=0
# HA: ??difference ???0

t.test(Government$Difference, mu=0, alternative="two.sided")

## 
##  One Sample t-test
## 
## data:  Government$Difference
## t = 51.637, df = 629, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##   9.677724 10.442911
## sample estimates:
## mean of x 
##  10.06032

# .	State a Conclusion
# Based on the results of a paired t-test, we reject the null hypothesis and conclude that the
# mean difference in attitudes is significantly different from zero, and that there was a
# change in attitude over the course of 12 months by an average of 10.06 points [95% CI=9.68, 10.44; t(df=629)=51.64, p<0.0001).  We interpret these results with caution, considering the large percentage of missing observations.

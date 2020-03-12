
#######################################################
#####  R Lab t-Statistic ##############################
#######################################################

# BST 611
# Research Question: Does our sample's pro-government attitudes differ from the population's
# mean of 20?
# Procedure:
#   1. Univariate Analyses: Descriptive Statistics and Cleaning the Data
# 2. Check the Assumptions
# 3. Run the Statistical Test
# 4. State a Conclusion
# Create a dataset named "Government"
# .	Univariate Analyses:  Descriptive Statistics and Cleaning the Data
# For continuous variables, we will use "summary" to obtain summary statistics and use "hist" to get a histogram
########################################################################################################################
##############################################################
#Need to paste the directory to the files of your data below##
##############################################################
#directoryTofiles<-"\\\\Mac\\Home\\BST6111B\\" 

Government=read.csv("GovernmentAttitudes.csv",sep=",")
attitude.summary = summary(Government$Pro_government_Attitudes)
attitude.summary
 
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##       6      22      25      24      27      28     570
 
#Get a histogram of the Pro_government_Attitudes variable
hitsgram=hist(Government$Pro_government_Attitudes, main="a histogram of the Pro_government Attitudes variable")

# .	Clean the Data:  Review the Variable's Output.  Attitudes is negatively skewed as shown in the histogram.  Mean is slightly less than the median.  Minimum is 6 and Maximum is 28.  N=630; Missing 570 (47.5% missing!)
# .	Check Assumptions: Normal Distribution.
# Plot the PP plot to check normality. Use the "qqnorm" function.

 qqnorm(Government$Pro_government_Attitudes)
 qqline(Government$Pro_government_Attitudes)

# Use the Kolmogorov-Smirnov test to test for normality. Use the "ks.test" function.
# Note: if your variable has missing (NA) values, include the "na.rm=TRUE" option in these functions

mean(Government$Pro_government_Attitudes,na.rm=TRUE)

## [1] 24.00476
sd(Government$Pro_government_Attitudes,na.rm=TRUE)
## [1] 3.367521
ks.test(x=Government$Pro_government_Attitudes,"pnorm",mean=24.00476,sd=3.367521)

#Line added by Dale:
shapiro.test(Government$Pro_government_Attitudes)


########################################################################################
## Warning in ks.test(x = Government$Pro_government_Attitudes, "pnorm", mean =
## 24.00476, : ties should not be present for the Kolmogorov-Smirnov test
## 
##  One-sample Kolmogorov-Smirnov test
## 
## data:  Government$Pro_government_Attitudes
## D = 0.12891, p-value = 1.614e-09
## alternative hypothesis: two-sided
########################################################################################

t.test(x=Government$Pro_government_Attitudes, alternative="two.sided", mu=20)

## 
##  One Sample t-test
## 
## data:  Government$Pro_government_Attitudes
## t = 29.849, df = 629, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 20
## 95 percent confidence interval:
##  23.74130 24.26823
## sample estimates:
## mean of x 
##  24.00476

# .	State a Conclusion mj
# Based on the results of a single-sample t-test, we reject the null hypothesis and conclude
# that the sample mean is significantly different from the population mean (t(df=629)=29.85,
#                                                                           p<0.0001).  We are 95% confident that the true mean difference is between 23.74 and 24.27. 
# We interpret these results with caution, considering the large percentage of missing observations. 
# 

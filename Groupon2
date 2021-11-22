# R codes for ABM 2018 - 2019 (Lectures 12)

#Clear up all objects in the current environment:
rm(list=ls())

###########################################
#Set up the R environment and read in data:
##########################################

#Setting working directory:
setwd("The path to your own working directory (folder)")

#Read in the data set stored in a CSV file (make sure 
#CSV file is in the working directory)
#Store the data set in a data frame named 'dt.groupon'
#Data frame: Used for storing data tables; each column 
#contains values of one variable and each row contains 
#one set of values from each column
dt.groupon <- read.csv("dt-groupon-purchases-all.csv")

###################################################
#Manipulate a data frame using the 'dplyr' package
###################################################

#If a package is used first time, you need to install it
install.packages("dplyr")

#Loading the installed package
library("dplyr")
#or equivalently,
require("dplyr")

#For demonstrations, we select the following variables (columns)
#from the original data frame: 
  # 1. Purchase: A respondent's stated likelihood to purchase the deal (scale 0 to 100)
  # 2. ShareLink: A respondent's stated likelihood to share the deal's link on social media 
       #(scale 0 to 100)
  # 3. Deal01: Type of product, 1 = Restaurant, 2 = Headphone
  # 4. Deal02: Requirement to login with social media account in order to purchase the deal, 
       #1 = Yes, 2 = No
  # 5. Deal03: Discount percentage offered by the deal, 1 = 20%, 2 = 40%, 3 = 60%, 4 = 80%
  # 6. Deal04: Number of friends that should buy the deal via the shared linjk in order for the 
       #focal user to get the deal for free, 1 = one friend, 2 = three friends, 3 = five friends
  # 7. PQ1: Respondent's perceived product quality (measure 1, scale 0 to 7)
  # 8. PQ2: Respondent's perceived product quality (measure 2, scale 0 to 7)
  # 9. PQ3: Respondent's perceived product quality (measure 3, scale 0 to 7)
  # 45. dGender: Gender of the respondent, 1 = male, 2 = female
  # 47. IBA: Respondent in IBA or BA, 1 = BA, 2 = IBA
  # 48. SatFac: Satisfaction with studying in the program, scale 0 to 100
  # 49. HrPWork: Number of hours per week spent on performing paid work
  # 50. HrVWork: Number of hours per week spent on performing voluntary work
  # 51. HrClass: Number of hours per week spent on attening classes
  # 52. HrStud: Number of hours per week spent on studying other than attending classes

#Select the above-listed columns and overwrite the original data frame
#'<-': assignment operator; assign the data after the operator to the 
#object before the command
dt.groupon <- select(dt.groupon, c(1:9, 45, 47:51))

#Based on the definitions of variables Deal01-04, we change their names 
#by using the 'rename()' function
dt.groupon <- rename(dt.groupon, ProductType=Deal01, Login=Deal02, 
                     Discount=Deal03, Invitation=Deal04)

#Creating new variable using the function 'mutate' in package 'dplyr'
#Create a new variable, 'PQ', that has the average of the three product 
#quality measures
dt.groupon <- mutate(dt.groupon, PQ = (PQ1+PQ2+PQ3)/3)
#Drop the original three PQ variables:
dt.groupon <- select(dt.groupon, -c(PQ1, PQ2, PQ3))

##################################################
#Dealing with missing values
##################################################
#Assume here (and also in your case assignment) that values are missing 
#at random
#Simple solution: listwise deletion 

#The following code deletes all observations with a missing value for
#at least one variable and overwrite the original data frame
dt.groupon <- dt.groupon[complete.cases(dt.groupon), ]

##########################################
#Correlation analysis
##########################################

# Pearson correlation coefficient and 
# corresponding statsitical test
cor.test(case$mean_user, case$vol_user, method="pearson")

# Check if the two variables follow normal distributions
shapiro.test(case$mean_user)
shapiro.test(case$vol_user)

# Spearman correlation coefficient and 
# corresponding statsitical test
cor.test(case$mean_user, case$mean_critic, method="spearman")


############################################
# Two-Sample T-test with independent samples
############################################

# In case of two independent samples, need to test the equality of variances
fligner.test(dt.groupon$Purchase, dt.groupon$ProductType)
#Or equivalently,
fligner.test(Purchase ~ ProductType, data=dt.groupon)

# Compare two means of independent samples with T-test
# Assuming equal variances:
t.test(Purchase ~ ProductType, data=dt.groupon, var.equal=TRUE)
# Assuming unequal variances:
t.test(Purchase ~ ProductType, data=dt.groupon, var.equal=FALSE)

###############################################
# Two-Sample T-test with matched pairs sample
###############################################
# Compare two means of matched samples with T-test using 'dt.groupon'
t.test(Purchase ~ ProductType, data=dt.groupon, paired=TRUE)
# You will see a error from runing the above test because the data
# in this case are not collected as matched pairs design

# Now mannually input another data set to show how to conduct
# paird T-test
# Approach 1: data in long format
Input = ("
         Bird    Feather   Length
         A       Typical   -0.255
         B       Typical   -0.213
         C       Typical   -0.19
         D       Typical   -0.185
         E       Typical   -0.045
         F       Typical   -0.025
         G       Typical   -0.015
         H       Typical    0.003
         I       Typical    0.015
         J       Typical    0.02
         K       Typical    0.023
         L       Typical    0.04
         M       Typical    0.04
         N       Typical    0.05
         O       Typical    0.055
         P       Typical    0.058
         A       Odd       -0.324
         B       Odd       -0.185
         C       Odd       -0.299
         D       Odd       -0.144
         E       Odd       -0.027
         F       Odd       -0.039
         G       Odd       -0.264
         H       Odd       -0.077
         I       Odd       -0.017
         J       Odd       -0.169
         K       Odd       -0.096
         L       Odd       -0.33
         M       Odd       -0.346
         N       Odd       -0.191
         O       Odd       -0.128
         P       Odd       -0.182
         ")
# Read the data in the 'Input' object into a data frame 'dt.bird'
dt.bird = read.table(textConnection(Input),header=TRUE)
# Note: data must be ordered so that the first observation of Group 1
# is the same subject as the first observation of Group 2, and so on 

# If in practice your data are not such ordered, you can sort the observations
# by indiviudals within each group:
dt.bird <- dt.bird[order(dt.bird$Feather, dt.bird$Bird),]

# Paird T-test to compare Length over two groups: "Odd" vs. "Typical"
t.test(Length ~ Feather, data=dt.bird, paired=TRUE)
# In the output, "mean of the dfferences" is "mean(Odd) - mean(Typical)"
# This order is consistent with the alphabatic order of the two lables: "Odd" vs. "Typical"

# Approach 2: data in wider format
Input2 = ("
        Bird   Typical  Odd
         A     -0.255   -0.324
         B     -0.213   -0.185
         C     -0.190   -0.299
         D     -0.185   -0.144
         E     -0.045   -0.027
         F     -0.025   -0.039
         G     -0.015   -0.264
         H      0.003   -0.077
         I      0.015   -0.017
         J      0.020   -0.169
         K      0.023   -0.096
         L      0.040   -0.330
         M      0.040   -0.346
         N      0.050   -0.191
         O      0.055   -0.128
         P      0.058   -0.182  
       ")

dt.bird.2  = read.table(textConnection(Input2),header=TRUE)

# Paird T-test to compare Length over two groups: "Typical" vs. "Odd"
t.test(dt.bird.2$Typical, dt.bird.2$Odd, paired=TRUE)

# In the output, "mean of the dfferences" is "mean(Typical) - mean(Odd)"
# This order is consistent with the order of two variables in the 't.test' function


##########################################
# Wilcoxon Rank Sum Test
##########################################
# Check if the outcome variable is normally distributed in each subgroup
shapiro.test(dt.groupon$Purchase[dt.groupon$ProductType==1])
shapiro.test(dt.groupon$Purchase[dt.groupon$ProductType==2])

# Conduct the test using the 'wilcox.test()' function
wilcox.test(Purchase ~ ProductType, data=dt.groupon)

##########################################
# Wilcoxon Signed-Rank Test
##########################################
# Check if the pairwise difference is normally distributed
# Assumption checking is easier for data in wide format: dt.bird.2
# First create a new variable containing all the pairwise differences
dt.bird.2$diff <- dt.bird.2$Typical - dt.bird.2$Odd
# Conduct the normality test using the sample pairwise differences
shapiro.test(dt.bird.2$diff)

# If the test result suggests normality is violated, conduct
# the Wilcoxon Signed-Rank Test:
wilcox.test(dt.bird.2$Typical, dt.bird.2$Odd, paired=TRUE)

##########################################
# Chi-squared goodness-of-fit test
##########################################
# A Chi-squared goodness-of-fit test can be used to test
# the dependency (correlation) between two categorical variables
chisq.test(dt.groupon$dGender, dt.groupon$IBA)

# Use a barchart to visualize how the two variables are inter-dependent 
# 1. Create a cross-classfication table and store it as a data.frame
cross.table <- as.data.frame(table(dt.groupon$dGender, dt.groupon$IBA))
View(cross.table)
# 2. Create a barchart using 'ggplot2' and cross-classification table
library(ggplot2)
ggplot(cross.table, aes(fill=Var1, y=Freq, x=Var2)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete(name="Study Program", labels=c("1"="BA","2"="IBA")) + 
  scale_fill_discrete(name="Gender", labels=c("1"="Male","2"="Female"))





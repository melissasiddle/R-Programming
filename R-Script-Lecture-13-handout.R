# R codes for ABM 2018 - 2019 (Lectures 13)

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
# Manipulate a data frame using the 'dplyr' package
###################################################

# If a package is used first time, you need to install it
# install.packages("dplyr")

# Load the installed package
library("dplyr")


# For demonstrations, we select the following variables (columns)
# from the original data frame: 
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
# One-way ANOVA
##########################################

# Conduct one-way ANOVA to see if likehood to purchase differs between
# different levels of discount

# First declare the categorical variable using the 'factor()' function;
# Within the 'factor()' function, new labels to the variable's values
# can be assigned.
dt.groupon$Discount <- factor(dt.groupon$Discount, 
                              levels=c("1","2","3","4"), 
                              labels=c("20%","40%","60%","80%"))

# In the following, 'aov()' is used to conducte one-way ANOVA; the results
# are stored in the object -- 'result.aov'
result.aov <- aov(Purchase ~ Discount, data=dt.groupon)

# Print out the results (ANOVA table) using 'summary()'
summary(result.aov)

# Use the package 'ggpubr' to create plot of means
# Install the package if it has not been installed before
install.packages("ggpubr")
# Load the package
library("ggpubr")
# Use the 'ggline' function in the package to create the mean plot
ggline(dt.groupon, x="Discount", y="Purchase", add="mean", ylim = c(20, 70))

# Tukey Method for post-hoc multiple pairwise-comparisons
TukeyHSD(result.aov)

# Check ANOVA assumptions:
# #1: Fligner-Killen test to see if variances are equal across groups
fligner.test(Purchase ~ Discount, data=dt.groupon)

# #2: Shapiro-Wilk test to see if the outcome variable is normally 
# distributed in each subgroup
shapiro.test(dt.groupon$Purchase[dt.groupon$Discount=="20%"])
shapiro.test(dt.groupon$Purchase[dt.groupon$Discount=="40%"])
shapiro.test(dt.groupon$Purchase[dt.groupon$Discount=="60%"])
shapiro.test(dt.groupon$Purchase[dt.groupon$Discount=="80%"])

# If equal-variance assumption is violated but normality assumption holds, 
# we can use the an unequal-variance version of F-test:
oneway.test(Purchase ~ Discount, data=dt.groupon, var.equal=FALSE)

# If normality assumption is violated, use the non-paramertric alternative
# Kruskal-Wallis Rank Sum Test
kruskal.test(Purchase ~ Discount, data=dt.groupon)

# Dunn's test for post-hoc multiple comparions based on
# rank sums; conducted after Kruskal-Wallis Rank Sum Test
# to find out which specific group means are statistically
# different from each other

# Use the package 'FSA' to do this
# Install the package if it has not been installed before
install.packages("FSA")
# Load the package
library("FSA")
# Use the 'dunnTest' function in the package to conduct the 
# Dunn test
dunnTest(Purchase ~ Discount, data=dt.groupon)


##########################################
# Two-factor ANOVA
##########################################
# Conduct two-factor ANOVA to see if discount level and product type interact 
# to affect likehood to purchase

# First declare the categorical variable using the 'factor()' function;
# Within the 'factor()' function, new labels to the variable's values
# can be assigned.

# Note the 'Discount' variable has been declared as 'factor' above,
# so do NOT apply the 'factor()' function again to 'Discount' variable 
# (declaring once is just enough)
dt.groupon$ProductType <- factor(dt.groupon$ProductType, 
                              levels=c("1","2"), 
                              labels=c("Restaurant","Headphone"))

# Use the 'aov()' function to conduct two-factor ANOVA;
# Note how the formula is defined
result.2aov <- aov(Purchase ~ Discount + ProductType + 
                     Discount:ProductType, data = dt.groupon)
summary(result.2aov)

# Use the 'ggline' function in the 'ggpubr' package to create the mean plot
# in order the provide more insights about the nature of the interaction
# effect
# Load the package
library("ggpubr")

ggline(dt.groupon, x="Discount", y="Purchase", linetype="ProductType", 
       add="mean", ylim = c(30, 70))

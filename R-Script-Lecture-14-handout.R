# R codes for ABM 2018 - 2019 (Lecture 14)

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
# Multiple Linear Regression
##########################################

# First declare the categorical variables using the 'factor()' function
dt.groupon$ProductType <- factor(dt.groupon$ProductType)
dt.groupon$Discount <- factor(dt.groupon$Discount)
dt.groupon$Login <- factor(dt.groupon$Login)
dt.groupon$Invitation <- factor(dt.groupon$Invitation)
dt.groupon$dGender <- factor(dt.groupon$dGender)
dt.groupon$IBA <- factor(dt.groupon$IBA)

# Define a linear model and store the formula in the object 'modelA'
modelA <- (Purchase ~ ShareLink + ProductType + Login + Discount + Invitation
            + dGender + IBA + SatFac + HrPWork + HrVWork + HrClass + PQ)

# Estimate the above model by OLS using the sample data
# We call the function 'lm()' and store the results in 'resultA'
resultA <- lm(formula=modelA, data=dt.groupon)

# Print out the estimation results
summary(resultA)

#-----------------------------------
# Change order of levels of a factor
#-----------------------------------
# Check the default order of levels of 'Discount'
levels(dt.groupon$Discount)

# Change the baseline level of 'Discount'
# Put level '2' as the first level, then level '2' will be treated as
# (omitted) baseline level in regression
dt.groupon$Discount <- factor(dt.groupon$Discount, 
                              levels=(c("2","1","3","4")))

# Re-check the default order of levels of 'Discount'
levels(dt.groupon$Discount)

# Reestimate 'modelA' and store the resutls in 'resultA.2'
resultA.2 <- lm(formula=modelA, data=dt.groupon)

# Print out the estimation results
summary(resultA.2)

#--------------------------------------
# Residual analysis
#--------------------------------------
# Obtain the histogram of the residuals
hist(residuals(resultA), breaks=50)

# Obtain the scatter plot of residuals against the fitted values of y
# 'pch' controls marker type; 'cex' controls marker size
plot(resultA, 1, pch=19, cex =.3)

# Two Formal tests of the residuals:

# Shapiro-Wilk normality test of the residuals
shapiro.test(resultA$residuals)

# Non-constant variance score test: Breusch-Pagan Test
# Using the 'ncvTest()' function in the "car" package

# Install the 'car' package if it has not been installed before
install.packages("car")

# Load the 'car' package
library("car")

# Use the 'ncvTest()' to conduct the Breusch-Pagan Test
ncvTest(resultA)

#--------------------------------------
# Multicollinearity
#--------------------------------------
# Use the 'vif()' function in the 'car' package to obtain 
# variance inflation factors (VIFs) for the independent 
# variables based on the estimation results

# Load the 'car' package
library("car")

# Use the 'vif()' function to obtain VIFs
vif(resultA)

#--------------------------------------
# Non-linearity
#--------------------------------------
# Suppose you suspect that 'ShareLink' affects 'Purchase' nonlinearly

# Define another linear model with a quadratic term of 'ShareLink' 
# and store the formula in the object 'modelB'
# Note that the quadratic term of 'ShareLink' is created in the formula
# by 'I(ShareLink^2)', so you don't need to mannually create the quadratic
# term beforehand
modelB <- (Purchase ~ ShareLink + I(ShareLink^2) + ProductType + Login + Discount 
           + Invitation+ dGender + IBA + SatFac + HrPWork + HrVWork + HrClass + PQ)

# Estimate the above model by OLS using the sample data
# Call the function 'lm()' and store the results in 'resultB'
resultB <- lm(formula=modelB, data=dt.groupon)

# Print out the estimation results
summary(resultB)

#--------------------------------------
# Non-linearity & Interaction
#--------------------------------------
# 1. Interaction between two continuous variables:

# Define another linear model with an interaction term between 'ShareLink' 
# and 'PQ'; Store the formula in the object 'modelC'
# Note that the interaction term is created in the formula by 'ShareLink:PQ', 
# so no need to mannually create the interaction term beforehand.
modelC <- (Purchase ~ ShareLink + ProductType + Login + Discount + Invitation 
           + ShareLink:PQ + dGender + IBA + SatFac + HrPWork + HrVWork 
           + HrClass + PQ)

# Estimate the above model by OLS using the sample data
# Call the function 'lm()' and store the results in 'resultC'
resultC <- lm(formula=modelC, data=dt.groupon)

# Print out the estimation results
summary(resultC)

# 2. Interaction between one continuous and one categorical independent variable:

# Define another linear model with an interaction term between 'ShareLink' 
# and 'Invitation'; Store the formula in the object 'modelD'
# Note that the interaction term is created in the formula by 'ShareLink:Invitation', 
# so no need to mannually create the interaction term beforehand.
modelD <- (Purchase ~ ShareLink + ProductType + Login + Discount + Invitation 
          + ShareLink:Invitation + dGender + IBA + SatFac + HrPWork + HrVWork 
           + HrClass + PQ)

# Estimate the above model by OLS using the sample data
# Call the function 'lm()' and store the results in 'resultD'
resultD <- lm(formula=modelD, data=dt.groupon)

# Print out the estimation results
summary(resultD)

#--------------------------------------
# Integrating regression results
#--------------------------------------
# Use the 'stargazer' package again
library(stargazer)

# If you work with latex, you can output table in the 'latex' type
stargazer(resultA, resultB, resultC, resultD, type="latex", no.space=TRUE,
          out="dt.groupon.regression.table.tex")

# If you work with Word, you can output table in the 'html' type and specify the output 
# file with a .doc extension
stargazer(resultA, resultB, resultC, resultD, type="html", no.space=TRUE,
          out="dt.groupon.regression.table.doc")


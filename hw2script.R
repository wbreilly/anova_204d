# Walter Reilly 
# hw 2
# 4.17.17

#1 What are the assumptions of a one-way fixed-effects ANOVA?
#2 Bring in the data file to R, verify the variable names, and declare the ‘group’ variable as a class variable.
#3 Check to see if there are missing values in the data file.
#4 Create boxplots to compare ‘attendance’ by ‘group’.
#5 Conduct Bartlett’s test for homogeneity of variance; report and interpret the results of the test.
#6 Fit the one-way ANOVA using the ‘lm’ function
#7 Fit the one-way ANOVA using the ‘aov’ function
#8 Com pare the results from (6) and (7)
#9 Evaluate the residuals for normality
#10 Based on your analysis, including checks on model assumptions, decide if an alternative procedure should be used. If so, 
#   perform and report on the additional analysis.

#1 Assumptions of a fixed effects ANOVA 
# Methodological:
# (1) random sampling / random assignment to groups
# (2) independence of scores within group 
# Statistical
# (3) normally distributed populations
# (4) homogeneity of variance across groups


library(tidyverse)

#2
d = read.csv('lab2.csv')

# variable names
names(d)
# [1] "group"    "attended"

class(d$group)
d$group = as.factor(d$group)

#3
sum(complete.cases(d))
# no incomplete cases

#4
# boxplot
boxplot(d$attended ~ d$group, 
        xlab="group",
        ylab="attended", 
        main = "Boxplots By Group")

#5
bartlett.test(attended ~ group, data = d) 
# Bartlett test of homogeneity of variances
# 
# data:  attended by group
# Bartlett's K-squared = 0.98494, df = 3, p-value = 0.8049
# Bartlett's test indicates that the variances are equal p = .80 

#6 fit with lm
anova(lm(attended ~ group, data = d))
# Analysis of Variance Table
# 
# Response: attended
# Df  Sum Sq Mean Sq F value   Pr(>F)   
# group      3  738.59 246.198  6.8177 0.001361 **
#   Residuals 28 1011.12  36.112                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#7 fit the anova model using aov()
m1 <- aov(attended ~ group, data = d)
summary(m1)
# Df Sum Sq Mean Sq F value  Pr(>F)   
# group        3  738.6  246.20   6.818 0.00136 **
#   Residuals   28 1011.1   36.11                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#8 
# The results of the two anova computations are identical 

#9 check for normality in residuals
plot(m1)
# Based on the qq plot, the distirbution appears to be mostly normal, perhaps with a slight left skew. 

#10
# Based on the available information, it does not appear that another approach is needed. Bartlett's test indicated homoscedasticity
# and the qq plot indicated a normal population. I don't know how the data were collected so I can't speak to the assumptions
# of random sampling and random assignment 



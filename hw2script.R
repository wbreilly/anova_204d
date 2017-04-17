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

library(tidyverse)

d = read.csv('lab2.csv')

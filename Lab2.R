# PSC204d, Spring 2017
# Lab 2

# Read in mydata (replace this path with your own path)
dpath = 'D:/Documents/Work/UCD/TAing/PSC204d_Spring2012/Data/CSV/ch1labs.csv'
mydat <- read.csv(dpath, header=TRUE, sep = ',')  # if in .csv

# CRITICAL: make sure categorical variables are classed as factors
mydat$group = as.factor(mydat$group)

# What are the variables in this (simple) data set?
names(mydat)

# What is the class of our data set?
class(mydat) # is usually a data frame sometimes we work with matrices

# What about our variables in the data set?
class(mydat$group); class(mydat$attended)
	# recall that group is a factor, if it's not a factor, make sure it's a factor!

complete.cases(mydat)
	# returns vector indicating which values are complete, obviously all records are complete
	
# Create a duplicate dataset with missing values
duplicate = mydata
duplicate[29,2] = NA # designate the 29th row, 2nd column as NA

complete.cases(duplicate) # complete.cases may be unwieldy with large data sets

sum(is.na(duplicate$attended)) # checks each element in the vector to see if it is NA and returns a sum, 1 or more indicates that a missing value is present
	# use this method for a variable with a lot of values

# -------------------------------------------------
# 1-Way ANOVA: Outcome (DV) ~ Group (IV) (4 levels)
# -------------------------------------------------

# fit the anova model using aov()
fit1 <- aov(attended ~ group, data = mydat)
summary(fit1)
print(model.tables(fit1, type = "means"))	#report the means each group

# alternatively, fit the anova model using lm() 
fit2 <- lm(attended ~ group, data = mydat)
anova(fit2)   # tests for sig F
summary(fit2)

# notice that the F-value is the same in both fit1 and fit2
	# also notice that the t-value in fit2 is the square root of the F-value
	# t^2 = F
	# t statistic is related to the F statistic

# effect size: explained variance = R^2 = SSb /(SSb + SSw)
	# should be 709.8/(709.8+1039.9) = .406

# model assumptions diagnostics
plot(fit1) 									   # QQ normality
bartlett.test(attended ~ group, data = mydat)  # homoscedasticity, assumption of anova that the variance in each group is the same
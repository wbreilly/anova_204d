# PSC204d, Spring 2017
# Lab 2

########################################################
#################Multiple Comparisons###################
########################################################

# Read in mydata (replace this path with your own path)
dpath = 'D:/Documents/Work/UCD/TAing/PSC204d_Spring2012/Data/CSV/ch1labs.csv'
mydat <- read.csv(dpath, header=TRUE, sep = ',')  # if in .csv

# CRITICAL: make sure categorical variables are classed as factors
mydat$group = as.factor(mydat$group)

# ANOVA to compare means in each group
fit1 <- aov(attended ~ group, data = mydat)
summary(fit1)
	# p.value indicates that we reject the null hypothesis
	# we can then conclude that...?
	
# What's the problem with simply rejecting the null hypothesis of an ANOVA?

# What if we want to know specifically which groups are different from each other?

pairwise.t.test(mydat$attended, mydat$group, p.adjust = "none", pool.sd = TRUE)
	# Does not correct alpha level for multiple comparisons
	# Type I error rate is elevated

pairwise.t.test(mydat$attended, mydat$group, p.adjust = "bonferroni", pool.sd = TRUE)
	# Bonferroni correction is used to find means that are 				significantly different from each other, but controls for 			family error rate
	# Problem is that alpha level may be extremely low when there 		are multiple groups, which makes each test severely underpowered

TukeyHSD(fit1)
	# Also compares all possible pairwise differences

################################################
##############Transformations###################
################################################

# Recall the assumptions of ANOVA
# 1. Independence of observations
# 2. Normality of residuals
# 3. Homoscedasticity

# If the residuals aren't normal, we can transform one or more variables to satisfy assumption #2

# Log-transform the dependent variable
mydat$log.attended = log(mydat$attended)

fit2 = aov(log.attended ~ group, data = mydat)
summary(fit2)

# Examine residuals of model fit2
hist(fit2$residuals) # does this look better?

# Squared transformation
mydat$squared.attended = (mydat$attended)^2

fit3 = aov(squared.attended ~ group, data = mydat)
summary(fit3)

# Examine residuals of model fit3
hist(fit3$residuals) # Looks terrible

# Let's do one more transformation: square root transformation
mydat$sqrt.attended = sqrt(mydat$attended)

fit4 = aov(sqrt.attended ~ group, data = mydat)
summary(fit4)

# Examine residuals of model fit4
hist(fit4$residuals)

# Log, square, and square root are the most common transformations you will see
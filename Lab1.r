
# PSC204d, Spring 2017
# Basic Statistics in R
# Example code provided by S. Aichele


# -------------------------
# Load Stuff Into Workspace
# -------------------------

# remove old mydata from active computer memory
rm(list = ls())

# load libraries as needed
library(gplots)		# for plotting (requires gplots package)
library(gmodels)	# for planned comparisons (requires gmodels package)
library(agricolae)  # for scheffe post-hoc test (requires agricolae package)

# Read in mydata (replace this path with your own path)
dpath = 'ch1labs.csv'
mydat <- read.csv(dpath, header=TRUE, sep = ',')  # if in .csv


# -----------------
# Descriptive Stats
# -----------------

# What variables are here?
names(mydat)

# CRITICAL: make sure categorical variables are classed as factors
mydat$group = as.factor(mydat$group)

# How are they distributed overall?
summary(mydat)
sd(mydat)

# How are they distributed by group?
by(mydat, mydat[,"group"], summary)
by(mydat$attended, mydat[,"group"], sd)


# --------------------------------
# Plot the mydata for fun and profit
# --------------------------------

# histogram
hist(mydat$attended, 
	 freq = TRUE, 
	 breaks = max(mydat$attended),
	 col = 400,
	 xlim = c(0, max(mydat$attended)),
	 ylim = c(0, max(table(mydat$attended)) + 1),
	 main = "Histogram",
	 xlab = "Attended"
	 )
	 
# boxplot
boxplot(mydat$attended ~ mydat$group, 
          xlab="group",
          ylab="attended", 
		  main = "Boxplots By Group"
		  )
	 
# means w/ CIs; requires gplots package
plotmeans(mydat$attended ~ mydat$group, 
          xlab="group",
          ylab="attended", 
		  main="Mean Plot\nwith 95% CI"
		  )


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

# effect size: explained variance = R^2 = SSb /(SSb + SSw)

# model assumptions diagnostics
plot(fit1) 									   # QQ normality
bartlett.test(attended ~ group, data = mydat)  # homoscedasticity (eq. variance?)

# planned comparisons
fit.contrast(fit1, mydat$group, rbind(" g1 vs. g4"     = c(1,0,0,-1),  
                                    " g1,2 vs. g3,4" = c(-1,-1,1,1),
									" g1 vs. g2,3,4" = c(3,-1,-1,-1)
									))

# post-hoc comparisons  (least to most conservative)
LSD.test(fit1, "group", p.adj("bonferroni"))
TukeyHSD(fit1)  
scheffe.test(fit1, "group")

# -------------------------------------------------
# Stuff to complete your first assignment
# -------------------------------------------------

# I'm aware that there's an R extension to download the data but I'll show you to import an SPSS file into R

install.packages('Hmisc') # this package will allow you to import SPSS files

library('Hmisc')

data = spss.get('Anxiety2.sav', use.value.labels = TRUE) # use your path, don't copy this directly
	# ignore warnings, data imports fine

# Examine summary statistics
summary(data) # easy way to simultaneously look at summary of all variables

# What if you want to look at a specific variable?
summary(data$subject); summary(data$trial3) # for example...

# Let's create a new variable, gender
data$gender = 0

data$gender[which(data$anxiety == "high")] = 1 # this creates a "1" for the values of which anxiety is "high"

# Examine correlations
cor(data[,4:7]) # Gives us correlation matrix between the trials

cor(data$gender,data[,4:7])

# Boxplots
boxplot(data[,4:7], main = "Boxplot of Trials")

# Given all this, you should be able to complete your first assignment!
#  Walter Reilly
# HW 3


# clear workspace
rm(list = ls())

# load libraries as needed
library(gmodels) # for certain planned comparisons
library(agricolae) # for scheffe post-hoc test


d = read.csv('lab2.csv')

# declare the factor (group) as a class variable
class(d$group)
d$group = as.factor(d$group)

# Calculate group means and save in the vector grpmeans
grpmeans = tapply(d$attended, d$group, mean)
grpmeans

# Use tapply( ) to create two vectors, 
# one for the sample size of each group and the other
# to include the group standard deviations

ngrp=tapply(d$attended,d$group,length)
sdgrp=tapply(d$attended,d$group,sd); 
ngrp
sdgrp

# replicate the one-way ANOVA from Lab 2
fit2 = aov(attended ~ group, data = d)
summary(fit2, intercept=T)

# First calculate all possible unadjusted pairwise comparisons
# Note: By dafault R will provide adjusted p-values
# Indicate those tests of comparisons that are statistically significant
pairwise.t.test(d$attended, d$group, p.adjust="none", pool.sd = T )

# 1 and 2, 1 and 3, 1 and 4, 2 and 4 are all
# statistically significant at an alpha of .05


# Control the family-wise Type I error rate by adjusting the p-values

# The first adjustment is Bonferroni
# compare these results to those from the unadjusted tests above
pairwise.t.test(d$attended, d$group, p.adjust="bonferroni", pool.sd = T)

# now only 1 and 3, and 1 and 4 are significant. 
# Interestingly the p-value for 1 and 4 increased slightly.

# The Bonferroni adjustment is conservative
# Perform the comparisons using Holm’s method
# How does Holm's method compare to Bonferroni for these data?
pairwise.t.test(d$attended, d$group, p.adjust="holm", pool.sd = T)

# Holm's method produces slightly less conservative results (smaller p-values)
# than bonferroni, but still more conservative than no correction 

# Use Tukey's method to test all possible mean pairs
# report on the difference in results based on Tukey's versus all other methods above
TukeyHSD(fit2, conf.level=.95)

# for Tukey's method those comparisons that were significant in Holm's method
# were also significant

# include in your summary a plot of the confidence intervals based on Tukey's method
plot(TukeyHSD(aov(d$attended~d$group), conf.level=.95))

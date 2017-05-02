# Walter Reilly
# 5.1.17

# clear workspace
rm(list = ls())

# load libraries as needed
library(gplots)

# Read in data 
library(haven)
d =  read_sav("~/walter/anova_204d/ch3labs2.sav")

# define factors as class variables
d$attract = as.factor(d$attract)
d$time = as.factor(d$time)

# -------------------------------------
# Descriptive Stats & Plotting of Means
# -------------------------------------

# DV = labs; IVs = attract, time
summary(d)

# plot to check for main effects, interaction 
interaction.plot(d$attract, d$time, d$labs, 
                 type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", 
                 lwd=2, pch=c(18,24,22), 
                 xlab="Attractiveness", 
                 ylab="Mean Number of Labs Attended", 
                 main="Interaction Plot"
)


# -----------------------------
# Fit the Factorial Anova Model
# -----------------------------

fit1 <- aov(labs ~ attract * time, data = d)
summary(fit1)
print(model.tables(fit1, type = "means"))

fit2 <- lm(labs ~ attract * time, data = d)
fit2a <- anova(fit2)
fit2a
p.adjust(fit1,method = "bonf")



# Report the results of the two-way ANOVA
# The 2 x 4 two-way ANOVA indicated a significant main effect of attractiveness, F(3,31) = 21.35, p < .001, 
# and a significant effect of time, F(1,31) = 61.79, p < .001, but no interaction, F < 1
# 
# Following Cramer et al. (2016), 
# evaluate the set of tests 
# (2 main effects and the interaction effect)
# using a family-wise error rate of .05

peas = fit2a$"Pr(>F)"
# First, use the sequential Bonferroni correction procedure
p.adjust(peas,method = "holm")
# Second, use standard Bonferroni correction procedure
p.adjust(peas,method = "bonf")
# Third, use Benjamini-Hochberg procedure (1995)
p.adjust(peas,method = "fdr")
# Compare the results from the 3 procedures 
# and describe why you might prefer one method 
# over another
# The standard bonferroni is overly conservative. I would never prefer it. I might choose FDR 
# if I would prefer higher power at the price of more false positives. This is the case in medical 
# testing. 
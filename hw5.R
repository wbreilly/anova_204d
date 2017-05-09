# Walter Reilly
# HW 5 

# PSC204d, Spring 2017
# Conduct and report on the following using ANCOVA

# requires data file ch4stateg.csv

# clean workspace
rm(list = ls())

# load libraries as needed
library(lattice) # for plotting
library(car) # for Type II SS and Type III SS
library(tidyverse)

# Read in d (replace path with your own)

d <- read.csv("ch4stateg.csv", header=TRUE, sep = ',')

# declare factor variable as class variable
d$group = factor(d$group, labels = c("traditional", "modern"))

# Descriptive Statistics

d1 <- d[d$group == "traditional", ]
d2 <- d[d$group == "modern", ]

# examine means, SDs, correlations, regression slopes for each group
summarise(d1, mean.quiz = mean(quiz),mean.aptitude= mean(aptitude),
          sd.quiz = sd(quiz), sd.aptitude = sd(aptitude))
cor(d1$quiz,d1$aptitude)
lm(quiz ~ aptitude, data = d1)

# group 2 - modern
summarise(d2, mean.quiz = mean(quiz),mean.aptitude= mean(aptitude),
          sd.quiz = sd(quiz), sd.aptitude = sd(aptitude))
cor(d2$quiz,d2$aptitude)
lm(quiz ~ aptitude, data = d2)

# examine means, SDs, correlations, and slopes across all
summarise(d, mean.quiz = mean(quiz),mean.aptitude= mean(aptitude),
          sd.quiz = sd(quiz), sd.aptitude = sd(aptitude))
cor(d$quiz,d$aptitude)
lm(quiz ~ aptitude, data = d)
      
      
      # ----------------------------------------
      # plot outcome by covariate for each group
      # ----------------------------------------
      
      colrs = c("blue", "firebrick3")
      
      xyplot(quiz ~ aptitude, # outcome ~ predictor
             data = d, 
             groups = group, # different lines, colors for each group
             type = c("p", "r"), # points & regression line for each group
             pch = c(16, 17), # symbols to use for points (2 types)
             cex = 1.1, # size of symbols
             lwd = 2, # line width
             col = colrs, # colors to use for plotting lines
             main = list(label = "Quiz Score by Method", # Title properties
                         fontface = "plain", 
                         fontsize = 12
             ),
             xlab = list(label = "Aptitude", # x-label properties
                         fontface = "plain",
                         fontsize = 12
             ),
             ylab = list(label = "Score", # y-label properties
                         fontface = "plain",
                         fontsize = 12
             ),
             key = list( # create a key
               corner = c(.85, .15), # where on the chart to place the key (x,y)
               title = "Teaching Method", # title of the key
               cex.title = 1.1, # font size for title of key
               col = colrs, # font colors
               points = list(pch = c(16, 17), # use same types here as above
                             col = colrs, # use same colors here as above
                             cex = 1.1 # size of the symbols
               ),
               text = list(levels(d$group), # labels to use for groups
                           cex = 1.0, # size of text
                           col = colrs # use same colors as above
               ),
               padding.text = 1.4
             )
      )
      
      
      # -------------------------------
      # fit the ANCOVA model using lm()
      # -------------------------------
      
      # Homogeneity of Slopes Test (Includes Interaction)
      fit0 <- lm(quiz ~ group * aptitude, data = d)
      summary(fit0)
      summary.aov(fit0)
      
      # ANCOVA proper (no interaction)
      fit1 <- lm(quiz ~ group + aptitude, data = d)
      summary(fit1)
      summary.aov(fit1)
      
      # if you want type 2 or type 3 sums of squares
      Anova(fit1, type="II")
      Anova(fit1, type="III")
      
      # compare to ANOVA (no covariate)
      fit2 <- lm(quiz ~ group, data = d)
      summary(fit2)
      summary.aov(fit2)
      
      # is model reduction justified? 
      # (e.g., can we remove the covariate?)
      # No, the covariate aptitude is an important and significant (!) predictor of quiz score. 
      # The type III anova indicates that it explains unique variance from group. ALthough with a sample
      # size like this dataset has I wouldn't bother with parametric or inferential statistics statistics.
      # 
      anova(fit1, fit2)
      
      # another way to test need for terms using step()
      fit3 <- lm(quiz ~ group * aptitude, data = d)
      step(fit3)
      
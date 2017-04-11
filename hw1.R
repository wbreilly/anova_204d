# Walter Reilly
# 4.10.17 
# HW 1

library(tidyverse)
#3
d = read.delim("~/walter/anova_204d/22081-0001-Data.tsv")

#4
d = d %>% mutate(female = V1102 - 1)

d %>% summarise(mean = mean(female), SD = sd(female))
# the mean is .5, indicating there are just as many females as males, 238 of each

#5
d$female = as.factor(d$female)
d %>% group_by(female) %>%  
  summarise(mean = mean(DEPRESS, na.rm = TRUE), SD = sd(DEPRESS, na.rm = TRUE))
# female     mean        SD
# <fctr>    <dbl>     <dbl>
# 1      0 1.909664 0.4981306
# 2      1 1.905063 0.4983614

#6
d %>% group_by(female) %>%  
  summarise(mean = mean(ANX2, na.rm = TRUE), SD = sd(ANX2, na.rm = TRUE))
# female     mean        SD
# <fctr>    <dbl>     <dbl>
# 1      0 2.109874 0.6131103
# 2      1 2.259283 0.7285043

#7
 dfemale = d %>% filter(gender == 1)  
cor.test(dfemale$DEPRESS,dfemale$ANXIETY)
# cor 
# 0.3072
dmale = d %>% filter(gender == 0)  
cor.test(dmale$DEPRESS,dmale$ANXIETY)
cor 
# 0.2128101 

#8
p <- ggplot(d, aes(factor(female), ANXIETY))
p + geom_boxplot()

#9


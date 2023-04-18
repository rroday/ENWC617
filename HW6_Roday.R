##
## Date Created: 2023-03-20
##
## Copyright (c) Rachel Roday, 2023
## Email: rroday@udel.edu
##
##---------------------------
##
##  Notes: ENWC 617 HW 6
##        
##        
##        
##---------------------------

# Q01
library(dplyr)
data(CO2)
view(CO2)
CO2.n <- CO2 %>% filter(conc == 95)

# Q02
library(ggplot2)
ggplot(CO2.n, aes(y=uptake, x=Treatment))+
  geom_jitter(alpha = .5, width = .05) +
  theme_bw()

# Q03
t.test(uptake ~ Treatment, data = CO2.n, var.equal = TRUE)

# Q04
wilcox.test(uptake ~ Treatment, data = CO2.n)

# Q05
# Since the data are parametric (normally distributed), the wilcoxon rank sum test results in a
# warning message. Thus a student's t test is more appropiate

# Q06
data("InsectSprays")
view(InsectSprays)
# ANOVA would be the best type of analysis to run because there are more than 
# 2 groups that are being compared. THere are counts from 6 different groups.
# T tests compare two samples populations.

# Q07
ggplot(InsectSprays, aes(x=count))+
  geom_histogram(fill = "#Ae95c9", color = "black",
                 boundary = 0, closed = "left",
                 binwidth = 1) +
  facet_wrap(~spray, ncol = 1, scales = "free_y", strip.position = "right") +
  theme_bw()


# Q08
data(msleep)
view(msleep)

msleep.n <- msleep %>% 
  group_by(order) %>%
  filter(length(name) > 10)

view(msleep.n)

# Checking answer
msleep %>% group_by(order) %>% summarise(count = length(name))

# Q09
# TRUE/FALSE. Consider msleep.n from the previous question. Determining whether
# mean bodywt varies among levels of order should be done by conducting with an 
# ANOVA with no data transformation needed. Why? (2pts)

# TRUE. ANOVA calculates the variance between samples and groups and in doing so
# finds the difference between an obervation and the sample mean, so we dont
# need to find the group (bodywt) mean first because ANOVA does this for us

# Q10
mod <- lm(bodywt ~ order, data = msleep.n)
anmod <- anova(mod)

mod.n <- lm(log(bodywt) ~ order, data = msleep.n)
anmod.n <- anova(mod.n)

# Q11
anmod.n$`F value`[1] /anmod$`F value`[1]

# Q12
anmod.n$`Pr(>F)`[1] /anmod$`Pr(>F)`[1]

# Q13

TukeyHSD(aov(mod.n))





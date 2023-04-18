##
## Date Created: 2023-02-13
##
## Copyright (c) Rachel Roday, 2023
## Email: rroday@udel.edu
##
---------------------------
##
##  Notes: ENWC 617 HW 1
##        
##         
##        
---------------------------
  

install.packages("vegan")
  
# Load Libs
library(vegan)
library(ggplot2)
library(dplyr)

# Go! Get! Your! Data!
data(BCI)
data(BCI.env)
# What does it look like?
head(BCI)
summary(BCI)
head(BCI.env)
table(BCI)
?BCI

# Question 1
# BCI stands for the Barro Colorado Island tree counts, in which 50 plots of land have
# counts of trees. BCI contains the total species across all 50 plots (for a total
# of 225 species (225 columns) with the Latin name as the column name). 
# BCI.Env gives the environmental conditions (precip, elevation, etc) for all 50 plots.

# Question 2
ggplot(BCI.env, aes(x=fct_infreq(Habitat)))+
  geom_bar(fill = "#a3313c", color="black")+
  labs(y="Frequency", x="Habitat") +
  theme_bw()

# Question 3
ggplot(BCI.env, aes(x=EnvHet))+
  geom_histogram(fill = "#a3313c", color = "black", binwidth = .08, closed = "left", boundary = 0)+
  labs(y="Number of Plots", x="Environmental Heterogeneity") +
  theme_bw()
# I chose .08 as a binwidth so that we could see the binomial distribution of the data

# Question 4
bci.1.n <- data.frame(species = names(BCI[1, ]), abundance = as.numeric(BCI[1, ]))
bci.1 <- bci.1.n[!bci.1.n$abundance == 0, ]
ggplot(bci.1, aes(x=abundance))+
  geom_histogram(fill = "violet", color = "black", binwidth = 5, closed = "left", boundary = 0)+
  labs(y="Frequency", x="Abundance") +
  theme_bw()

# Question 5
# The shape of the distribution from the Q5 histogram is right skewed

# Question 6
# There are more of the rare species at Site 1. (The frequency of occurrence for
# species that only showed up 0-5 times is higher than those that showed up 5-25 times)

# Question 7
ggplot(bci.1, aes(x=log10(abundance)))+
  geom_histogram(fill = "violet", color = "black", binwidth = .1, closed = "left")+
  labs(y="Frequency", x="Abundance") +
  theme_bw()
# The dsitribution doesnt really change all that much

# Question 8
unique(BCI.env$Age.cat)
# There are 2 unique age categories; C2, and C3
BCI.env %>% 
  group_by(Age.cat) %>% 
  count()
#C3 is more common

# Question 9
ggplot(BCI.env, aes(x=Stream))+
  geom_bar(fill = "skyblue", color="darkblue")+
  labs(y="Frequency", x="Stream Presence")+
  theme_bw()
# Most plots of land do not have streamside habitat

# Question 10
ggsave("HW1_StreamPlot.png",
  plot = last_plot(),
  width = 6,
  height = 5,
  units = "in",
  dpi = 300,
  path = "C:/Users/RER/Documents/Masters UD/Classes/Spring 2023/ENWC 617 Intro to Quantitative Ecology")

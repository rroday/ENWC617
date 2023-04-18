## Guided Practice 1
## Feb 8 2023
## Rachel Roday (c)

# Load libs
library(ggplot2)
library(tidyverse)


## Categorical Data
head(msleep)

table(msleep$order)
table(msleep$order)[order(table(msleep$order), decreasing = TRUE)]
addmargins(table(msleep$order)[order(table(msleep$order), decreasing = TRUE)])


table(msleep$vore)
addmargins(table(msleep$vore)[order(table(msleep$vore), decreasing = TRUE)])


as.data.frame(msleep)

# Make Bar Graph (y = count) 
ggplot(data = msleep, aes(x = fct_rev(fct_infreq(order)))) + #reorder x axis according to count 
  geom_bar(fill = "dodgerblue" , color = "black") +
  xlab("") +
  ylab("Number of species") + 
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## Continuous Data
msleep$sleep_total
table(cut(msleep$sleep_total, breaks = seq(0, 24, by=4), right = FALSE)) # cut() makes bins
addmargins(table(cut(msleep$sleep_total, breaks = seq(0, 24, by=4), right = FALSE))) # add margins adds sums

msleep$sleep_rem
table(msleep$sleep_rem)
addmargins(table(cut(msleep$sleep_rem, breaks = seq(0, 7, by=1), right = FALSE)))

## Make histogram
ggplot(data = msleep, aes(x = sleep_total)) +
  geom_histogram(fill = "lightgray", color = "black", binwidth = .5, closed = "left", boundary = 0) + #without boundary=0, bins go to negative numbersr
  xlab("Sleep total (hrs)") +
  ylab("Number of species") +
  theme_classic(base_size = 14) +
  scale_x_continuous(breaks = seq(0,20, by =2))

## symmetrical? Yeah sorta


## Skewed distributions
## We use log10 transformation to view the distribution better
ggplot(data = msleep, aes(x = log10(bodywt))) +
  geom_histogram(fill = "dodgerblue", color = "black", binwidth = 1, closed = "left",
                 boundary = 0) +
  xlab("Body weight (kg)") +
  ylab("Number of species") +
  theme_classic(base_size = 14)


## Extra plotting practice
ggplot(msleep, aes(x=fct_infreq(order)))+
  geom_bar(fill = "dodgerblue", color = "black") +
  scale_y_continuous(name="Frequency", limits=c(0,25)) +#breaks=seq(0,50, by=5)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size =8))

# This is a left skewed relationship
ggplot(msleep, aes(brainwt))+
  geom_histogram(fill = "violet", color="black")+
  theme_bw()
ggplot(msleep, aes(log10(brainwt)))+
  geom_histogram(fill = "violet", color="black")+
  theme_bw()
# Log transforming DOES make t he data symmetrical

## Next challenge
pop_sim <- data.frame(var1 = rnorm(n = 1000, mean = 20, sd = 12))

ggplot(pop_sim, aes(x = var1)) +
  geom_histogram(fill = "dodgerblue", color = "black", binwidth = 5, closed = "left", boundary = 0) +
  ylab("Frequency") +
  xlab("Variable of interest") +
  theme_classic()

random_sample <- data.frame(var1 = sample(pop_sim$var1, size = 20))

ggplot(random_sample, aes(x = var1)) +
  geom_histogram(fill = "dodgerblue", color = "black", binwidth = 5, closed = "left", boundary = 0) +
  ylab("Frequency") +
  xlab("Variable of interest") +
  theme_classic()
## These samples are NOT similar to the OG population.
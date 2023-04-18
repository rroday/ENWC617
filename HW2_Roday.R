## Rachel ROday  Feb 21 2023 (C)  
## ENWC 617 HW 2



# Q1
beaver1.n <- beaver1[beaver1$day == 346, ]
head(beaver1.n)

library(ggplot2)
ggplot(beaver1.n, aes(x = time, y= temp)) +
  geom_point() +
  geom_line(size = 1)+
  labs(x = "Time (hrs)", y = "Temperature (C)") +
  theme_bw()

# Q2
#install.packages("lme4")
library(lme4)
data(cbpp)
head(cbpp)

ggplot(cbpp, aes(x = period, y = incidence/size, group = herd)) +
  geom_point(aes(size = size)) +
  geom_line() +
  theme_bw() +
  labs(x = "Period", y = "Proporiton of herd infected", size = "Herd size") 
# Not sure how to specify dimensions in ggplot but just adjust your plot window size and it'll match

# Q3
#No. To better viszualize this, we can facet by herd number. Herd 8 clearly has one observation, Herd 2 also only has 3 observations, not 4.
ggplot(cbpp, aes(x = period, y = incidence/size, group = herd)) +
  geom_point(aes(size = size)) +
  geom_line() +
  theme_bw() +
  labs(x = "Period", y = "Proporiton of herd infected", size = "Herd size") +
  facet_wrap(~herd)


# Q4
data(msleep)
msleep.n <- msleep[!is.na(msleep$brainwt), ]
head(msleep.n)
ggplot(msleep.n,  aes(y=log10(brainwt), x=log10(bodywt)))+
  geom_point() +
  facet_wrap(~order)+
  theme_bw() +
  labs(x = "Body Weight", y = "Brain Weight")

# Q5
# No because there is only one datum point. We need more data to determine the relationship between the explanatory and respose variables.

# Q6
# There is a positive relationship between body weight and brain weight for members in the order Primate
# I believe there is a typo and this should say "In Question 4" not "question 3" since question 3 deals with herd sizes.

# Q7
msleep.nn <- msleep[!is.na(msleep$conservation), ]
head(msleep.nn)
ggplot(msleep.nn, aes(x=order, fill= conservation))+
  geom_bar(position="stack")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=.5))+
  labs(x = "", y="Number of species", fill = "conservation \nstatus") +
  scale_fill_grey()

# Q8
data(diamonds)
head(diamonds)
ggplot(diamonds, aes(x =carat, y= price))+
  geom_point(shape = 1)+
  labs(x = "Carat", y= "Price (USD)") +
  theme_bw()

# Q9
# Unfortunately, you cannot buy a 2 carat diamond for $1000 or less. The diamond market is a scam, though.

# Q10
data(ToothGrowth)
head(ToothGrowth)
?ToothGrowth

ggplot(ToothGrowth, aes(x= dose, y=len))+
  geom_jitter(width = .05, shape =1, size =2, aes(color = supp))+
  scale_colour_manual(values=c("orange","lightblue"))+
  theme_classic() +
  labs(color = "Supplement type", x= "Dose of vitamin C (mg/day)", y="Odontoblast length")


# Q11
# It really doesnt look like OJ or VC supplement has any impact on odontobalst length. If we make a boxplot at 2mg/day, we can see the spread of the data. The median is the same for both supplmetns, but VC has a larger range of data.
library(dplyr)
ToothGrowth %>%
  filter(dose == 2) %>%
  ggplot( aes(x= dose, y=len, group = supp, color = supp))+
  geom_boxplot() +
  scale_colour_manual(values=c("orange","lightblue"))+
  theme_classic() +
  labs(color = "Supplement type", x= "Dose of vitamin C (2 mg/day)", y="Odontoblast length") +
  scale_x_discrete(breaks = seq(2,2))


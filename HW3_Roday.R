##
## Date Created: 2023-02-28
##
## Copyright (c) Rachel Roday, 2023
## Email: rroday@udel.edu
##
---------------------------
  ##
  ##  Notes: ENWC 617 HW 3
  ##        
  ##         
  ##        
  ---------------------------

#Q01
library(ggplot2)
data("msleep")
conservation.n <- msleep$conservation[!is.na(msleep$conservation)]

#Q02
c.df <- data.frame(conservation.n)
ggplot(c.df, aes(x=conservation.n)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Conservation Status", y="Number of Species")


#Q03
library(dplyr)
msleep %>% filter(conservation == "en" | conservation == "nt")
# OR base R solution 
msleep.filter <- msleep[msleep$conservation == "nt" | msleep$conservation == "en" ,]

#Q04
msleep %>% filter(!conservation == "domesticated")
# OR base R solution 
msleep.filter2 <- msleep[!msleep$conservation == "domesticated",]

#Q05
# If I'm expected to use ggplot (in the tidyverse) to plot...I will also use dplyr to tidy the data. My brain doesnt work in Base R anymore. Im sorryyyy
library(tidyverse)
msleep.filter3 <- msleep %>% drop_na(sleep_rem)
ggplot(msleep.filter3, aes(x = sleep_total, y=sleep_rem, color=log10(bodywt))) +
  geom_point() +
  labs(x = "Total sleep (hrs)", y = "Total REM sleep (hrs)", color = "Log 10 \nBody Weight")+
  theme_bw()

#Q06
vector <- c(2, 10, 3, -56, 12, 0, 1, 3.4)
logical.vector <- c(vector <=4)


#Q07
my_list <- list(x = 1:10, y = c("cat", "dog", "mouse", "buzzard"), z = list(c(1, 3, 8), 6))
new.vector <- my_list$y[2]

#Q08
df <- msleep %>% filter(!conservation %in% NA)
df <- msleep %>% filter(!conservation == NA)

# The == operator treats NAs as a logical argument. == directly compares two elements to see if they are exactly equal to one another. %in% matches values in different vectors, such that you can have 2 vectors of different length that are being compared and matched. In the case of NAs, %in% matched which position there was a NA in the conservation column, whereas == found when a nonexistant value exactly matched NA (which basically made my dataframe self destruct)
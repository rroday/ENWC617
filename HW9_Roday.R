##
## Date Created: 2023-05-02
##
## Copyright (c) Rachel Roday, 2023
## Email: rroday@udel.edu
##
##---------------------------
##
##  Notes: ENWC 617 HW 7
##        
##        
##        
##---------------------------

# Q01
setwd("~/Masters UD/Classes/Spring 2023/ENWC 617 Intro to Quantitative Ecology")

df <- read.csv("Kranke Data Public.csv")


# Q02
library(tidyverse)


Q02 <- df %>%
  filter(!PH_Lin1 == "?") %>%
  group_by(Species) %>%
  filter(PH_Lin1 == unique(PH_Lin1)) %>%
  mutate_all(~na_if(., '')) %>%
  filter(!is.na(PH_Lin1)) %>%
  mutate(Num.of.parasite.spp = n())



# Q03
Q03 <-  df %>%
 # mutate_all(~na_if(., '')) %>%
 # filter(!PH_Lin1 == "?") %>%     
  group_by(Species) %>%
  filter(!duplicate == 0) %>%
  mutate(n.indiv = n()) %>%
  filter(n.indiv > 9) %>%
  mutate(inf = ifelse(PH_Lin1 == "", 0, 1)) %>%
  summarise(total.inf = sum(inf),
            inf.prop = total.inf/n.indiv)


fulldf <- left_join(distinct(Q03), Q02, by = "Species")

ggplot(fulldf, aes(x= inf.prop, y= Num.of.parasite.spp))+
  geom_point()+
  labs(x="Prevalence", y="Number of Parasite Species") +
  theme_bw()


# Q04
Q04 <- Q03 %>%
  filter(inf.prop == 0)

length(unique(Q04$Species))     
# "Alcedo atthis"     "Dendrocopus major" "Regulus regulus"   "Sturnus vulgaris" 





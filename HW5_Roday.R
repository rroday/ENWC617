##
## Date Created: 2023-03-20
##
## Copyright (c) Rachel Roday, 2023
## Email: rroday@udel.edu
##
##---------------------------
##
##  Notes: ENWC 617 HW 5
##        This HW took 20 minutes
##        
##        
##---------------------------

library(ggplot2)
library(dplyr)

data(msleep)
head(msleep)


# Q01
msleep %>% filter(order == "Rodentia", sleep_total > 1)

# Q02
diamonds %>% 
  group_by(cut) %>%
  summarise(max_cut = max(price))

# Q03
storms
length(unique(storms$year))

# Q04
storms %>% 
  group_by(status) %>%
  summarise(avg.max.ws = mean(wind))

# Q05
storms %>%
  group_by(status)%>%
  summarise(avg.max.ws = mean(wind),
            range.ws = max(wind)-min(wind)) 

# Q06
msleep.n <- msleep %>%
  select(name, vore, sleep_total) %>%
  rename("Diet" = "vore") %>%
  filter(Diet %in% c("carni","herbi"))


# Q07
msleep.nn <- msleep %>%
  select(name, vore, sleep_total) %>%
  rename("Diet" = "vore") %>%
  filter(Diet %in% c("carni","herbi")) %>%
  mutate(log_sleep_total = log(sleep_total))

ggplot(msleep.nn, aes(x = sleep_total, y=log_sleep_total))+
  geom_point() +
  theme_bw() +
  labs(x = "Total Sleep" ,y = "Total Sleep (natural log)")

# Q08
msleep.n3 <- msleep.nn %>%
  mutate(Diet.n = recode(Diet, "carni" = "Carnivore", "herbi" = "Herbivore"))

ggplot(msleep.n3, aes(x=Diet.n, y=sleep_total))+
  geom_jitter(width=.1) +
  theme_bw()+
  labs(x="", y = "Sleep (hrs)")


# Q09
my_dat <- data.frame(aou = c("SUTA", "REVI", "DOWO", "BWWA", "OVEN", "WEVI"), 
                     n = c(2, 6, 1, 1, 4, 1))

species_info <- data.frame(species = c("REVI", "WEVI", "SCTA", "SUTA", 
                                       "YBCH", "NOPA", "PIWO", "DOWO", "AMGO",
                                       "PRAW", "MODO", "BGGN"), 
                           incubation_period = c(13, 14, 13, 11.5, 11, 13, 16.5,
                                                 12, 13, 12.5, 14, 13))

new.df <- left_join(my_dat, species_info, by = c("aou" = "species"))
view(new.df)

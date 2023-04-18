# ENWC 617 
# Rachel Roday 
# 3/20/23
# GP5

library(dplyr)

msleep %>%
  filter(order == "Carnivora")

msleep[msleep$order == "Carnivora", ]


msleep %>%
  filter(conservation == "vu" | conservation == "nt")
filter(msleep, conservation %in% c("vu","nt"))

filter(msleep, order == "Carnivora", conservation == "vu")

1:10 %>%
  mean() %>% 
  exp()


msleep %>%
  filter(order == "Carnivora") %>%
  filter(sleep_total > 10) %>%
  filter(!is.na(sleep_rem))

select(msleep, genus, order, sleep_total)

select(msleep, -genus, -order, -sleep_total)

msleep %>%
  filter(brainwt > 0.01) %>%
  select(name, vore, sleep_rem, brainwt)

msleep %>%
  select(name:sleep_total)
msleep %>%
  select(-(name:sleep_total))


rename(msleep, species = name)
rename(msleep, species = name, GENUS = genus, ORDER = order)

mutate(msleep, sleep_rel = sleep_total / 24)

msleep.n <- msleep %>%
  mutate(sleep_rel = sleep_total / 24) %>%
  select(name, order, sleep_total, sleep_rel)


msleep %>%
  mutate(sleep_rel = sleep_total / 24,
         sleep_rem.n = sleep_rem * awake,
         cons_nt = conservation == "nt")

msleep %>%
  mutate(brainwt.log = log10(brainwt))

msleep %>% 
  filter(order == "Chiroptera") %>% 
  select(name, sleep_total, bodywt) %>% 
  mutate(sleep_total_rank = min_rank(sleep_total))

msleep %>%
  summarize(species_n = length(name),
            order_n = length(unique(order)),
            sleep_mean = mean(sleep_total))

msleep %>%
  group_by(order) %>%
  summarize(sleep_mean = mean(sleep_total),
            sleep_sd = sd(sleep_total),
            n = length(name))


msleep %>%
  group_by(order) %>%
  summarize(sleep_mean = mean(sleep_total),
            sleep_sd = sd(sleep_total),
            n = length(name)) %>%
  mutate(sleep_se = sleep_sd / sqrt(n))


diamonds %>% 
  group_by(cut, clarity) %>% 
  summarize(n = length(cut), 
            price_mean = mean(price))

ChickWeight %>%
  group_by(Diet) %>%
  summarize(weight_mean = mean(weight),
            weight_sd = sd(weight))

ChickWeight %>%
  group_by(Diet, Time) %>%
  summarize(weight_mean = mean(weight),
            weight_sd = sd(weight)) %>%
  arrange(Time)


my_dat <- data.frame(Species = c("SCTA", "SCTA", "NOCA", "NOPA", "YBCH", "SCTA",
                                 "REVI", "WEVI", "NOCA", "WOTH", "REVI", "REVI",
                                 "AMRO", "AMRO", "HOSP", "NOCA", "HOSP", "NOCA",
                                 "AMRO", "AMGO", "HOSP", "NOCA", "NOCA", "HOSP",
                                 "AMRO", "AMRO", "HOSP", "EUST", "EUST", "HOSP"),
                     Site = c(rep("Site1", 12),
                              rep("Site2", 7),
                              rep("Site3", 11)))
my_dat.n <- my_dat %>%
  group_by(Site, Species) %>%
  summarize(n = length(Species))

## common name data
aou.names <- data.frame(Species = unique(my_dat$Species),
                        Common_Name = c("Scarlet tanager", "Northern cardinal",
                                        "Northern parula", "Yellow breasted chat",
                                        "Red eyed vireo", "White eyed vireo",
                                        "Wood thrush", "American robin",
                                        "House sparrow", "American goldfinch",
                                        "European starling"))

## join
my_dat.nn <- my_dat.n %>%
  left_join(aou.names, by = "Species")


aou.names.n <- aou.names %>%
  rename(aou_species = Species)

## join
my_dat.nn <- my_dat.n %>%
  left_join(aou.names.n, by = c("Species" = "aou_species"))


ggplot(my_dat.nn, aes(x = Common_Name, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Site, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.x = element_blank())




## data frame 1
new_dat <- data.frame(var1 = c("a", "a", "b", "c"),
                      var2 = c("dog", "dog", "fish", "turtle"),
                      var3 = c(2, 4, 26, 0.3))

## data frame 2
another_dat <- data.frame(var1 = c("a", "b", "c"),
                          var2 = c("dog", "fish", "gopher"),
                          var4 = c("big", "small", "medium"))

## join by var1 and var2
new_dat.n <- new_dat %>%
  left_join(another_dat, by = c("var1", "var2"))

band_members.n <- band_members %>%
  left_join(band_instruments2, by = c("name" = "artist"))


band_members.n %>%
  slice_sample(n = 2)

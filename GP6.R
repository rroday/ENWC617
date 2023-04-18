# GP 6
# 4/5/23
# R. Roday

## species richness data
fire <- structure(list(site = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 
                                6L, 6L, 7L, 7L, 8L, 8L, 9L, 9L, 10L, 10L), treatment = c("burned", 
                                                                                         "unburned", "burned", "unburned", "burned", "unburned", "burned", 
                                                                                         "unburned", "burned", "unburned", "burned", "unburned", "burned", 
                                                                                         "unburned", "burned", "unburned", "burned", "unburned", "burned", 
                                                                                         "unburned"), species_richness = c(8, 4, 16, 12, 5, 2, 26, 18, 
                                                                                                                           11, 6, 5, 6, 4, 4, 9, 7, 22, 19, 9, 2)), class = c("tbl_df", 
                                                                                                                                                                              "tbl", "data.frame"), row.names = c(NA, -20L))
library(ggplot2)
ggplot(fire, aes(x=factor(treatment, levels = c("unburned", "burned")), y=species_richness, group = site))+
  geom_point(size =2)+
  geom_line()+
  theme_classic()+
  labs(y="Flowering plant species richness", x="")


# t.test (response variable ~ explanatory variable )

t.test(species_richness ~ treatment, data = fire, paired = TRUE)

?t.test
# default of paired = FALSE

test <- t.test(species_richness ~ treatment, data = fire, paired = TRUE)
test$statistic

fire.w <- data.frame(burned = c(8, 16, 5, 26, 11, 5, 4, 9, 22, 9),
                     unburned = c(4, 12, 2, 18, 6, 6, 4, 7, 19, 2),
                     site = 1:10)


t.test(fire.w$burned, fire.w$unburned, paired = TRUE)

## calculate paired differences
fire.w.n <- fire.w %>%
  mutate(paired_diff = burned - unburned)

## plot
ggplot(fire.w.n, aes(x = paired_diff)) +
  geom_histogram(fill = "dodgerblue", color = "black",
                 binwidth = 2, closed = "left",
                 boundary = 0) +
  theme_classic()


#  An option for a non-parametric version of a paired t test is the Wilcoxon signed rank test.

wilcox.test(fire.w$burned, fire.w$unburned, paired = TRUE)


## T sample t test

chickwts.n <- chickwts %>%
  filter(feed %in% c("horsebean", "sunflower"))

## plot
ggplot(chickwts.n, aes(x = feed, y = weight)) +
  geom_jitter(size = 3, shape = 1, width = 0.05, height = 0) +
  theme_classic()

chickwts.n %>%
  group_by(feed) %>%
  summarize(mean_weight = mean(weight),
            sd_weight = sd(weight))


ggplot(chickwts.n, aes(x = weight)) +
  geom_histogram(fill = "dodgerblue", color = "black",
                 closed = "left", boundary = 0, binwidth = 20) +
  facet_wrap(~feed, ncol = 1, scales = "free_y", strip.position = "right") +
  theme_bw()

t.test(weight ~ feed, data = chickwts.n, var.equal = TRUE)

wilcox.test(weight ~ feed, data = chickwts.n)


# Welches two sample t test

t.test(weight ~ feed, data = chickwts.n)
# Differences are p value size, df, and t statistic


msleep.n <- msleep %>% filter(vore %in% c("carni", "herbi"))
t.test(sleep_total ~ vore, data = msleep.n)



ggplot(msleep.n, aes(x = vore, y = sleep_total)) +
  geom_jitter(size = 3, shape = 1, width = 0.05, height = 0) +
  theme_classic()

## stacked histogram
ggplot(msleep.n, aes(x = sleep_total)) +
  geom_histogram(fill = "dodgerblue", color = "black",
                 boundary = 0, closed = "left",
                 binwidth = 2) +
  facet_wrap(~vore, ncol = 1, scales = "free_y", strip.position = "right") +
  theme_bw()


####### ANOVA #######

ggplot(chickwts, aes(x=feed, y=weight))+
  geom_jitter(width=.1, alpha = .5)+
  theme_bw()

# Stacked hisotrgram
ggplot(chickwts, aes(x= weight))+
  geom_histogram(fill = "lightblue", color = "black",
                 boundary = 0, closed = "left",
                 binwidth = 40) +
  facet_wrap(~feed, ncol = 1, scales = "free_y", strip.position = "right") +
  theme_bw()


length(unique(chickwts$feed))



mod <- lm(weight ~ feed, data = chickwts)

anova(mod)


identical(anova(mod)$Mean[1] / anova(mod)$Mean[2], anova(mod)$F[1])


summary(mod)$r.squared


TukeyHSD(aov(mod))

msleep.nn <- msleep %>%
  filter(!is.na(vore))


ggplot(msleep.nn, aes(x = vore, y = log(sleep_total))) +
  geom_jitter(size = 3, shape = 1, width = 0.05, height = 0) +
  theme_classic()+
  labs(x='', y="Sleep total")

kruskal.test(sleep_total ~ vore, data = msleep.nn)
anova(lm(sleep_total ~ vore, data = msleep.nn))

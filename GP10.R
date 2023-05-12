# GP 10
# ENWC617
# 5/10/23
# Rachel Roday

library(dplyr)

## subset
chickwts.n <- chickwts %>%
  filter(feed %in% c("horsebean", "sunflower"))

## packages
library(ggplot2)

## plot
ggplot(chickwts.n, aes(x = feed, y = weight)) +
  geom_jitter(size = 3, shape = 1, width = 0.05, height = 0) +
  theme_classic()


t.test(weight ~ feed, data = chickwts.n, var.equal = TRUE)
# P value is significant


## calculate group means
groupMeans <- chickwts.n %>%
  group_by(feed) %>%
  summarize(mean_weight = mean(weight))

## calculate difference between group means
diff_obs <- groupMeans$mean_weight[1] - groupMeans$mean_weight[2]

# Run for loop to resample #########

## set parameters and empty vector
nrand <- 1000 # number of randomizations

rand_diff <- vector() # empty vector for storing differences under null hyp.

## for-loop
for(i in seq_len(nrand)){
  chickwts.n.rand <- chickwts.n %>%
    mutate(weight = sample(weight, replace = FALSE)) # shuffle weight
  groupMeans.r <- chickwts.n.rand %>%
    group_by(feed) %>%
    summarize(mean_weight = mean(weight)) # calculate mean weight by feed type
  rand_diff[i] <- groupMeans.r$mean_weight[1] - groupMeans.r$mean_weight[2]
}


ggplot(data.frame(rand_diff), aes(x = rand_diff)) +
  geom_histogram(color = "black", fill = "dodgerblue",
                 boundary = 0, closed = "left",
                 binwidth = 5) +
  xlab("Randomized differences in weight") +
  theme_classic()

ggplot(data.frame(rand_diff), aes(x = rand_diff)) +
  geom_histogram(color = "black", fill = "dodgerblue",
                 boundary = 0, closed = "left",
                 binwidth = 5) +
  xlab("Randomized differences in weight") +
  geom_vline(xintercept = diff_obs, color = "red") +
  theme_classic()

# Calculcate p value
sum(rand_diff <= diff_obs) / nrand



### Randomizing a correlation ####
x <- rnorm(100)

y <- x + rnorm(100, mean = 0, sd = 3)

dat <- data.frame(x, y)


## plot
ggplot(dat, aes(x = x, y = y)) +
  geom_point(size = 2, shape = 1) +
  theme_classic()

## correlation test
cor.test(dat$x, dat$y)

### Conduct a randomization rest on correlation coefffience 

## parameters and empty vector
nrand <- 1000
r_ran <- vector()

## for loop
for(i in seq_len(nrand)){
  r_ran[i] <- cor(dat$x, sample(dat$y, replace = FALSE))
}

## P value
r_obs <- cor(dat$x, dat$y) #observed r
(sum(r_ran >= r_obs) / nrand) * 2

## a more precise P value counting values in both tails of the distribution
sum(abs(r_ran) >= abs(r_obs)) / nrand

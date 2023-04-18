# Rachel Roday
# ENWC 617
# 4/12/23
# GP7

# Correlation
# Correlation coefficient r is between -1 and 1. 0 is no relation between x and y

## variable 1
x <- rnorm(n = 100)

## variable 2
y <- x + rnorm(n = length(x))

df <- data.frame(x,y)

ggplot(df, aes(x=x, y=y))+
  geom_point()

cor(x, y)

## update y
y <- x + rnorm(n = length(x), mean = 0, sd = 0.2)

## calculate correlation
cor(x, y)

## update y
y <- x + rnorm(n = length(x), mean = 0, sd = 50)

## calculate correlation
cor(x, y)

# we test the null hypothesis of r=0 using the t statistic
cor.test(x, y)
str(cor.test(x, y))


# Challenge
ggplot(data = msleep, aes(x=sleep_total, y=sleep_rem))+
  geom_point()

cor.test(msleep$sleep_total, msleep$sleep_rem)

msleep %>%
  filter(!is.na(sleep_total)) %>%
  filter(!is.na(sleep_rem)) %>%
  summarize(n_obs = length(sleep_total))

#Notice how on the right side of the plot, the variance of sleep_rem is higher than on the left side of the plot. This is known as heteroscedasticity.

## sleep_rem
ggplot(msleep, aes(x = sleep_rem)) +
  geom_histogram(binwidth = 0.5, fill = "dodgerblue", color = "black",
                 closed = "left", boundary = 0)
# This is right skewed

## sleep_total
ggplot(msleep, aes(x = sleep_total)) +
  geom_histogram(binwidth = 3, fill = "dodgerblue", color = "black",
                 closed = "left", boundary = 0)


## sleep_rem
ggplot(msleep, aes(x = log(sleep_rem))) +
  geom_histogram(binwidth = 0.5, fill = "dodgerblue", color = "black",
                 closed = "left", boundary = 0)
# Now its left skewed

ggplot(data = msleep, aes(x=sleep_total, y=log(sleep_rem)))+
  geom_point()
# better but not great

cor.test(msleep$sleep_total, log(msleep$sleep_rem))
# Better

# We can calculate and test the correlation using the non-parametric Spearman’s rank correlation test. 
cor.test(msleep$sleep_total, msleep$sleep_rem, method = "spearman")

storks <- read.csv(url("https://figshare.com/ndownloader/files/1263982"))

ggplot(storks, aes(x = Storks, y=Birth))+
  geom_point()
cor.test(storks$Storks, storks$Birth)
cor.test(storks$Storks, storks$Birth, method = "spearman")





# Linear Regression

lm(sleep_rem ~ sleep_total, data = msleep)

mod <- lm(sleep_rem ~ sleep_total, data = msleep)

summary(mod)
summary(mod)$r.squared
anova(mod)


ggplot(msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point(size = 3, shape = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_classic()


## calculate prediction interval
msleep.n <- msleep %>%
  select(sleep_total, sleep_rem) %>%
  filter(!is.na(sleep_total)) %>%
  filter(!is.na(sleep_rem)) %>%
  mutate(fit = predict(mod, interval = "prediction")[,1]) %>%
  mutate(lwr = predict(mod, interval = "prediction")[,2]) %>%
  mutate(upr = predict(mod, interval = "prediction")[,3])

## plot
ggplot(msleep.n, aes(x = sleep_total, y = sleep_rem)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), 
              fill = "black", alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, col = "black") +
  geom_point(size = 3, shape = 1) +
  theme_classic()



# Challenge
## load the vegan package
library(vegan)

## load data
data(BCI)
data(BCI.env)

## add species richness to BCI.env
BCI.env.n <- BCI.env %>%
  mutate(sp_rich = specnumber(BCI))

## linear regression
mod.bci <- lm(sp_rich ~ EnvHet, dat = BCI.env.n)
summary(mod.bci)


## plot
ggplot(BCI.env.n, aes(x = EnvHet, y = sp_rich)) +
  geom_point(size = 3, shape = 1) +
  xlab("Environmental heterogeneity") +
  ylab("Number of species") +
  geom_smooth(method = "lm", color = "black") +
  theme_classic()


## run regression model
mod.n <- lm(brainwt ~ bodywt, data = msleep)

## extract residuals
msleep.n <- msleep %>%
  select(brainwt, bodywt) %>%
  filter(!is.na(brainwt)) %>%
  filter(!is.na(bodywt)) %>%
  mutate(res = residuals(mod.n))

## residual plot
ggplot(msleep.n, aes(x = bodywt, y = res)) +
  geom_point(size = 3, shape = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()

ggplot(msleep.n, aes(x = bodywt, y = brainwt)) +
  geom_point(size = 3, shape = 1) +
  geom_smooth(method = "lm") +
  theme_classic()
#Indeed, these variables are strongly skewed. Let’s try log transforming bodywt first.


## run regression model
mod.nn <- lm(brainwt ~ log(bodywt), data = msleep)

## extract residuals
msleep.nn <- msleep %>%
  select(brainwt, bodywt) %>%
  filter(!is.na(brainwt)) %>%
  filter(!is.na(bodywt)) %>%
  mutate(res = residuals(mod.nn))

## plot data with regression line
ggplot(msleep.nn, aes(x = log(bodywt), y = brainwt)) +
  geom_point(size = 3, shape = 1) +
  geom_smooth(method = "lm") +
  theme_classic()

## residual plot
ggplot(msleep.nn, aes(x = log(bodywt), y = res)) +
  geom_point(size = 3, shape = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()

# The relationship between the variables appears to be non-linear and we can see in the residual plot that the data do not meet the assumptions - notice the strong negative relationship and the high variance in the residuals at high body weights.

## run regression model
mod.nnn <- lm(log(brainwt) ~ log(bodywt), data = msleep)
summary(mod.nnn)

## extract residuals
msleep.nnn <- msleep %>%
  select(brainwt, bodywt) %>%
  filter(!is.na(brainwt)) %>%
  filter(!is.na(bodywt)) %>%
  mutate(res = residuals(mod.nnn))

## plot data with regression line
ggplot(msleep.nnn, aes(x = log(bodywt), y = log(brainwt))) +
  geom_point(size = 3, shape = 1) +
  geom_smooth(method = "lm") +
  theme_classic()

## residual plot
ggplot(msleep.nnn, aes(x = log(bodywt), y = res)) +
  geom_point(size = 3, shape = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
# yay


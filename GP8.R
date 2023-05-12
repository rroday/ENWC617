# GP8
# Rachel Roday
# 4/26/23

# Binomial distribution

dbinom(x=3, size = 4, prob = .5)

plot(dbinom(1:8, 8, .2))


lyme <- data.frame(inf = 0:8, prob = dbinom(0:8, 8, prob = .2))

ggplot(lyme, aes(x=inf, y=prob))+
  geom_bar(stat = "identity")+
  theme_classic()


lyme2 <- data.frame(inf = (0:80)/80, prob = dbinom(0:80, 80, prob = .2))

ggplot(lyme2, aes(x=inf, y=prob))+
  geom_bar(stat = "identity")+
  theme_classic()


sefxn <- function(p, n){
  sqrt((p * (1-p))/n)
}


sefxn(p = .32, n = 10)


library(binom)

binom.confint(x=5, n=10, method = "ac")

binom.test(x=20, n=30, p =.5)


####### Chi sq ##########

50 * c(.6, .3, .1)

chisq.test(c(15,10,25), p = c(.6,.3,.1))




100 * c(.9995, .00025, .00025)

mean(100 * 100 * c(.995, .00025, .00025))
chisq.test(c(97,2,1), p = c(.9995, .00025, .00025))




dat.am<- matrix(c(16,34,23,12), nrow =2)

row.names(dat.am) <- c("inf", "uninf")
colnames(dat.am) <- c("HY", "AHY")

fisher.test(dat.am)

chisq.test(dat.am, correct = FALSE)

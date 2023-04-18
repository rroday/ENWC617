## ENWC 617
## Feb 08 2023

arth <- c("ant", "ant", "butterfly","bee", "tick", "tick","ant", "bee", "tick", "bee", "fly")
table(arth)

library(ggplot2)

arthdf<-as.data.frame(arth)
dev.off()

ggplot(data=arthdf, aes(x=arth)) +
  geom_bar(stat = "count")



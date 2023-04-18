# Rachel ROday  (C)
# Feb 19 2023

# GP2


#####    Categorical vs Categroical    #####

library(ggplot2)
radio_data <- data.frame(repro = c(rep("success", 7),
                                   rep("no success", 5),
                                   rep("success", 6),
                                   rep("no success", 4)),
                         radio = c(rep("no tag", 12),
                                   rep("tagged", 10)))
table(radio_data)
addmargins(table(radio_data))

ggplot(radio_data, aes(x = radio, group = repro)) +
  geom_bar(aes(fill=repro), position = "dodge") +
  labs(y= "Number of birds", x="Radio tag status", 
       fill = "Reproductive \nsuccess") +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("orange", "sky blue"))



ggplot(radio_data, aes(x = radio, group = repro)) +
  geom_bar(aes(fill = repro), position = "stack", color = "black") +
  scale_fill_grey(name = "Reproductive\nsuccess") +
  xlab("Radio tag status") +
  labs(fill = "") + 
  theme_classic(base_size = 12 ) +
  scale_y_continuous(name = "Number of birds",
                     breaks = c(0,2,4,6,8,10,12))



mosaicplot(t(table(radio_data)), col = c("lavender", "dodgerblue"),
           sub = "Radio tag status",
           xlab = "",
           ylab = "Relative frequency",
           main = "")


#install.packages("ggmosaic")
library(ggmosaic)


ggplot(radio_data, aes(x = radio, group = repro)) +
  geom_mosaic(aes(x = product(repro, radio), fill = repro)) +
  scale_y_continuous(breaks=c(0,1), name = "") +
  xlab("Radio tag status") +
  labs(fill = "Reproductive \nSuccess") +
  scale_fill_manual(values = c("#F0E442", "#CC79A7"))+
  theme_classic()

#####    Categorical vs Numerical    #####



data(PlantGrowth)


ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_jitter(shape = 1, width = 0.1, height = 0, size = 3,aes(color = group)) +
  theme_bw()+
  labs(x= "Treatments", y="Weight (g)", color = "Treatment")


ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_violin(fill = "dodgerblue") +
  stat_summary(fun = mean,  geom = "point", color = "black", size = 3)#+
  geom_jitter(shape = 1, width = 0.1, height = 0, size = 3)

  
  
ggplot(PlantGrowth, aes(x = group, y = weight)) +
   geom_boxplot(aes(fill = group)) +
   geom_jitter(shape = 6, width = 0.1, height = 0, size = 3)+
   theme_bw()+
   labs(x= "Treatments", y="Weight (g)", color = "Treatment")
  

### Means and confiendece interval plot ###

PlantGrowth.n <- data.frame(group = c("ctrl", "trt1", "trt2"),
                            weight.mean = c(5.03, 4.66, 5.53),
                            weight.se = c(0.184, 0.251, 0.140))

ggplot(PlantGrowth.n, aes(x = group, y = weight.mean)) +
  geom_point(size = 2, aes(fill = group)) +
  geom_errorbar(aes(ymax = weight.mean + weight.se, ymin = weight.mean - weight.se),
                width = .2, size = 1) +
  theme_bw()+
  labs(x= "Treatments", y="Weight (g)", color = "Treatment", fill = "Treatments")


### multiple histograms plot ###

ggplot(PlantGrowth, aes(x = weight)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "left", fill = "dodgerblue", color = "black") +
  facet_wrap( ~ group, ncol = 1, scales = "free_y", strip.position = "left") +
  scale_x_continuous(breaks = c(3.5, 4, 4.5, 5, 5.5, 6, 6.5), name = "Plant weight") +
  ylab("Number of plants") +
  theme_classic()


## Logistic regression stle plot ###


sheep <- data.frame(age = c(0.3, 0.31, 0.55, 0.59, 0.73, 0.86, 1.12, 1.20, 1.20, 1.31, 1.38, 1.46, 1.88, 2.1),
                    infected = c(0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1))

ggplot(sheep, aes(x = age, y = infected)) +
  geom_jitter(shape = 1, width = 0, height = 0.07, size = 3) +
  theme_bw()+
  labs(x= "Age (yr)", y=" Infected sheep") +
  scale_y_continuous(breaks = c(0,1))



######    Numerical vs Numerical   ########



data(trees)
head(trees)


ggplot(trees, aes(x = Girth, y = Height)) +
  geom_point()
# Positive relationship


ggplot(trees, aes(x = Volume, y = Height)) +
  geom_point()


ggplot(trees, aes(y = Volume, x = Girth)) +
  geom_point(color = "white") +
  theme_dark()+
  labs(y = "Volume of tree (cm^3)", x = "Girth of tree (cm)")


# Volume vs grith is strongest relationship
# Never add jitter to scatter plot


## Line Plot   ##

data(lynx)

lynx.n <- data.frame(captures = as.numeric(lynx),
                     year = seq(1821, 1934, by = 1))

ggplot(lynx.n, aes(x = year, y = captures)) +
  geom_point() +
  geom_line(size=2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1820, 1940, by=10))


####### adding additional variable   ######
ggplot(msleep, aes(x = log10(bodywt), y = sleep_total)) +
  geom_point()

msleep.n <- msleep[!is.na(msleep$vore), ]

ggplot(msleep.n, aes(x = log10(bodywt), y = sleep_total)) +
  geom_point(aes(shape = vore))
ggplot(msleep.n, aes(x = log10(bodywt), y = sleep_total)) +
  geom_point(aes(color = vore))
ggplot(msleep.n, aes(x = log10(bodywt), y = sleep_total)) +
  geom_point(aes(color = vore, shape = vore))



ggplot(msleep.n, aes(x = log10(bodywt), y = sleep_total)) +
  geom_point() +
  facet_wrap(~vore) +
  theme_light() +
  labs(y = "Total Sleep", x = "Body Weight")


data(ChickWeight)

head(ChickWeight, n = 24)


ggplot(ChickWeight, aes(x = Time, y = weight, group = Chick)) +
  geom_point() +
  geom_line() +
  facet_grid(~Diet)

ggplot(trees, aes(x = Girth, y = Volume)) +
  geom_point(aes(color = Height), size = 3)

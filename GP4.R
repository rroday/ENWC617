# ENWC 617 
# Rachel Roday 
# 3/6/23
# GP4

######## Subsetting ########
x <- data.frame(var1 = c(1, 2, 4, NA), var2 = c(1, 3, 4, 5))

x[x$var1 ==2,]
x[which(x$var1 ==2),]
# USing the which function, better evalutes NAs vs actual values 


x[x$var1 %in% 2,]
x[x$var1 == 2 | is.na(x$var1),]

x[x$var1 != 2,]

x[x$var1 != 2 & !is.na(x$var1),]


########## Factors ##########

fact1 <- factor(c("dog", "goat", "bat"), levels = c("goat", "dog", "bat")) # a factor

fact2 <- factor(c("dog", "goat", "bat"), levels = c("bat", "dog", "goat")) # change order of factor level

identical(fact1, fact2)

bird_survey <- factor(c("REVI", "WOTH", "REVI", "WEVI", "NOPA", "REVI", "SCTA", "WEVI", "REVI", "REVI", "WOTH", "EAWP", "BWWA", "REVI"))
bird.df <- data.frame(bird_survey)
library(ggplot2)
ggplot(bird.df, aes(x=bird_survey))+
  geom_bar()

birds.levels <- c("REVI", "WEVI", "WOTH", "BWWA", "EAWP", "NOPA", "SCTA") # vector of levels in order

bird_survey.n <- factor(bird_survey, levels = birds.levels)
bird.sf2 <- data.frame(bird_survey.n)

ggplot(bird.sf2, aes(x=bird_survey.n))+
  geom_bar()

library(forcats)

fct_infreq(bird_survey)
birds.fct <- fct_recode(bird_survey, "Red-eyed Vireo" = "REVI", "White-eyed Vireo" = "WEVI", "Wood Thrush" = "WOTH", "Black-and-white Warbler" = "BWWA", "Eastern Wood Peewee" = "EAWP", "Northern Parula" = "NOPA", "Scarlet Tanager" = "SCTA")
birds.fct2 <- data.frame(fct_infreq(birds.fct))

ggplot(birds.fct2, aes(x=fct_infreq.birds.fct.))+
  geom_bar()+
  xlab("") +
  ylab("Number of bird detections")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


factor(c("fork", "knife", "spoon", "spoon", "Knife"), levels = c("fork", "knife", "spoon"))


####### Functions ########

mean(x = 1:10)
mean(1:10)

mean(c(1, 2, 3, NA), na.rm = TRUE) # removes the NA before calculation of the mean

mean(c(1, 2, 3, NA), TRUE) # gives an error because R thinks TRUE refers to the second argument "trim" which takes a number between 0 and 0.5

## plot histogram of normal distribution with mean = 0, sd = 1
x.sd1 <- rnorm(n = 1000, mean = 0, sd = 1)
ggplot(data.frame(x.sd1), aes(x = x.sd1)) +
  geom_histogram(closed = "left", boundary = 0, binwidth = 0.5, fill = "dodgerblue", color = "black")


x.sd10 <- rnorm(n=1000, mean = 0, sd=10)
ggplot(data.frame(x.sd10), aes(x=x.sd10))+
  geom_histogram(closed="left",boundary =0, binwidth = 5, color = "black",fill ="red")

x.sd100 <- rnorm(n=1000, mean = 0, sd=10)
ggplot(data.frame(x.sd100), aes(x=x.sd100))+
  geom_histogram(closed="left",boundary =0, binwidth = 50, color = "black",fill ="red")

#CHALLENGE. How does changing the number of random numbers generated affect the shape of the resulting distribution?
# The shape of the distribution gets tighter, and the count on the y axis is obviously larger when n increases


std_err <- function(x){
  sd(x)/sqrt(length(x))
}

std_err(1:10)
std_err(rnorm(100))


std_err(c(1:10, NA))

sd(c(1:10, NA), na.rm = TRUE)
sd(1:10)

length(1:1000)

std_err <- function(x){
  sd(x, na.rm = TRUE)/sqrt(length(x[!is.na(x)]))
}
# OR

std_err <- function(x){
  x.n <- x[!is.na(x)]
  sd(x.n)/sqrt(length(x.n))
}

std_err <- function(x, na.rm){
  sd(x, na.rm = na.rm)/sqrt(length(x[!is.na(x)]))
}

std_err(c(1:10, NA), na.rm = TRUE) # remove NAs

std_err(c(1:10, NA), na.rm = FALSE) # keep NAs
std_err(1:10)
std_err(c(1:10, NA)) # gives error

# Set default mode in the fuction 
std_err <- function(x, na.rm = FALSE){
  sd(x, na.rm = na.rm)/sqrt(length(x[!is.na(x)]))
}

std_err(c(1:10, NA))


std_err <- function(x, na.rm = FALSE){
  if (na.rm){
    x.n <- x[!is.na(x)] # if na.rm = TRUE, remove NAs from x and rename x.n
    sd(x.n)/sqrt(length(x.n)) # calculate se on x.n
  } else {
    sd(x)/sqrt(length(x)) # if na.rm = FALSE, calculate se on x
  }
}

mean50 <- function(x){
  NAx <- x[!is.na(x)]
  mean(NAx * 50)
}

vectorx <- c(1:10, NA)

mean50(vectorx)
mean(vectorx, na.rm = TRUE)
5.5 * 50





mean50 <- function(x, na.rm) {
  mean(x, na.rm = na.rm) * 50
}

mean50(vectorx, na.rm = FALSE)
mean50(vectorx, na.rm = TRUE)

#####################   IF/ELSE ##############
# if (condition) {(TRUE)} else{(FALSE)}
# IF else only works for true or false for na.rm

std_err <- function(x, na.rm = FALSE){
  if(na.rm){
    x.n <- x[!is.na(x)]
    sd(x.n)/sqrt(length(x.n))
  } else{
    sd(x)/sqrt(length(x))
  }
}


std_err(c(1:10, NA), na.rm = TRUE)





cor_sq <- function(vec1, vec2){
  if(length(vec1) != length(vec2)){
    stop("X and Y must be same length, hehe hi rachel")
  }else{
  cor(vec1, vec2)^2
  }
}


cor_sq(vec1=1:10, vec2 = 21:30)
cor_sq(vec1=1:10, vec2 = 20:30)


z.scores <- function(x, na.rm){
  (x-mean(x, na.rm = na.rm))/sd(x, na.rm = na.rm)
}

z.scores(c(1:10, NA), na.rm = TRUE)

############### ITeration ####################
dat <- data.frame(one = rnorm(50), two = rnorm(50), three = rnorm(50), four = rnorm(50))

output_sd <- vector(mode = "double")

for(i in seq_along(dat)){
  output_sd[i] <- sd(dat[,i])
}


apply(dat, 2, sd)


## packages
library(ggplot2)

## count number of orders in msleep
orders_n <- length(unique(msleep$order))

## define empty vector
output_sleep <- vector(mode = "double", length = orders_n)
names(output_sleep) <- unique(msleep$order) # name it by order

## run for loop
for(i in seq_len(orders_n)){   #alternatively (i in 1:orders_n)
  order.i <- unique(msleep$order)[[i]] # select each order
  msleep.n <- msleep[msleep$order %in% order.i, ] # subset msleep
  output_sleep[[i]] <- mean(msleep.n$sleep_total, na.rm = TRUE) # calculate mean and save to output
}

## check output
output_sleep
?seq_len

aggregate(sleep_total ~ order, msleep, mean)

## use tapply
tapply(msleep$sleep_total, msleep$order, mean)

#### Challenge

output_mean <- vector(mode = "numeric")

for(i in seq_len(nrow(dat))){
  output_mean[[i]] <- mean(unlist(dat[i,]))
}

output_mean

?unlist

apply(dat, 1, mean)

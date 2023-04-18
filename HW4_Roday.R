##
## Date Created: 2023-03-08
##
## Copyright (c) Rachel Roday, 2023
## Email: rroday@udel.edu
##
##---------------------------
##
##  Notes: ENWC 617 HW 4
##        This HW took about 40 minutes
##         Factors, functions, and for loops (FFF)
##        
##---------------------------

# Create vector for Q1-Q4
my_fac <- factor(c("a", "a", "a", "a", "q", "q", "c", "d", "b", "j", "L", "L", "L"))


# Q01
# These are the factor levels
levels(my_fac)

# Q02
# Default levels are arranged alphabetically

# Q03
df <- data.frame(my_fac)
# add libs
library(forcats)
#reorder
df2 <- data.frame(fct_infreq(my_fac))

# Q04
ggplot(df2, aes(my_fac))+
  geom_bar(fill="skyblue")+
  labs(y = "Number of Observations", x = "My Factor")+
  theme_bw()

# Q05
add_four<- function(x){
  x + 4
}
#Check work
add_four(1:10)
#Nice


# Q06
my_fun <- function(x){
  tot <- sum(x)
  x/tot
}
#check work
sum(1:10)
(1:10)/55
my_fun(1:10)
identical(my_fun(1:10),(1:10)/55)
# nice


# Q07
# t.test()

# Q08
# The default for var.equal is FALSE. When specified as TRUE, "the pooled variance is used
# to estimate the variance"

# Q09
mean(2, 3, 4, 5, 6)
# mean() takes the first argument and finds the average. The mean of 2 is 2. 
# To find the mean of these five numerics, the line should read "mean(c(2,3,4,5,6))"

# Q10
output <- vector()              # Define output vector - I called it output
for (i in seq_along(trees)){    #Write for loop
  output[i] <- mean(trees[,i])
}

output                          # View output vector
#check work tho
trees %>%
  summarise(
    avg1 = mean(trees$Girth),
    avg2 = mean(trees$Height),
    avg3 = mean(trees$Volume)
  )
# nice

# Q11
# I code all my HWs like this because theres nothing worse than going back to a 
# script a year later and having no f***ing clue what all of it means.
# In fact, I barely know what my code from a week ago means. 
# Also, yeah I dont have soft wrap on. I prefer it this way. 
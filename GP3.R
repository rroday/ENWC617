# GP3 
# ENWC 617
# Rachel Roday (c) 2023

c(1, 2, 3, 4, 5)
1:5
seq(from = 1, to = 5, by = 1)
my_vec <- c(1, 2, 3, 4, 5)
length(my_vec)
named_vec <- c(x = 2, y = 3, z = 4)

named_vec
attributes(named_vec)

typeof(5)
typeof(5L)

sqrt(2)^2
(sqrt(2)^2) - 2

c(1, 2, 3, 4) + 2
c(1, 2, 3, 4) / 2
c(1, 2, 3, 4) * c(0, 0, 1, 1)
c(-1, 0, 1, NA) / 0
log10(0)


x <- c("this is a very long string to show you that length of the string does not matter", "also", "it", "can", "be", "short!")
typeof(x)

c(TRUE, TRUE, FALSE, TRUE)
vec1 <- c(2, 3, 5, 3)

vec1 == 3
vec1>=2
vec2<-c(1:20)
vec2 %% 2  ## %% gives you x divided by y and the integer remainder 


x <- c(1, 32, 6, 4)

x[c(1,3)]
z <- c("oilbird", "pelican", "penguin", "auk", "egret", "heron", "woodpecker", "owl")
z[c(8, 6,3)]

x <- c(1, 32, 6, 4)

x[c(-1,-3)]
x[c(TRUE, TRUE, FALSE, TRUE)]


x[x>3]

x[x != 32]


b <- c(2, 1, 56, NA, -4, 12, NA)
b[!is.na(b)]


fruit <- c("apple", "pear", "kiwi", "plum")

grep("a", fruit)

fruit[grep("e", fruit)]




1:5 + 10

1:5 + c(10, 10, 10, 10, 10)

identical(1:5 + 10, 1:5 + c(10, 10, 10, 10, 10))

c(3, 2, 1, 1) + c(1, 0)
c(3, 2, 1, 1) + c(1, 0, 0)


## Changing Types
TRUE + FALSE + TRUE

a <- c(2, 9, 12, 23, 3, 98, 1)
sum(a>10)
a[a<10]

typeof(2)

typeof(as.integer(2))

typeof(as.logical(2))

typeof(as.character(2))

x <- c(1, 2, 3)

y <- c(-2, 12, 1, 5)

z <- c(x, y)

typeof(x)

typeof(y)

typeof(z)



x <- c(1, 2, 3)

y <- c("dog", "cat", "goldfish")

z <- c(x, y)

typeof(z)



x <- c(1, 2, 3)

y <- c("dog", "cat", "goldfish")

my_list <- list(x, y)
my_list

my_list[[2]][2]




x <- data.frame(var1 = 1:4, var2 = c("a", "b", "c", "d"))

x
attributes(x)
names(x)

rownames(x)
typeof(rownames(x))
x[3, ]
x[ , 2]
x[1:3, 2]
x$var1
x[x$var1 > 2, ]
x[x$var2 == "b", ]
x$var2 %in% c("b", "c")
x[x$var2 %in% c("b", "c"), ]
x[x$var2 == "b" | x$var2 == "c", ]


library(ggplot2)
data(msleep)
msleep[msleep$vore == "carni" & msleep$sleep_total > 10,]

length(unique(msleep$order))


df<- msleep[msleep$order %in% c("Carnivora","Rodentia","Cetacea") , c(1,3,4,6)]
msleep.n <- msleep[!msleep$order == "Cetacea",]
msleep[,-6]

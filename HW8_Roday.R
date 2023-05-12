##
## Date Created: 2023-05-02
##
## Copyright (c) Rachel Roday, 2023
## Email: rroday@udel.edu
##
##---------------------------
##
##  Notes: ENWC 617 HW 7
##        
##        
##        
##---------------------------


# Q01
## total number of alleles
num_total <- sum(1469, 138, 5) * 2
## number A alleles
num_A <- (1469 * 2) + 138
## number a alleles
num_a <- (5 * 2) + 138
## frequency of A alleles = p
freq_p <- num_A / num_total
## frequency of a alleles = q
freq_q <- num_a / num_total
#Next calculate the genotype frequencies expected under Hardy Weinberg Equilibrium.
## p^2
freq_p2 <- freq_p^2
## 2pq
freq_2pq <- freq_p * freq_q * 2
## q^2
freq_q2 <- freq_q^2

#--------#

indiviudals <- num_total/2

expAa<- indiviudals*freq_2pq
expAA <- indiviudals*freq_p2
expaa <- indiviudals*freq_q2

# Q02
# we can run a chi sq contingency test to compare frequencies against a null distribution frequency

# Q03
# Just like in the goodness-of-fit test, we assume that no frequencies are less than one and that no more than 20% of expected frequencies are less than five.
# The expected frequency of aa (recessive homogeneous alleles) is less than 5 (3.39), and the percentage of this cell is more than 20% of the observations (33.33%). Im just realizing I did the quiz wrong lol. 
# So we should do a fisher test instead of a chi sq

# Q04
HW8 <- matrix(c(1469, expAA, 138, expAa, 5, expaa), nrow = 2)
row.names(HW8) <- c("Actual", "Expected") # add rownames
colnames(HW8) <- c("AA", "Aa", "aa")

chisq.test(HW8)
fisher.test(HW8)
# P values are similar but fisher.test is more exact bc it doenst violate assumptions

# Q05
# from the textbook
# df = (r-1)(c-1)
# where r = freq p and freq q, and c = observed and expected
# df = (2-1)(2-1) = 1 degree of freedom

# Q06
?pchisq
pchisq(q = 0.34369, df = 1, lower.tail = FALSE)
# We  fail to reject the null hypothesis the the observed freuqency is different from the expected frequency (null distribution) (P=ns, X62 = 0.345, by distribution function of chi square). So yes, the observed frequencies ARE in hardy weinberg equilibrium


# Q07
# We should run a  chi sq test to test if frequencies ares the same between both locations. However, since we have more than 20% of the data that have freqencies less than 5 observations (75% (4,2,3)), we will fun a fishers exact test instead

Q07 <- matrix(c(4, 2, 3, 9), nrow = 2) # create matrix
row.names(Q07) <- c("infected", "uninfected") # add rownames
colnames(Q07) <- c("LocationA", "LocationB")


chisq.test(Q07)
fisher.test(Q07)

#There is no difference in infection prevalence between location a (n=6) and location b (n=12) (P=ns, Fisher's exact test for count data). Therefore, we fail to reject the null hypothesis that infection prevalence in mice is the same between locations. Therefore, infection of mine does not seem to depend on location.  
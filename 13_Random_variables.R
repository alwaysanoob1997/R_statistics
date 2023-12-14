# Random Variables and Distributions

# A random variable is one that can take any value in the distribution
# It is denoted by a capital letter, i.e. P(X = 2) etc

# We have already gone through sampling models in R. You can define the complete
# population and randomly sample or you can sample from possibilities according 
# to relative probability


# A binomial distribution is one that has only 2 outcomes, success or failure


# We have already seen the distribution of a given dataset. Probability 
# distribution differs from that in the fact that , it does not depend on a list
# of recorded data already, but instead on the possibilities and the probabilities 
# of each. The curve, known as the probability distribution function is derived 
# mathematically.
# The mean is known as the expected value and the standard deviation is known as 
# the standard error

# Expected value of a random variable
# a(p) + b(1-p) for a binomial distribution where a and b are outcomes and p is 
# the P(a) and 1-p is the P(b)

# Expected value of a sum of n draws ==
# n *(a(p) + b(1-p))

# Standard error for a binomial distribution
## |b - a| * sqrt(p* (1-p)) 

# And standard error for a sum of n draws ==
## sqrt(n) * |b - a| * sqrt(p* (1-p)) 



# Central limit theorem states that s that when the number of independent draws-
# also called sample size- is large, the probability distribution of the sum of 
# these draws is approximately normal.


# For example: let winning a roulette game give you $17 and losing cost $1
# but P(w) = 2/38 and P(nw) = 1 - P(w)

w <- 17
nw <- -1
pw <- 2/38
pnw <- 1 - pw

# Let there be a 1000 games
n <- 1000

# One example of a sum outcome of a 1000 games is 
sum(sample(c(w, nw), n, replace = TRUE, prob = c(pw, pnw)))

library(tidyverse)


# Now to plot the sum a 1000 times to get a distribution of the sums
x <- replicate(n, sum(sample(c(w, nw), n, replace = TRUE, prob = c(pw, pnw))))


data.frame(x) %>% 
  ggplot(aes(x = x)) +
  geom_histogram()

# QQ plot to check normality
# mean of the prob dist, i.e. expected value and standard error
E <- n * (w * pw + nw * pnw)
SE <- sqrt(n) * abs(nw - w) * sqrt(pw * pnw)
p <- seq(0.01, 1, 0.01)

theoretical <- pnorm(p, mean = E, sd = SE) # wrong, should've used qnorm
real <- quantile(x, p)
?pnorm

# It appears that it is actually quite normal
data.frame(real = (real - mean(real)) / sd(real)) %>%
  ggplot(aes(sample = real)) + 
  geom_qq() + 
  geom_abline(aes(intercept = 0, slope = 1))

# Hence CLT proved. HEHEHEHE




# Apparently 30 instances is enough in many cases to prove CLT. Hence we shall 
# try that
x <- replicate(30, sum(sample(c(w, nw), n, replace = TRUE, prob = c(pw, pnw))))


data.frame(x) %>% 
  ggplot(aes(x = x)) +
  geom_histogram()

# QQ plot to check normality
# mean of the prob dist, i.e. expected value and standard error
E <- n * (w * pw + nw * pnw)
SE <- sqrt(n) * abs(nw - w) * sqrt(pw * pnw)
p <- seq(0.01, 1, 0.01)

theoretical <- pnorm(p, mean = E, sd = SE) # wrong, should've used qnorm
real <- quantile(x, p)

# Dayummmm, it still appears that it is actually quite normal
data.frame(real = (real - mean(real)) / sd(real)) %>%
  ggplot(aes(sample = real)) + 
  geom_qq() + 
  geom_abline(aes(intercept = 0, slope = 1))







# By basic laws of probabilities, E(X1 + X2 + ... Xn) = E(X1) + E(X2) + ... E(Xn)
# And SD(X1 + X2 + ... Xn) = sqrt(SD(X1)^2 + SD(X2)^2 + ... SD(Xn)^2)

# E(n*X) = n* E(X)
# And SD(n * X) = n * SD(X)

# And combining the two:
# For averages..
# E((X1 + X2 + ... Xn) / n) = E(X1) + E(X2) + ... E(Xn) / n
# == E(X)
# SD(X1 + X2 + ... Xn / n) = sqrt(SD(X1)^2 + SD(X2)^2 + ... SD(Xn)^2 ) / n
# == SD(X) / sqrt(n)

# From this we see that as n increases, the SD falls closer to 0 and E(X) is close
# to the true mean. This is known as the Law of Large Numbers


# The sample size required for the Central Limit Theorem and Law of Large Numbers
# to apply differs based on the probability of success.
# If the probability of success is high, then relatively few observations are needed.
# As the probability of success decreases, more observations are needed.
# If the probability of success is extremely low, such as winning a lottery, then
# the Central Limit Theorem may not apply even with extremely large sample sizes.
# The normal distribution is not a good approximation in these cases, and other 
# distributions such as the Poisson distribution (not discussed in these courses)
# may be more appropriate.




# Lottery winner numbers 
lott <- 1/10^6

# If 10 million people play
n = 10 * 10^6

num_winners <- sum(sample(c(0, 1), n, replace = TRUE, prob = c(1-lott, lott)))

# Lets replicate this a 100 times, i.e. a 100 lottery games
games <- replicate(30, sum(sample(c(0, 1), n, replace = TRUE, prob = c(1-lott, lott))))

# lets plot it
data.frame(games) %>% 
  ggplot(aes(x = games)) +
  geom_histogram()

# Lets see if its normal?
data.frame(real = (games - mean(games)) / sd(games)) %>%
  ggplot(aes(sample = real)) + 
  geom_qq() + 
  geom_abline(aes(intercept = 0, slope = 1))







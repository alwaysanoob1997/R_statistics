# Assume an urn of blue and red balls of which 25 balls are sampled of which 
# 13 are blue and 12 are red. In this sample, P(red) is 0.48
# Assume on multiple samplings, we take a mean each time. By CLT we can assume that 
# the means of all the samples follow a normal distribution. 
# Using this, we can try to calculate how close our initial sample is to the 
# expected value

obs_mean = 12/25 # Of red

# Now to calculate how close this value is to the expected mean, lets say within 1%
# this means, we want to see, P(|obs_mean - expected_mean| <= 0.01)
# Or P(obs_mean <= expected_mean + 0.01) - P(obs_mean <= expected_mean - 0.01) 

# Since we dont know the expected mean (which is the value we want to get close to),
# we can do the same trick as previously, i.e. convert to z scores
# That means, this becomes

# P(Z <= 0.01/SE(RV_mean)) - P(Z <= -0.01/SE(RV_mean)) # RV means random variable of mean

# But again, we dont really know the SE as well. But we can try to calculate the SE 
# using the obs_mean instead of the expected mean (or p). This gives an estimated SE,
# that is close enough to apply CLT

estimated_se <- sqrt(obs_mean * (1-obs_mean) / 25) # close to 0.1

# from this we can calculate the probability of the initial obs_mean being within
# 1% of the expected_mean, using pnorm and the estimated_se

example <- pnorm(0.01/estimated_se) - pnorm(-0.01/estimated_se) # Which is close to 8%
example


# Margin of Error: It is 2 * SD(RV_mean). It means that the obs_mean will be within
# 2 SD of the expected_mean 95% of the time
# Why 95? It's arbitrary
# Here, we can calculate the margin of error as 2 * estimated_se of the example 
# given 
moe <- 2 * estimated_se # Approx 0.2

# This MoE is quite large and hence we can say that the sample size of 25 is not 
# enough



#### ---------------------------------------------------------------------- ####
                              # Aside

# As a side, let's try to calculate the obs mean for which it might be the closest
# to the expected mean 
p_range <- seq(0, 1, 0.01)

throwaway <- function(x, total = 25){
  estimated_se <- sqrt(x * (1-x) / total)
  pnorm(0.01/estimated_se) - pnorm(-0.01/estimated_se)
}

answers <- sapply(p_range, throwaway)


p_range[answers == max(answers)]
p_range[answers == example]
plot(x = p_range, y = answers)

# This probably means that if the sample gives all blue or all red, it is very 
# likely that the urn contains all of  those colors only. 
# Interesting


# How about we increase the sample size? From 25 to 100
total_range <- seq(25, 100)
answer_total <- sapply(total_range, function(x, p = 0.48){
  estimated_se <- sqrt(p * (1-p) / x)
  pnorm(0.01/estimated_se) - pnorm(-0.01/estimated_se)
})

plot(x = total_range, y = answer_total) # We can obviously see that the graph 
# goes up as the sample size increases

#### ---------------------------------------------------------------------- ####




# Example 2


p <- 0.45    # unknown p to estimate. Lets do a test for 0.45
N <- 1000

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})


# Lets plot the x_hat

library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")

p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")

# I added the p3 to show that the abline does not work the same here, nor does the x axis scale
p3 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  # stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_qq() +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, p3, nrow=1)



# We see that if we increase the sample size, we can reduce the margin of error 
# Here, we have set the sample size to be 100000, and are calculating the MoE for
# all values of p (which are the realistic probabilities in an actual poll) and 
# we can graph the MoE
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line() # The max is 0.3% Which means very accurate

# But this large a sample is not taken because:
# 1. Expensive
# 2. Potential for bias
# 3. People may not be truthful




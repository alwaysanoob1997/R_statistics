library(dslabs)
data("nhtemp")

# nhtemp is a time series data set. The geom_smooth function uses confidence 
# intervals to plot the grey area near the line
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")


# Confidence interval is the interval of values where we can be X% sure that the 
# value of interest lies within the interval
# Usually, we choose 95% confidence intervals
# We can get this by adding the 2 * standard error (margin of error) of the
# estimated value of interest to the estimated value on both sides such that we 
# get the interval X_hat - 2 * SE to X_hat + 2 * SE. 

# Example with a monte carlo simulation
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean


# We can denote this as trying to find the probability of the value of interest, p
# being in between 2*SE on either side of the X_hat we just found.
# i.e. P(X_hat - 2*SE_hat <= p <= X_hat + 2*SE_hat). Subtracting X_hat and dividing 
# by SE_hat on all sides, we get...
# == P(-2 <= (p - X_hat) / SE_hat <= 2), where the middle expression appears to be
# an approximately normal distribution converted to a z distribution. We also know 
# that is probability we are trying to find in terms of Z distribution is 95%



# Suppose we wanted a 99% confidence interval, we can chose the z scores required like this
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval

# Meaning, instead of 2 * SD, we have to use z * SD to get the 99% confidence interval
pnorm(qnorm(1-0.995))


# Also suppose we wanted exactly 95% confidence interval, we use the formula:
# 1- (1-q)/2 = z (the z score required)
z <- qnorm(1 - (1-0.95)/2) # which is 97.5%, and the qnorm gives the z score of 
                           # about 1.96. (qnorm gives the value at the nth quatile we supply)
pnorm(z) - pnorm(-z) # this gives exactly 95% interval




# Let's run a Monte Carlo simulation to confirm that the confidence interval works
p <- 0.45
N <- 10
inside <- replicate(10000, {
  X_hat <- mean(sample(c(0, 1), replace = TRUE, size = N, prob = c((1-p), p)))
  SE_hat <- sqrt(X_hat * (1-X_hat) / N)
  SE_real <- sqrt(p * (1-p)/N)
  between(p, X_hat - qnorm(0.975) * SE_hat, X_hat + qnorm(0.975) * SE_hat)
})
mean(inside)
# In these tests, we are creating a random variable X_hat and an associated 
# random interval and are checking to see if p lies in this random interval. 
# Technically, p is not a random variable, but the interval is random

# 95% Confidence interval of the spread of an election with the sample size of 25
N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-1.96, 1.96) * 2 * sqrt(X_hat*(1-X_hat)/N) 
# Estimated spread = (2*X_hat - 1)
# Spread standard error = 2 * SE(X_hat)

# Here we see that the CI of the spread contains 0. Meaning the result could go
# both ways - from one side winning to a tie to the other side winning
# This is not helpful.
# This does not necessarily mean that the election will be close, just that the 
# sample size is too small.
# Power is the probability of detecting an effect when there is a true effect to 
# find. Power increases as sample size increases, because larger sample size means 
# smaller standard error.




### ---------------------------------------------------------------- ###

# P Values

# Assume, that we have a sample of 100 beads from an urn whose proportion come up 
# to 0.52 for the number of blue beads in it. To know whether this is a significant 
# value,, we can take the confidence intervals of the mean. Another option is to 
# assume that the proportion is lesser, and try to prove that wrong (null hypothesis)
# Let the null hypothesis be that the proportion is 0.5
# Let the alternate be that the proportion is > 0.5 (in this case 0.52)

N <- 100
X_hat <- 0.52
p <- 0.5


# If we assume that the null is true, what is the probability of the X_hat being true?
# This prob is known as the p-value
# What we have to find == P(X_hat - p >= 0.02)
# 
# Now if we divide by the standard error of the distribution of p for the sample 
# size of N, to convert both sides to a z score, we get...
#
# P(X_hat - p / SE(p) >= 0.02/SE(p)) which is...
# P((X_hat - 0.5)/sqrt(0.5 * (1-0.5) / N) >= 0.02 / sqrt(0.5 * (1-0.5) / N)) ==
# P(sqrt(N) * (X_hat - 0.5) / 0.5 >= sqrt(N) * 0.02 / 0.5).
# Basically we find the z score of the value X_hat, when the sample size is N
# of a distribution centered around p

z <- sqrt(N) * (X_hat - p)/ sqrt(p * (1-p)) # // sqrt(N) * 0.02 / 0.5

# Then we find the probability of finding values that are > than the z score of 
# X that we calculated. i.e. X > 0.52, when the expected mean is p (i.e. p value)
1 - (pnorm(z) - pnorm(-z)) # Which is 0.68



# Alternatively, lets look at how the p value changes with sample size
N <- 1000
X_hat <- 0.52
p <- 0.5
z <- sqrt(N) * (X_hat - p)/ sqrt(p * (1-p)) 
1 - (pnorm(z) - pnorm(-z)) # Reduced to 0.2

# Trial 3
N <- 100
X_hat <- 0.6 # Made the proportion a little more further away
p <- 0.5
z <- sqrt(N) * (X_hat - p)/ sqrt(p * (1-p)) 
1 - (pnorm(z) - pnorm(-z)) # Reduced to 0.04 (i.e. significant)


# If a 95% confidence interval does not include our observed value, then the 
# p-value must be smaller than 0.05.
# It is preferable to report confidence intervals instead of p-values, as 
# confidence intervals give information about the size of the estimate and p-values do not.


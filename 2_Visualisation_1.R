library(dslabs)
library(tidyverse)


data("heights")


heights %>% 
  filter(sex == "Male") %>% 
  select(height) %>%
  .$height %>%
  hist()



heights %>% 
  .$sex %>%
  plot() %>%
  pie()


# Find proportion of data using prop.table and table functions
prop.table(table(heights$sex))
?prop.table()




# CDF 
# define range of values spanning the dataset
a <- seq(min(heights$height), max(heights$height), length = 40)    

# computes prob. for a single value
cdf_function <- function(x) { 
  return(mean(heights$height <= x))
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)



# Normal Distribution is defined by a function that requires only the mean
# and SD. Hence why people love to equate any distribution to the normal

# Z scores are scores that have been normalised according to the mean and SD
# of a normal distribution, calculated by:
scale(a)
# Any Z score > 3 or < -3 is considered an anomaly


# Calculating the proportion of values whose z scores are less than 2, i.e. 
# within 2 SD of the mean. Scale(a) converts to z scores and abs removes the  
# negatives and < 2 converts to logical, and the mean at the end adds the ones 
# that are  true i.e. numeric value of 1.
### Smart code
mean(abs(scale(a)) < 2)


# 68, 95, 99.7 rule for 1, 2, 3 SD respectively


x <- heights %>%
  filter(sex == "Male") %>%
  pull(height)

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
# due to discretization
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


# How to check quantiles
w <- seq(0.01, 0.99, 0.01)
quantile(w, 0.25)

# Summary function
summary(heights)


# To get all percentiles
quantile(heights$height, w)
percentiles <- quantile(heights$height, w) # It returns a names vector. 
                                           # Run the prev line to see
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]




# To get CDF, we use pnorm which requires the check value, mean and SD. By default, mean = 0, and SD = 1
# qnorm can be used to get the inverse

pnorm(25)
pnorm(24)

pnorm(1) 
pnorm(2) 
pnorm(-1)

pnorm(-1.96) # Inverse with qnorm given below

# Given a value of CDF, we can calculate the theoretical "check" value using qnorm
qnorm(0.025) # default mean and SD is 0 and 1 respectively

pnorm(qnorm(0.025))



# you can also give a vector to qnorm which is a collection of percentiles that
# can divide the dataset into theoretical quantiles based on the vector, mean and SD

p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)
theoretical_quantiles


## QUANTILE - QUANTILE PLOTS
avg <- mean(x)
stdev <- sd(x)
mean(x <= 69)
qnorm(0.5, avg, stdev)
median(x)


p <- seq(0.05, 0.95, 0.05)
obs_q <- quantile(x, p)
th_q <- qnorm(p, avg, stdev)
plot(th_q, obs_q)
abline(0,1)


# You can also make it with scaled values i.e.
z <- scale(x)
scale_obs_q <- quantile(z, p)
scale_obs_q

scale_th_q <- qnorm(p)

plot(scale_th_q, scale_obs_q)
abline(0,1)

# Slightly lesser code, in the sense that you dont have to get the avg and sd

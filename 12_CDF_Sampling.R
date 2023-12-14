library(tidyverse)
library(dslabs)


data("heights")

x <- heights %>%
  filter(sex == "Male") %>%
  pull(height)


# Empirical CDF function for this data ---

ecdf <- function(a, data){
  mean(data <= a)
}

ecdf(60, x)


# If we have to calculate the proportion of students > 70 inches in ht
1 - ecdf(70, x)

# pnorm gives the theoretical cdf for the data of mean(x) and sd(x) values 
pnorm(69, mean = mean(x), sd = sd(x))
?pnorm

plot(table(x))



# Why -4, and 4 are used to create a sequence is because, without supplying avg 
# and sd to dnorm, we basically use z scores to calculate the density function
# and z scores are from 3 to -3. Changing to to a higher number makes the plot weird
y <- seq(-4, 4, length.out = 100)
sum(y)

data.frame(y, f = dnorm(y)) %>%
  ggplot(aes(y, f)) +
  geom_line()


# Creating a random data set that conforms to the given mean and sd using rnorm function
# Creating a new data set of the heights data
th_ht <- rnorm(n = length(x), mean(x), sd(x))

data.frame(simulated_heights = th_ht) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(fill="blue", binwidth = 2, alpha = 0.5) +
  geom_histogram(aes(x = x), fill = "red", binwidth = 2, alpha = 0.4) +
  xlab("Simulated Heights (blue) vs Actual Heights (red)")





# Using rnorm to find the probability of a 7 footer person
B <- 10000
# Running the simulation B times
tallest <- replicate(B, {
  # Using rnorm to create a 800 people dataset with mean and sd of the heights vector
  simulated_data <- rnorm(800, mean(x), sd(x))    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)


# The answer comes to about 1.6 percent



# Asssessment Test
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, mean = 20.9, sd = 5.7)

act_scores

mean(act_scores)
sd(act_scores)
max(act_scores)

hist(act_scores)
sum(act_scores >= 36)


mean(act_scores > 30)

mean(act_scores <= 10)


x <- seq(1, 36, 1)
x


plot(x = x, y = dnorm(x, mean = mean(act_scores), sd = sd(act_scores))) 


z_act <- (act_scores - mean(act_scores)) / sd(act_scores)
mean(z_act > 2)


2 * sd(act_scores) + mean(act_scores)


qnorm(0.975, mean(act_scores), sd(act_scores))


ceiling(qnorm(0.95, mean(act_scores), sd(act_scores)))


qnorm(0.95, 20.9, 5.7)


p <- seq(0.01, 0.99, 0.01)


sample_quantiles <- quantile(act_scores, probs = p)

sample_quantiles[sample_quantiles > 25 & sample_quantiles < 27]

# Better way
names(sample_quantiles[max(which(sample_quantiles < 26))])


theoretical_q <- qnorm(p, mean = 20.9, sd = 5.7)

qqplot(x = sample_quantiles, y = theoretical_q)



# Sampling Models

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S



# Monte Carlo simulations for the second sampling method
n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  return(sum(X))    # determine total profit
})

mean(S < 0)    # probability of the casino losing money









s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S

normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")


?dnorm






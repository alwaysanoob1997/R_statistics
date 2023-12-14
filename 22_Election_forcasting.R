# Lets forecast an election using the Bayesian approach
# Lets assume only 2 parties in the main competition
# Let us assume that historically, we cannot say who will win, i.e. the average 
# of the spread, mu is 0. Also 
# incidentally, we also notice that when victorious, the spread is generally
# about 3.5 %. Which means that the tau (standard error) of the mean is 0.035

mu <- 0
tau <- 0.035

# Lets now use the us polls dataset of the dslabs package. Here we will filter
# only the whole country polls and that were done towards the end and only contains
# one poll per pollster


library(tidyverse)
library(dslabs)

polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% 
  group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
# The results df gives us the mean observed spread, se of the mean and the ci
# of the mean


# lets define the variables x_bar and sigma
x_bar <- results$avg
sigma <- results$se

# Using the continuous version off Bayes
B <- sigma^2 / (sigma ^2 + tau ^2)
post_SE <- sqrt(1/(1/sigma^2 + 1/tau^2))
posterior_mean <- B * mu + (1-B) * x_bar

# The credible interval, assuming the posterior mean is distributed normally
post_ci <- c(posterior_mean + qnorm(0.025) * post_SE, 
             posterior_mean + qnorm(0.975) * post_SE)

# Probability of Clinton winning --
1-pnorm(0, mean = posterior_mean, sd = post_SE)
# == 0.9999975
# However, this seems way to confident that clinton will win, which was not the 
# same as predicted by the pollsters at the time
# This is because of a general bias that occurs with all pollsters in all polls
# but we cannot really explain it
# It is however noted that the general bias averages to about 2-3% each year. 
# We cannot see this bias with just a single year. 


# Mathematical model of the election forecasting
# Assuming there are i pollsters who take j polls each. Assume that each pollster
# has a bias of h_i, which varies from pollster to pollster, and is also assumed 
# to be normal with an expected value of 0, and an SE of 2.5%, (assumed value)
# which is what we have seen till now in our lessons
# Since we are looking back, we know the actual spread for the 2016 election - 0.021

# Lets take the actual poll values that is there in the data set for reference
polls %>%
  group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(x=pollster, y=spread)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


d <- 0.021
p <- (d + 1)/2
N <- 2000 # (Number of people in each poll)
SE_d <- 2 * sqrt(p * (1-p)/ N)
j <- 6 # number of polls by a pollster

# Expected spreads from just one pollster in their polls
X <- d + rnorm(j, 0, SE_d)


# Expected spreads from 5 different pollsters
i <- 5
X <- sapply(1:i, function(pollster){
  d + rnorm(j, 0, SE_d)
  })
boxplot(X)

# Expected values when adjusting for pollster(house) bias that varies from pollster
# to pollster that is assumed to be normally distributed with expected value of 
# 0 and a Sd of 0.025
h <- rnorm(i, 0, 0.025)
X <- sapply(1:i, function(pollster){
  d + h[pollster] + rnorm(j, 0, SE_d)
})
boxplot(X)
# This more closely resembles the actual data in the dataset

# Finally let's add the general bias that occurs year to year. Let's assume it
# to be 0.025
gen_bias <- 0.025

X <- sapply(1:i, function(pollster){
  d + gen_bias + h[pollster] + rnorm(j, 0, SE_d)
})

boxplot(X)
# This might be a little better than the last plot, realistically

# Now to get the probaility of Clinton getting the popular vote in 2016 with Bayesian
# stats and general bias

mu <- 0
tau <- 0.035

X_bar <- results$avg
sigma <- sqrt(results$se^2 + gen_bias^2)

new_B <- sigma^2/(sigma^2 + tau^2)
new_p_mean <- B * mu + (1-B) * X_bar
new_p_se <- sqrt(1/(1/sigma^2 + 1/tau^2))

# Prob of the new spread > 0, i.e. Clinton will win
1 - pnorm(0, mean = new_p_mean, sd = new_p_se)

# This is about 81%, much closer to FiveThirtyEight's prediction of about 
# 80 something % and much lesser than our super likely scenario in the last model
# which predicted about 99.99% which is better. 



# Electoral college votes
# In the US, winning an election is not by popular vote, but rather the majority of 
# the electoral college votes. 
# Each state has a number of electoral votes, which varies from state to state. 
# Another quirk is that, if you win even a minor majority of electoral votes in
# a state, you win all the elctoral votes of the state - all or nothing
# This measure was done to prevent a largely populous states of US controlling the 
# election

results_us_election_2016 %>%
  arrange(desc(electoral_votes)) %>%
  head()

results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", state) & # Pattern matching function to remove entries with CD in it
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state)) # This, I'm not sure yet why it was done

# The top 10 closest state, also known as the battleground states
results %>%
  slice_min(n=10, order_by = abs(avg))

# If you also look, you see that some states were not even polled by the pollsters
results_us_election_2016 %>%
  filter(!state %in% results$state)
# This is because, in these states, the votes were already known
# That is 7 electoral votes for Clinton


# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# assigns sd to states with just one poll as median of other sd values 
# (as sd cannot be calculated with just 1 value)
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))


# Bayesian method to get chance of Clinton winning using electoral votes
# This model also assumes nothing about general victories
mu <- 0
tau <- 0.02 # Lets assume per state, the SD is lesser than for the country
updated <- results %>% 
  mutate(sigma = sd/sqrt(n),
         B = sigma^2/ (sigma^2 + tau^2),
         posterior_mean = B*mu + (1-B)*avg,
         posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean)) %>%
  select(state, avg, sd, n, electoral_votes, sigma, B, posterior_mean, posterior_se)

# We see that those that had multiple polls, retained the posterior mean similar to 
# the original value. 
updated %>%
  ggplot(aes(y = posterior_mean/avg, x = n, group = n)) +
  geom_boxplot()


# Combining that with a Monte Carlo simulation to find out the chance of Clinton
# winning

clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})

mean(clinton_EV > 269)    # over 269 votes wins election
# This gives a 99.9% chance of success
# This is because we have not accounted for general bias in this model

# histogram of outcomes
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)


# Accounting for a general bias of 3% within states (higher in the states apparently)
# this has to be added to the individual state SDs.
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election
# This is now a bit lower ~ 88%, a bit more realistic

data.frame(clinton_EV_2) %>%
  ggplot(aes(clinton_EV_2)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

# FiveThirtyEight had a prediction of about 71% as they used a lot more features
# that is lengthy and complicated for this edx course





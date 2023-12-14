# Obama poll election prediction
# Actual spread
d <- 0.039
# Sample sizes of actual 12 polls taken
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
# Getting a value of p for the given actual spread d
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  # Creating a random sample for the poll of size N in Ns
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  # Proportion of Obama voters
  X_hat <- mean(X)
  # SE of the Proportion
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  
  # This part, is getting the actual spread and then also the spread at the 
  # lowest and highest "likely" values of X_hat (i.e. at 95% confidence intervals)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# The confidence_interval object is a matrix array object. 
# t() transposes it


# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), 
                    sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
# This gives the poll spread estimates and the 95% confidence intervals for the same
polls

library(tidyverse)
polls %>%
  ggplot(aes(x = poll, y = estimate, group = poll)) + 
  geom_boxplot(aes(lower = estimate, 
                   upper = estimate, 
                   middle = estimate, 
                   ymin = low, 
                   ymax = high),
               stat = "identity") +
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = 0.039), linetype = "dotted") +
  scale_x_continuous(breaks = c(1:12), name = "Poll Num") +
  coord_flip()

  
# We see that all the polls 

# Get the average spread over the entire sample size of all polls together
# with the estimate weighted according to the sample size
d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

# Generating a new p_hat from the d_hat
p_hat <- (1+d_hat)/2

# Calculating and rounding the margin of error for the spread as well as the spread itself
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)



library(dslabs)
data("polls_us_election_2016")

?polls_us_election_2016
# Actual dataset of 2016 us poll elections. 
# Creating a spread column for clinton vs trump. With p being ClintonFiltering 
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & 
           enddate >= "2016-10-31" & 
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = (rawpoll_clinton - rawpoll_trump) / 100)

# Based on the urn model of earlier, we assume that the spread obtained from
# various polls follow a normal distribution

# Aggregate spread from all the polls --
d_hat <- sum(polls$spread * polls$samplesize) / sum(polls$samplesize)


x
# Assuming the earlier urn models we have used, we see that --
# Aggregate p from d_hat 
p_hat <- (d_hat + 1) /2

# Margin of error = 1.96 * SD of spread (assuming normal distribution)
moe <- 1.96 * (2 * sqrt(p_hat * (1-p_hat) / sum(polls$samplesize)))

# Here we see that the spread is 1.43% with MOE of 0.66%, i.e. it is very good

# But the actual result was that the spread was 2.1, which is just outside of
# the 95% CI

# If we plot the spread of the dataset, we see...
polls %>% 
  ggplot(aes(x = spread)) +
  geom_histogram(color = "black", binwidth = 0.01) # - which does not appear to 
# be normal

# Apparently this is because of pollster bias - various pollsters have their own
# bias that is repeated each time they poll. And pollsters poll a different number 
# of times in the last week we are filtering
polls %>% 
  group_by(pollster) %>%
  summarise(num_polls = n())

# Suppose we filter only those that poll >= 6 times
polls %>% 
  group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(spread)) + 
  geom_histogram(color = "black", binwidth = 0.01) # doesn't really help

# Looking at the pollster vs spread graph, we see that even when they repeat the 
# poll multiple times, they tend to have a bias to over spread or under spread
polls %>% 
  group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() + 
  theme(axis.text.x  = element_text(angle = 90, hjust = 1))


# Looking at the standard error for each pollster
polls %>% 
  group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarise(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))
# Assuming that p_hat is the true p and trying to calculate the average spread 
# for a poll done by a pollster, based on their sample size we get the above. 
# (Not a aggregate of all the polls, hence we do not do sum(samplesize))
# Not very sure why this was required. I would assume that this only checks to see
# whether the sample sizes were adequate enough to get a moe that can include the 
# actual result)

polls %>% 
  group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarise(D_ps = median(spread))
# However we do see that the expected result of these polls vary, where some 
# predict that clinton will win, vs some other predicting that trump will win
# This is what the course calls pollster bias, and the data collector called
# house effect




# Now to develp a data driven model for the same:
# Get the last poll for each pollster
one_poll_per_pollster <- polls %>%
  group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()


# Graphing this data
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)


# We are using a new model to think about this data now. Rather than think of the 
# population as 0s and 1s that are not voting for and voting for clinton respectively
# with the pollsters taking a random sample from this population and trying to 
# find the actual poll prediction from that, we can think of the population as being 
# the polls of the pollsters with the spread ranging from -1 to 1 with an unknown 
# standard error and an unknown expected value i.e. the expected spread
# 
# In short, we are moving away from the binomial model to a continuous variable model
# of the spread, where we want to know the expected value of the spread as well as 
# the standard deviation of the spread

# CLT still works for the sample averages of this population, as they can be considered
# random variables. For a large enough sample size N, we can consider the distribution 
# of the sample average X_bar as being approximately normal with a SD which is 
# sigma / sqrt(N)


# Using this model, we can try to find the expected spread and the se of the spread 
#  construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)
# This includes the actual poll spread of 2.1% within the 95% confidence interval




data(heights)


x <- heights %>%
  filter(sex == "Male") %>%
  .$height


# Define `mu` as the population average
mu <- mean(x)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000



# Define an object `res` that contains a logical vector for simulated intervals that contain mu
res <- replicate(B, (){
  y <- sample(x, N, replace = TRUE)
  se <- sd(y)/sqrt(N)
  ci <- c(mean(y) - qnorm(0.975) * se, mean(y) + qnorm(0.975) * se)
  return(between(mu, ci[1], ci[2]))
})

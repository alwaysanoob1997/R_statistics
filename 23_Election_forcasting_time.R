library(dslabs)
library(tidyverse)

data("polls_us_election_2016")

# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts even with the min samplesize
se <- one_pollster %>%
  summarize(observed = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")


# Plotting the spreads of all the pollsters variation based on time. 
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)





# Plotting the raw percentage of trump and clinton over time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))




# T distributions ...
# When you have sample sizes that are small, like with the following case, CLT
# fails, as it does not follow a normal distribution. A more appropriate distribution
# is the t-distribution (if we are sure the population is distributed normally)
# In t - distributions, the SE is calculated as SD/sqrt(N-1) where N-1 is the 
# degrees of freedom. Meaning here, the SE is larger than the CLT theory. 
# As a result, the graph has a thicker tail at both ends, i.e. extreme values 
# are more common with distributions with lesser degrees of freedom

z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
# nrow being the total DoF, and nrow -1 is what we divide with

# Calculating moe with t-dist ka z instead, that gives us a wider CI than the 
# normal dist calculations
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)



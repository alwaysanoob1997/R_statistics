# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)


# a histogram of murder rates
murders <- mutate(murders, rate = total / population * 100000)
hist(murders$rate)

# boxplots of murder rates by region
boxplot(rate~region, data = murders)



# Summarize function to get a single row output from rows of data
# Can be used to use a function to summarize the data - a custom function also 
# works
# minimum, median, and maximum murder rate for the states in the West region
murders %>% 
  filter(region == "West") %>%
  summarize(minimum = min(rate), 
            median = median(rate), 
            maximum = max(rate))


# average rate unadjusted by population size (Wrong)
mean(murders$rate)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate


# Since the above returns a dataframe, if you want a single value, you can use
# "$" or the "pull" function
# using pull to save the number directly
murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  pull(rate)

# using the dot to access the rate using "$"
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  .$rate
us_murder_rate


# Groupby
murders %>% 
  group_by(region) %>%
  summarise(median = median(rate))


# Sorting tables
murders %>% 
  arrange(region, desc(rate)) 


# Top n rows
murders %>%
  group_by(region) %>%
  arrange(region, rate) %>%
  top_n(1, rate)




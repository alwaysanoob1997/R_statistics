library(tidyverse)
library(dslabs)
data(murders)

murders %>% 
  ggplot(aes(population, total, label = abb, color=region)) + geom_label()



str(murders)

head(murders)
murders$population
names(murders)

states <- murders$state

class(states)
length(states)

levels(murders$region)
help(class(murders$region))

class(20L)



a <- 2
b <- -1
c <- -4

x = ((-1)*b)/(2*a) - sqrt(b^2 - 4*a*c)/(2*a)
x


log(1024, 4)

# Structure and factors
data("movielens")
str(movielens)
nlevels(movielens$genres)

# Vectors
numbers = c(1, 4, 7, 9, 21, 17, 18, 19, 20, 15, 25, 28, 30, "abacus=4", 40, 50, 234, 245345, 134234, 1234234, 356456)
numbers
as.numeric(numbers)


# Soort and Orders

sort(murders$total)

ordered <- order(murders$total)
ordered
murders$total
murders$total[ordered]
class(ordered)


murders$state[ordered]


# Max function
max(murders$total)
which.max(murders$total)
i_max <- which.max(murders$total)
murders$state[i_max]


# Min has the similar functions

# Rank
rank(murders$total)
class(rank(murders$total))


# Vector Arithmetic
murder_rate <- (murders$total / murders$population) * 100000
ind <- order(murder_rate, decreasing = TRUE)
states <- murders$state

new_df <- data.frame("State"=states[ind], "Murder Rate"=murder_rate[ind])
new_df


# creating a logical vector that specifies if the murder rate in that state is less than or equal to 0.71
index <- murder_rate <= 0.71
# determining which states have murder rates less than or equal to 0.71
murders$state[index]
# calculating how many states have a murder rate less than or equal to 0.71
sum(index)

# creating the two logical vectors representing our conditions
west <- murders$region == "West"
safe <- murder_rate <= 1
# defining an index and identifying states with both conditions true
index <- safe & west
murders$state[index]


# NAs in vectors
data("na_example")
na_example

mean(na_example[!is.na(na_example)])

mean(na_example, na.rm = TRUE)

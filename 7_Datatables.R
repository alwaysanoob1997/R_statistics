# Data.tables that is apparently better than tidyverse for larger datasets
library(tidyverse)
library(data.table)
library(dslabs)

# Convert DF to a data.table object
murders <- setDT(murders)

# selecting in dplyr
select(murders, state, region)


# selecting in data.table - 2 methods
murders[, c("state", "region")] |> head()
murders[, .(state, region)] |> head()


# adding or changing a column in dplyr
murders <- mutate(murders, rate = total / population * 10^5)

# adding or changing a column in data.table
murders[, rate := total / population * 100000]
head(murders)
murders[, ":="(rate = total / population * 100000, rank = rank(population))]


# Pointers
# TODO - to understand how the := operator works
# y is referring to x and := changes by reference
x <- data.table(a = 1)
y <- x

x[,a := 2]
y

y[,a := 1]
x

# use copy to make an actual copy
x <- data.table(a = 1)
y <- copy(x)
x[,a := 2]
y
x



# Subsetting -- a lot simpler
murders[rate < 0.7]


# Combining functions together
murders[rate < 0.7, .(state, region)]



# Summarising
# summarizing in data.table
murders[, .(average = mean(total), standard_deviation = sd(total))]


# subsetting and then summarizing in data.table
murders[region == "Northeast", .(average = mean(total), standard_deviation = sd(total))]


# grouping and then summarizing in data.table
murders[, .(average = mean(total), standard_deviation = sd(total)), by = region]


# Ordering
# order by population
murders[order(population)] |> head()

# order by population in descending order
murders[order(population, decreasing = TRUE)] 

# order by region and then murder rate
murders[order(region, desc(rate))]



# Tibbles to be learnt
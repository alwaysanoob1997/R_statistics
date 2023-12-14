# dplyr package tutorial
# mutate, filter and select functions with the %>% operator

library(tidyverse)
library(dslabs)

data(murders)

# Mutate
murders <- mutate(murders, rate=total/population * 100000)

head(murders)


# Filter
filter(murders, rate>3)

# select
select(murders, state, region, rate) 
  

# Combining it all together
# %>% operator
murders %>% 
  mutate(rate = total/population *100000) %>%
  select(state, region, rate) %>%
  filter(rate <= 0.71)


# creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)

grades
class(grades$names)


# the last argument is no longer required in the modern R
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90))

grades
class(grades$names)





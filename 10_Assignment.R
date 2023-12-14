library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

library(tidyverse)

head(heights)


heights %>% summarise(average = mean(height))


heights %>% summarise(above_average = sum(height > mean(height)))

heights %>%
  mutate(avg = mean(height)) %>% 
  group_by(sex) %>% 
  summarise(above_average = sum(height > avg))


# Proportion of gender
heights %>% 
  group_by(sex) %>% 
  summarise(total = n()) %>% 
  mutate(proportion = total/sum(total))

# Min height in the table
min(heights$height)


# Match with the one with min height
match(min(heights$height), heights$height)


# Subset by index
heights$sex[match(min(heights$height), heights$height)]


# Max height
max(heights$height)


x <- min(heights$height):max(heights$height)

sum(!x %in% heights$height)


# Heights in cm
heights2 <- heights %>% mutate(height_cm = height *2.54)

heights2$height_cm[18]

mean(heights2$height_cm)


heights2 %>% filter(sex == "Female") %>% count()


heights2 %>% 
  filter(sex == "Female") %>% 
  select(height_cm) %>% 
  summarise(average = mean(height_cm))


data(olive)
head(olive)


plot(olive$palmitic, olive$palmitoleic)


hist(olive$eicosenoic)


boxplot(palmitic~region, data=olive)



x <- 25
s <- 5

test <- function(x){
  1/x
}


test(x)

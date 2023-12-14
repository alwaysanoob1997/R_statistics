# Titanic Assessment
library(tidyverse)
library(titanic)
data("titanic_train")

str(titanic_train)
options(digits = 3)


# Creating the DS to work with 
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic
str(titanic)
?titanic_train


# Density plot without proportionate numbers for sexes
sep <- titanic %>%
  ggplot(aes(x = Age, color = Sex, fill = Sex)) +
  geom_density(alpha = 0.2)

# Density plot with count
com <- titanic %>%
  ggplot(aes(x = Age, y = ..count.., color = Sex, fill = Sex)) +
  geom_density(alpha = 0.2)

# Visualising both
library(gridExtra)
grid.arrange(sep, com)
?gridExtra


params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

# QQ plot to see if it follows a normal distribution
params
titanic %>% 
  filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age)) + 
  geom_qq(dparams = params) +
  geom_abline()


str(titanic)

# Position dodge separates the filled groups
titanic %>%
  ggplot() +
  geom_bar(aes(x = Sex, fill = Survived), position = position_dodge())





# Density plots for age vs survival
titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)


titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)


# Survival by Fare
titanic %>% 
  filter(!is.na(Fare) & Fare > 0) %>%
  ggplot(aes(x = Survived, y = Fare, color = Survived)) +
  geom_boxplot() + 
  scale_y_continuous(trans = "log2") +
  geom_jitter(width = 0.1, alpha = 0.2)



# Passenger Class with survival - bar plots
# Normal
titanic %>% 
  filter(!is.na(Pclass)) %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar()


# Position fill
titanic %>% 
  filter(!is.na(Pclass)) %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(position = position_fill())

# Survival by position fill
titanic %>% 
  filter(!is.na(Pclass)) %>%
  ggplot(aes(x = Survived, fill = Pclass)) +
  geom_bar(position = position_fill())



# Survival by age, sex and pclass
titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.3) + 
  facet_grid(Sex~Pclass)

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, y = ..count..)) +
  geom_density(alpha = 0.3) + 
  facet_grid(Sex~Pclass)

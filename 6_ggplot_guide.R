library(tidyverse)
library(dslabs)
data("murders")


# While creating ggplot graphs, there are several components
# 1. Data component
# 2. Geometric component
# 3. Aesthetic mapping component
# 4. Scale component
# 5. Labels, title legend etc


# Syntax for creating a scatter plot
# geom point is a layer added to the ggplot object, and is the syntax for scatterplot
# aes is the aesthetic mapping function
ggplot(data = murders) + geom_point(aes(x = population/10^6, y= total))

ggplot(data = murders) + 
  geom_point(aes(x = population/10^6, y= total)) +
  geom_text(aes(x = population/10^6, y= total, label = abb), nudge_x = 1)


# global aes
ggplot(data = murders, aes(x = population/10^6, y= total, label = abb)) + 
  geom_point(size = 2) +
  geom_text(nudge_x = 1)



# Scales, Labels and colors
# Change scale to log scale
ggplot(data = murders, aes(x = population/10^6, y= total, label = abb)) + 
  geom_point(size = 2) +
  geom_text(nudge_x = 0.075) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

## ORRR

ggplot(data = murders, aes(x = population/10^6, y= total, label = abb)) + 
  geom_point(size = 2) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()


# Labels

ggplot(data = murders, aes(x = population/10^6, y= total, label = abb)) + 
  geom_point(size = 2) + # Removing only this to allow for edit
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions with log10 scale") +
  ylab("Total murders in log10 scale") +
  ggtitle("Gun murders in USA w.r.t. population size")


graph <- ggplot(data = murders, aes(x = population/10^6, y= total, label = abb)) + 
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions with log10 scale") +
  ylab("Total murders in log10 scale") +
  ggtitle("Gun murders in USA w.r.t. population size")


graph + geom_point(size = 3, aes(col = region))

# To get an abline (i.e. avg line for usa), the formula is y = mx + c where y is total murders, 
# m is the murder rate avg for usa, and x is the population
avg_rate <- sum(murders$total)/sum(murders$population)  * 10^6




# Adding ABline, and changing color of points acc to region
graph <- graph + 
  # lty == line type, and place the abline first to keep the line behind the dots
  geom_abline(intercept = log10(avg_rate), lty = 2, color = "darkgrey") +
  # Place the col argument outside the aes for common coloring
  geom_point(aes(col = region), size = 3)


## More Customization of the Legend
graph <- graph + scale_color_discrete(name = "Region")
graph


library(ggthemes)
graph + theme_fivethirtyeight()


# ggrepel package is required to properly render the labels making them legible
library(ggrepel)


# Now to do it all again customizing it properly
# the abline slope
avg_rate


ggplot(data = murders, aes(x = population / 10^6, y = total, label = abb)) +
  geom_text_repel(max.overlaps = 30) +  # The only change is here
  geom_abline(intercept = log10(avg_rate), col = "darkgrey", lty = 2) +
  geom_point(aes(col = region), size = 3) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions log scale") +
  ylab("Total no. gun murders log scale") +
  ggtitle("Gun murders in USA in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()







## Male heights histogram
data("heights")
mh <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))



## Male heights histogram
mh + 
  geom_histogram(binwidth = 1, fill = "grey", col = "black") +
  xlab("Height of male students in inches") +
  ylab("Count of number of students") +
  ggtitle("Distribution of Male Student Heights") +
  theme_economist()



## Male heights density
mh + 
  geom_density() +
  xlab("Height of male students in inches") +
  ylab("Count of number of students") +
  ggtitle("Distribution of Male Student Heights")




# QQ plot
# Need to redefine mh, as aes needs sample for this function
mh_qq <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(sample = height))


# Making a qq plot, however this is compared to a normal distribution
mh_qq + 
  geom_qq() 


# To compare it to the theoretical distribution of height, we have to give the 
# dparams argument
params <- heights %>% 
  filter(sex == "Male") %>%
  summarise(mean = mean(height), sd = sd(height))

mh_qq + 
  geom_qq(dparams = params) +
  geom_abline()


# You can also scale the height before plotting the qq plot, 
# This saves us from creating the params arguments, and hence a little cleaner



# Grid Plots
library("gridExtra")


p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "grey", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "grey", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "grey", col = "black")

grid.arrange(p1, p2, p3)
grid.arrange(p1, p2, p3, ncol = 3)

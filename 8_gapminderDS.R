library(dslabs)
library(tidyverse)
data("gapminder")


head(gapminder)

# Compare countries - Sri Lanka and Turkey in 2015
gapminder %>% 
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
  select(country, infant_mortality)


gapminder %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  summary()


library(ggrepel)
library(gridExtra)

gap <- gapminder %>% filter(year == 2014)


g1962 <- ggplot(data = gap2016, aes(x = fertility, y = life_expectancy, label = country)) +
  geom_point(aes(size = population/10^6, col = continent)) +
  geom_text_repel()
  
g1962
g2014 <- ggplot(data = gap, aes(x = fertility, y = life_expectancy, label = country)) +
  geom_point(aes(size = population/10^6, col = continent)) +
  geom_text_repel()
g2014

grid.arrange(g1962, g2014)




## Facet Grid in ggplot

gapminder %>% 
  filter(year %in% seq(1962, 2012, 50)) %>%
  ggplot(aes(x= fertility, y = life_expectancy, col = continent)) +
  geom_point(aes(size = population/10^6)) +
  facet_grid(.~year)



gapminder %>% 
  filter(year %in% seq(1960, 2012, 10)) %>%
  ggplot(aes(x= fertility, y = life_expectancy, col = continent)) +
  geom_point(aes(size = population/10^6)) +
  facet_wrap(~year)




# Now to see if the changes were uniform acc to time or country
## TIME series plots

filter(gapminder, country %in% c("South Korea", "Germany")) %>%
  ggplot(aes(x = year, y = fertility, col= country)) +
  geom_line()




# To avoid the legend all together, as it is not preferred in time series plots
countries <- c("South Korea", "Germany")
# Giving country names and position of the label text
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
filter(gapminder, country %in% countries) %>%
  ggplot(aes(x = year, y = life_expectancy, col= country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")






## Transformations
gapminder_u <- gapminder %>%
  mutate(dollarspday = gdp/population/365)


ggplot(data = filter(gapminder_u, year == 1970), aes(x=dollarspday)) +
  geom_histogram(binwidth = 1, fill = "grey", col="black")


# transforming to log2 scale before plotting
ggplot(data = filter(gapminder_u, year == 1970), aes(x=log2(dollarspday))) +
  geom_histogram(binwidth = 1, fill = "grey", col="black")



# transforming after plotting
gapminder_u %>%
  filter(year == 1970) %>%
  ggplot(aes(dollarspday)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")




# Boxplots to compare regions with income
gapminder_u %>%
  filter(year == 1970) %>%
  ggplot(aes(x=region, y=dollarspday)) +
  geom_boxplot() +
  # To make the x axis readable
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Reorder function
vec_a <- c("East", "East", "West", "West", "West")
vec_b <- c(10, 9, 2, 5, 22)

reorder_vector <- reorder(vec_a, vec_b, FUN = median)
reorder_vector
levels(reorder_vector)

# Reorder the entries shown
gapminder_u %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollarspday, FUN = median)) %>%
  ggplot(aes(x=region, y=dollarspday, fill = continent)) +
  geom_boxplot() +
  geom_point(show.legend = FALSE) +
  # To make the x axis readable
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(trans="log2")
  
# We see that 4 groups are quite rich
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

gapminder_u %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollarspday)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ group)

# We see that the west countries are actually quite richer and skewed to the left
# Developing countries skkewed to the right

# 40 years later.....

gapminder_u %>%
  filter(year %in% c(1970, 2010) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollarspday)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)


# But as not all countries were present in 1970, we can filter out those that had
# been represented in the earlier date as well

early <- gapminder_u %>% filter(year == 1970 & !is.na(dollarspday)) %>% .$country
late <- gapminder_u %>% filter(year == 2010 & !is.na(dollarspday)) %>% .$country
common <- intersect(early, late)


# The new graph
gapminder_u %>%
  filter(year %in% c(1970, 2010) & country %in% common & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollarspday)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)


# To compare the box plots instead:

gapminder_u %>%
  filter(year%in%c(1970, 2010) & country %in% common & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollarspday, FUN = median)) %>%
  ggplot(aes(x=region, y=dollarspday, fill = continent)) +
  geom_boxplot() +
  geom_point(show.legend = FALSE) +
  # To make the x axis readable
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(trans="log2") +
  facet_grid(.~year)



# To compare years in one plot
gapminder_u %>%
  filter(year%in%c(1970, 2010) & country %in% common & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollarspday, FUN = median)) %>%
  ggplot(aes(x=region, y=dollarspday, fill = factor(year))) +
  geom_boxplot() +
  # To make the x axis readable
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(trans="log2")




# smooth density plots - area under each curve adds to 1
gapminder_u %>%
  filter(year == 1970 & country %in% common) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  group_by(group) %>%
  summarize(n = n()) 




# Density plots
gapminder_u %>%
  filter(year%in%c(1970, 2010) & country %in% common & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(x=dollarspday, y=..count.., fill = group)) + # y axis argument is 
  # generally not required but we have put a calculated field of the geom_density 
  # function to prevent misrepresentation of the western countries as density 
  # plots tend to have to add up to 1. Count is used instead here. 
  geom_density(alpha = 0.2, bw = 0.75) + # bw argument to smoothen the plot
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)



# Now to further view the developing world, we have to categorise even further
# Using case_when
gapminder_u <- gapminder_u %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels, so that they stavk accordingly in the stacked dens_plot
gapminder_u <- gapminder_u %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))


# Unstacked Density Plot
gapminder_u %>%
  filter(year%in%c(1970, 2010) & country %in% common & !is.na(gdp)) %>%
  ggplot(aes(x=dollarspday, fill = group, col = group)) +  
  geom_density(alpha = 0.2, bw = 0.75) + 
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)


# Stacked Density plot. All that changes is the stack argument in the geom_density
gapminder_u %>%
  filter(year%in%c(1970, 2010) & country %in% common & !is.na(gdp)) %>%
  ggplot(aes(x=dollarspday, fill = group, col = group)) +  
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + 
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)





# We can go even further by adding a weight to the densities according to the 
# population size as currently all the countries are weighted the same. 
# Population sizes should correlate with the weight of representation
gapminder_u %>%
  filter(year%in%c(1970, 2010) & country %in% common & !is.na(gdp)) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(x=dollarspday, fill = group, col = group, weight = weight)) +  
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + 
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)



# Limit, Break Arguments
# Lets study the rate of child survival vs income

# add additional cases
gapminder_u <- gapminder_u %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))


# Get a new df with the required data

surv_income <- gapminder_u %>%
  filter(year == 2010 & !is.na(group) & !is.na(infant_mortality) & !is.na(gdp)) %>%
  group_by(group) %>%
  summarise(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality*population/1000)/
              sum(population)) 

surv_income

ggplot(data = surv_income, aes(y = infant_survival_rate, x = income, label = group, col = group)) +
  geom_label(size = 3, show.legend = FALSE) +
  scale_x_continuous(trans = "log2", limits = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", 
                     limits = c(0.85, 0.9981), 
                     breaks = c(0.85, 0.90, 0.95, 0.99, 0.995, 0.998))






# Data Visualisation Principles
# Start At 0
# Avoid Pie Charts
# Do not distort quantities
# Order by meaningful parameters - rarely is alphabetical helpful
# Jitter and alpha blending in case you wanna show individual points in a dataset
data("heights")
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)
# Use common Axes
# Align plots vertically or horizontally to see changes horizontally or vertically respectively
# Transformations - log ones done previously to help see multiplicative changes easier
# Change color use to color blind friendly  



?paste0
?spread
# Bland Altman Plot
library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) + # ?This does not seem to work here
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")







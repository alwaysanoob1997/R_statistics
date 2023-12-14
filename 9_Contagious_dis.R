library(dslabs)
library(tidyverse)
data("us_contagious_diseases")


str(us_contagious_diseases)
head(us_contagious_diseases)


study_disease <- "Measles"

dat <- us_contagious_diseases %>% 
  filter(!state %in% c("Alaska", "Hawaii") & disease == study_disease) %>%
  mutate(rate = count/population * 10000, state = reorder(state, rate)) 


# Line plot of measles in california
dat %>% 
  filter(state == "California") %>%
  ggplot(aes(x = year, y = rate)) + 
  geom_line() +
  geom_vline(xintercept=1963, col = "blue") # Adding a variable line when the vaccine was introduced



# TIle Plot
# RColorBrewer package provides color schemes
# Sequential color palettes are best suited for data that span from high to low. 
# Diverging color palettes are best suited for data that are centered and 
# diverge towards high or low values.

dat %>% 
  ggplot(aes(year, state)) + 
  geom_tile(aes(fill = rate), color = "grey50") +
  geom_vline(xintercept=1963, col = "blue") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  theme_minimal() + 
  theme(panel.grid = element_blank()) +
  ggtitle(study_disease) +
  xlab("") +
  ylab("")



# Position and length are stronger cues than color for numeric values, but color
# can be appropriate sometimes.

avg <- us_contagious_diseases %>%
  filter(disease == study_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")





# Significant digits
# Functions for the same  - signif, round, options(digits = n)

num_my <- 123.123456
signif(num_my, digits = 1)
round(num_my, 3)











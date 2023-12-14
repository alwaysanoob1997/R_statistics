# Significance testing for binary, ordinal or categorical data

# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
?research_funding_rates


# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)



# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))


# Lady tasting tea problem
# A friend of a statistician - Fischer, claimed that she could tell whether 
# tea or milk was first poured into a cup. He gave her 4 pairs of cups with one of 
# each pair containing both alternatives. She could tell every single time. 

# On trying to calculate the probability of this,
# There are total of 8 cups with 4 tea first and 4 milk first. We can think of this
# as getting 4 tea first right - a binomial problem
# There are 8 choose 4 combinations possible.

# The null hypothesis is that she was guessing randomly and so...
# If it is a random choice, in order to get 3 right, there are 4 choose 3 * 4 choose 1
# possibilities out of 8 choose 4 options. 

# In order to get all right, there are 4 choose 4 * 4 choose 0 out of 8 C 4
library(gtools)
total_prob <- nrow(combinations(8, 4))

options3 <- nrow(combinations(4, 3)) * nrow(combinations(4, 1))
options4 <- nrow(combinations(4, 4))

prob_higher_3 <- (options3 + options4) / total_prob
# About 0.24. This is the p-value for getting 3 right when randomly guessing.


# Function called fisher.test that does this for you. You need to create a matrix
tab <- matrix(c(3, 1, 1, 3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab # This represents guessing 3 correct

# p-value calculation with fisher's exact test
fisher.test(tab, alternative = "greater")
# Didn't really get this yet, will hope it is explained later.
# This fisher test is used for hypergeometric 2x2 distributions I think

# We can get only the p-value like this. It is similar to the one we had already
# calculated
fisher.test(tab, alternative = "greater")$p.value









# Now.. going back to the first problem of the award winning.
totals
chi <- tibble(awarded = c("yes", "no"),
              men = c(totals$yes_men, totals$no_men),
              women = c(totals$yes_women, totals$no_women))

award_rate <- totals %>%
  mutate(t_awards = yes_men + yes_women,
         t_people = yes_men + yes_women + no_men + no_women,
         rate = t_awards/t_people) %>%
  .$rate


unbiased_chi <- tibble(awarded = c("yes", "no"),
                       men = c(award_rate * (totals$yes_men + totals$no_men), 
                               (1-award_rate) * (totals$yes_men + totals$no_men)),
                       women = c(award_rate * (totals$yes_women + totals$no_women), 
                                 (1-award_rate) * (totals$yes_women + totals$no_women)))

# We see a difference here, of 20. Let's see if that is random or if it's significant



chisq.test(select(chi, -awarded))
# We get a p-value of 0.051 or so. Is this really relevant?



# Odd's ratio
# odds of getting funding for men
odds_men <- (chi$men[1] / sum(chi$men)) /
  (chi$men[2] / sum(chi$men))

# odds of getting funding for women
odds_women <- (chi$women[1] / sum(chi$women)) /
  (chi$women[2] / sum(chi$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women
# The ratio is about 1.23

# In a 2 by 2 table, an odd's ratio may provide a better estimate of significance 
# than p-values as the p-value can be tiny with a large enough samplesize. But 
# the odds will remain the same

# For example, the same code as above, but sample sizes increased
chi <- tibble(awarded = c("yes", "no"),
              men = c(totals$yes_men * 10, totals$no_men * 10),
              women = c(totals$yes_women *10, totals$no_women *10))
chisq.test(select(chi, -awarded))

odds_men <- (chi$men[1] / sum(chi$men)) /
  (chi$men[2] / sum(chi$men))

odds_women <- (chi$women[1] / sum(chi$women)) /
  (chi$women[2] / sum(chi$women))

odds_men/odds_women

# We see that the p-value has gotten much smaller, but the odds has remained the same

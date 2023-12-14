beads <- rep(c("blue", "red"), times = c(2, 3))

beads
class(beads)
str(beads)



sample(beads, 2)

# Monte Carlo simulation means to perform an action a large enough number of times
# such that it is similar to if done forever
# replicate function to perform monte carlo simulation

events <- replicate(10000, sample(beads, 1))
table(events)

prop.table(table(events))


# We can also do this with sample without replicate
# Set the sampling to be done with replacement, which by default is false
sample_events <- sample(beads, 10000, replace = TRUE)
prop.table(table(sample_events))

set.seed(1986, sample.kind = "Rounding")
round(runif(1, 0 ,100))
runif(4)

paste(as.character(1:5), letters[1:5])
letters



# to create a deck of cards
suit <- c("Diamonds", "Hearts", "Clubs", "Spades")
number <- append(as.character(1:10), c("K", "Q", "J"))
deck <- (expand.grid(number = number, suit = suit))
deck <- paste(deck$number, deck$suit)
deck


kings <- paste("K", suit)
mean(deck %in% kings)


library(gtools) # permutations and combinations in this package
# Both of these functions by default does not replace
permutations(10, 7, v = 0:9)
combinations(5, 2)
class(permutations(5, 2))



# Now to check probability of getting 2 kings without replacement
all_combos <- permutations(52, 2, v = deck)
first_card <- all_combos[,1]
second_card <- all_combos[,2]

# Check probability of 2 kings occurring directly
mean(first_card %in% kings & second_card %in% kings)



# R version of the formula to check the correctness of the probability formula
# P(B |A) = P(A n B)/P(A)
one_g_two <- sum(first_card %in% kings & second_card %in% kings) / 
  sum(first_card %in% kings)

# P(A)
one<- mean(first_card %in% kings)

# P(A n B) = P(B|A) * P(A)
one * one_g_two



# Blackjack, you win if you get 21 points, i.e. Ace + a face card or a 10
# Ace can be 1 or 11 points
# To compute the probability of getting that in the first draw is...

ace <- paste("1", suit)
face_10 <- expand.grid(number = c("K", "Q", "J", "10"), suit = suit)
face_10 <- paste(face_10$number, face_10$suit)
face_10


hand <- combinations(52, 2, v = deck)
first_card <- hand[,1]
second_card <- hand[,2]

# Directly checking the probability....
mean((first_card %in% ace | first_card %in% face_10) & (second_card %in% ace | second_card %in% face_10))
### This was wrong ---


# Now to get the correct one
mean(first_card %in% ace & second_card %in% face_10)


# Which gives the same as this..., as we have used combinations here
mean((first_card %in% ace & second_card %in% face_10) | (second_card %in% ace & first_card %in% face_10))
# had we used permutations, we would need this particular code above

# This piece of code gives the probability of 0, as these combinations probably 
# coincidentally came up in the first way we checked. Safer method is the second way of checking
mean(second_card %in% ace & first_card %in% face_10)



# Using Monte Carlo to define the probability practically

# this gives 2 random cards
sample(deck, 2)

# Number of simulations
B <- 10000

# Replicate is a function that is a wrapper of sapply used for functions that 
# have random number generation
# We are creating a vector of length B that satisfies the condition specified, 
# and then calculating a mean
results <- replicate(B, {
  hand <- sample(deck, 2)
  return ((hand[1] %in% ace & hand[2] %in% face_10) | (hand[2] %in% ace & hand[1] %in% face_10))
})

mean(results)







# BIRTHDAY PROBLEM
# In a class of 50, probability of atleast 2 having the same birthday

n <- 50
possible <- 365

# Probability of unique bdays for all ==
all_probs = array(dim = n)
for (y in 1:n){ 
  prob <- (possible - y + 1)/possible
  print(prob)
  all_probs[y] <- prob
}

prod(all_probs)

# Atleast 2 having same bday ==   
1 - prod(all_probs)
  

# Using Monte Carlo simulations
B <- 10000
result <- replicate(B, any(duplicated(sample(possible, n, replace = TRUE))))

mean(result)



# Create a function "compute" that can provide the monte carlo simulations for 
# n number of people
compute <- function(n, B = 10000) {
  result <- replicate(B, any(duplicated(sample(possible, n, replace = TRUE))))
  mean(result)
}

compute(50)

# However, if we want to check the number of people where the probability crosses 50%, 
# using monte carlo simulations, we can use the same function, but cannot 
# apply it directly to the vector
num <- c(1:50)

compute(num)

# But we can use sapply for the same, to perform compute over a vector
prob_dist <- sapply(num, compute)

# We see that 23 people have a 50% chance and 32 have a 75% chance
compute(23)
compute(32)

plot(x = num, y = prob_dist)
abline(h = 0.5)
abline(h = 0.75)
abline(v = 23)
abline(v = 32)

seq(5, 5)

# A function to check exact probabilities rather than monte carlo simulations
exact_prob <- function(n){
  probs <- seq(365, 365-n+1)/365
  return (1-prod(probs))
}


exact_prob(50)


# Now to create the graph again 
exact_prob_dist <- sapply(num, exact_prob)
plot(x = num, y = exact_prob_dist)
abline(h = 0.5)
abline(h = 0.75)
abline(v = 23)
abline(v = 32)


# To compare in one graph
plot(num, prob_dist)
lines(num, exact_prob_dist, col = "red")

# We see that the graphs are actually quite similar.
# The takeaway being that if we are not able to compute the exact probabilities,
# Monte Carlo simulations may sometimes be an alternative




seq(1, 5, len = 10)
?seq




## How many Monte Carlo simluations are enough? Let C ==
C <- 10^seq(1, 5, len = 100)


new_compute <- function(B, n=60) {
  result <- replicate(B, any(duplicated(sample(possible, n, replace = TRUE))))
  mean(result)
}

variablity <- sapply(C, new_compute)


plot(x = log10(C), y = variablity, type = "l")




# Monte Carlo Simulations for the Monte Hall problem
N <- as.character(1:3)

game <- function(doors) {
  correct <- sample(doors, 1)
  choice <- sample(doors, 1)
  m_choice <- sample(doors[!doors %in% c(correct, choice)], 1)
  second_choice <- sample(doors[!doors %in% c(choice, m_choice)], 1)
  return(second_choice == correct)
}


simulations <- replicate(10000, game(N))

mean(simulations)


# Look at this code as their solution
doors <- as.character(1:3)
prize <- sample(c("car","goat","goat"))    # puts prizes in random order # Dayummm
prize_door <- doors[prize == "car"]    # note which door has prize
prize_door


sample(1:5)



outcomes <- replicate(100000, sample(0:1, 1))
mean(outcomes)




## Trial
# Cavs winning a tournament after losing the first game where the total is 7 games 
# and 4 to win

# Assign a variable 'n' as the number of remaining games.
n <- 7-1

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes <- c(0,1)

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
l <- rep(list(outcomes), n)

# All possibilities
possibilities <- expand.grid(l)

# Answer
results <- rowSums(possibilities) >= 4

mean(results)





# Monte Carlo version
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains at least four wins for the Cavs.
results <- replicate(B, {
  one_game <- sample(c(0, 1), 6, replace = TRUE)
  return (sum(one_game) >= 4)
})

mean(results)








# Assessment
# In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). In the 2012 Olympics, 3 of the 8 runners were from Jamaica and the other 5 were from different countries. The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).

# How many different ways can the 3 medals be distributed across 8 runners?
medals <- permutations(8, 3)
nrow(medals)

# Ways of distributution among jamaicans
jamaicans <- permutations(3,3)
nrow(jamaicans)

# What is the probability that all 3 medals are won by Jamaica?
possible <- permutations(8, 3)
mean(rowSums(possible) <= 6) # This is representing the jamaicans as 1, 2, 3 and then counting the sum


# A better and a more direct method:
nrow(jamaicans)/nrow(medals)

# Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

set.seed(1, sample.kind = "Rounding")

B <- 10000
results <- replicate(B, {
  winners <- sample(runners, 3)
  return (all(winners == "Jamaica"))
})

mean(results)


# Q2

# A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year. He doesn't think his current special actually allows that number of choices, but wants to change his special if needed to allow at least 365 choices.

# A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.


entree <- c(1:12)

side_comb <- nrow(combinations(6, 2))
drinks <- 3

choices <- function(e){
  return(e*side_comb*drinks)
}

valid <- sapply(entree, choices)
which(valid > 365)


# Min number of side choices to get 365 choices

sides <- c(2:12)

entrees <- 6
drinks <- 3

choices_sides <- function(s){
  return(6*3*nrow(combinations(s, 2)))
}

valid <- sapply(sides, choices_sides)
which(valid > 365)



# Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1

#Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. The built-in dataset esoph contains data from a case-control study in France comparing people with esophageal cancer (cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a variety of demographic and medical characteristics. The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases and controls grouped by age range (agegp).

library(tidyverse)
esoph

str(esoph)
head(esoph)
?esoph


all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)


# What is the probability that a subject in the highest alcohol consumption group is a cancer case?

length(max(esoph$alcgp))
sum(esoph$alcgp == max(esoph$alcgp))

ans <- esoph %>%
  filter(alcgp == min(alcgp)) %>%
  summarise(proportion = sum(ncases)/(sum(ncontrols) + sum(ncases)))
signif(ans, digits = 3)

# Given that a person is a case, what is the probability that they smoke 10g or more a day?
str(esoph)

esoph %>%
  group_by(tobgp) %>%
  summarise(cases = sum(ncases)) %>%
  mutate(above_10 = ifelse(tobgp >= "10-19", 1, 0)) %>%
  group_by(above_10) %>%
  summarise (total = sum(cases), proportion = total/200)



esoph %>%
  group_by(tobgp) %>%
  summarise(controls = sum(ncontrols)) %>%
  filter(tobgp >= "10-19") %>%
  pull(controls) %>%
  sum()/all_controls




case_a <- esoph %>%
  filter(alcgp == max(alcgp)) %>%
  pull(ncases) %>%
  sum()/all_cases


case_t <- esoph %>%
  filter(tobgp == max(tobgp)) %>%
  pull(ncases) %>%
  sum()/all_cases



case_aat<- esoph %>%
  filter(tobgp == max(tobgp) & alcgp == max(alcgp)) %>%
  pull(ncases) %>%
  sum()/all_cases




case_aot<- cases <- esoph %>%
  filter(tobgp == max(tobgp) | alcgp == max(alcgp)) %>%
  pull(ncases) %>%
  sum()/all_cases



control_a <- esoph %>%
  filter(alcgp == max(alcgp)) %>%
  pull(ncontrols) %>%
  sum()/all_controls

signif(control_a, 3)

signif(case_a/control_a, 3)


control_t <- esoph %>%
  filter(tobgp == max(tobgp)) %>%
  pull(ncontrols) %>%
  sum()/all_controls


control_aat <- esoph %>%
  filter(tobgp == max(tobgp) & alcgp == max(alcgp)) %>%
  pull(ncontrols) %>%
  sum()/all_controls
signif(control_aat, 3)


control_aot <- esoph %>%
  filter(tobgp == max(tobgp) | alcgp == max(alcgp)) %>%
  pull(ncontrols) %>%
  sum()/all_controls
signif(control_aot, 3)


signif(case_aot/control_aot, 3)

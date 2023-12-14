library(tidyverse)

# Simulating a bank that loans money. We have to calculate the ideal interest 
# rate that does not lose money and does not lose customers


# Assume the bank is ready to give out a 1000 loans of 180000
n <- 1000
loan <- 180000
# Assume 2% people default on their payments every loan
p <- 0.02
# And if they do, the loss to the bank is 200000
loss_per_default <- -200000

# You can adjust for this loss, by charging an interest rate high enough to adjust 
# for it, but low enough not to lose customers

loss_per_loan <- p * loss_per_default + (1-p) * 0 # As we dont gain anything from a loan right now




# Monte Carlo defaults == 
defaults <- sample(c(0, 1), n, replace = TRUE, prob = c(1-p, p))
loss_total <- sum(defaults * loss_per_default)
loss_total

# Monte carlo simulation 10000 times for identifying the amount lost 
B <- 10000
losses <- replicate(B, {
  defaults <- sample(c(0, 1), n, replace = TRUE, prob = c(1-p, p))
  return(sum(defaults * loss_per_default))
})

sd(losses)

# Plotting the distribution of losses ==
data.frame(losses_in_mil = losses/10^6) %>%
  ggplot(aes(x = losses_in_mil)) +
  geom_histogram(binwidth = 0.6, col = "black")




# As per CLT, loss over the whole set of loans
expected <- n * loss_per_loan
se <- sqrt(n) * abs(loss_per_default - 0) * sqrt(p * (1-p))

# To correct this loss, we need to adjust the loss per loan so as to make it 0 ==
# p * loss_per_default  + (1-p) * x == 0
interest <- -  (p * loss_per_default)/(1-p)
rate <- interest / loan # approx 2%


# This interest rate ensures that on average, our losses will be zero, i.e. 50% 
# of the times, we will lose money
# We need to make sure that this rarely happens, so that we dont lose customers 
# by going too high either. Lets try to make the losses <= 1%:

# Let S be the sum of the loan returns We need the P(S < 0) = 0.01
# We can modify the "S < 0" part of the equation, by converting it to a z score
# Now the equation reads -- P(Z < -1 * mean/SD) where the mean and SD of the 
# distribution of the loan returns

# Now, thinking about the loan return sums, applying CLT, we can approximate it 
# to a normal distribution. This means we can get a value z which refers to the 
# z score that corresponds to the 1st quantile.
# i.e. the z score of the interest that represents the 1st quantile
z = qnorm(0.01)

# Which also means that we can write the first equation as P(Z < z) = 0.01
# equating the right side of the last 2 equations, we get --
# z = - mean/SD, i.e.
# z == -1 * (n * (p  * loss_per_default + (1-p) * interest)/(sqrt(n) * abs(loss_per_default - interest) * sqrt(p * (1-p))))
# Where the only unknown variable is what interest we need to charge
# Therefore, to find out interest, solving for interest, we get ==
interest <- - loss_per_default * (n * p - z * sqrt(n * p * (1-p))) / (n * (1-p) + z * sqrt(n * p * (1-p))) 
interest/loan # interest rate of about 3 %

# Now the expected profit per loan ==
p * loss_per_default + (1-p) * interest # about 2000 per loan profit



# running a monte carlo with the given interest and loss levels, we can confirm the above
monte <- replicate(10000, {
  trial <- sample(c(interest, loss_per_default), n, replace = TRUE, prob = c(1-p, p))
  return(sum(trial))
})
mean(monte < 0)
mean(monte)
# This confirms our calculations above






# This is the basics now. The Big Short is as follows
# Looking at the previous equation z = -mean/sd, we can also right it as ==
# z = -n* mean (for single event)/ sqrt(n) * sd (for single event)
# Here the previous example also holds if z <= -mean/sd. Which means we can again rewrite as
# z <= n*mean(s) / SD(s)
# n >= z^2 * SD(s) ^2 / mean(s)^2.
# This means the prob can be maintained if n is larger than the RHS. 
# This is kind of a version of Law of Large Numbers 
# Applying this to the previous example
n = ceiling(z^2 * (abs(interest - loss_per_default) * sqrt(p * (1-p)))^2 / (p * loss_per_default + (1-p) * interest)^2)
n # you just need 1001 people to take your loan

# Now if the defaulting rate is 0.04 and we change the interest rate to 5%
p <- 0.04
interest <- 0.05 * loan
# Here the expected value is...
p*loss_per_default + (1-p) * interest

n = ceiling(z^2 * (abs(interest - loss_per_default) * sqrt(p * (1-p)))^2 / (p * loss_per_default + (1-p) * interest)^2)
n # With these changes, we would need 22000 people to take the loan, #LLN
# We can confirm with the monte carlo simulation for the same situation

profit <- replicate(10000, {
  trial <- sample(c(interest, loss_per_default), n, replace = TRUE, prob = c(1-p, p))
  return(sum(trial))
})
mean(profit < 0) # As we can see, it is about 1%

# However, we have derived this n from the assumption that the CLT holds valid

# The Central Limit Theorem does not hold when the events are not independent of
# each other. As is the case with crashes of economic markets, the default rates
# are influenced by an outside factor and the events are hence not independent of each other

# We can simulate this with a monte carlo simulation, where the default ratesfor
# all borrowers vary with each simulation between 3% to 5%

p <- 0.04
interest <- 0.05 * loan

profit <- replicate(10000, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  trial <- sample(c(interest, loss_per_default), n, replace = TRUE, prob = c(1-new_p, new_p))
  return(sum(trial))
})
mean(profit < 0) # Here we see that the chance of loss is much higher than 1%, 
# as the default rate changed and the n and interest rate did not.  
data.frame(profit_tr = profit) %>%
  ggplot(aes(x=profit_tr)) +
  geom_histogram()
# The plot shows a non normal distribution which explains the issue with the thinking 
# that the default rates are independent. CLT does not apply




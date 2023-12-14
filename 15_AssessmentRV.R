# Expexted value of points
1/5 * 1 + 4/5 * -0.25 # is 0

# 44 questions in the exam
# E(X) for guessing on all questions is 0
se <- sqrt(44) * abs(1 - -0.25) * sqrt(1/5*4/5)
se


pnorm(8, mean = 0, sd = se, lower.tail = FALSE)


set.seed(21, sample.kind = "Rounding")

# Monte carlo for 10000 students
n <- 10000
results <- replicate(n, sum(sample(c(1, -0.25), 44, replace = TRUE, prob = c(1/5, 4/5))))
mean(results > 8)


# Now if the rules change, penalty is gone and the choices are now 4

expected <- 44 * (1/4 * 1 + 3/4 * 0)
expected

se <- sqrt(44) * sqrt(1/4 * 3/4)


# Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) 
# representing a range of student skills.

# What is the lowest p such that the probability of scoring over 35 exceeds 80%?

p <- seq(0.25, 0.95, 0.05)

my_mean <- function(percent, n = 44) {
  return(1 - pnorm(35, mean = n * percent, sd = sqrt(n * percent * (1-percent))))
}


best <- sapply(p, my_mean)
min(p[which(best > 0.8)])





# Round 2
win <- 5/38
win_amt <- 6
lose_amt <- -1

# Expected payout for 1 bet ==
exp_pay <- win * win_amt + (1-win) * lose_amt
exp_pay

# Sd for 1 bet ==
se_pay <- abs(win_amt - lose_amt) * sqrt(win * (1 - win))
se_pay


# SD for avg payout for 500 bets ==
se_pay * 1/sqrt(500)

# expected value for sum of 500 bets ==
500 * exp_pay

# SD for sum of 500 bets == 
sqrt(500) * se_pay



pnorm(0, mean = 500 * exp_pay, sd = se_pay * sqrt(500))















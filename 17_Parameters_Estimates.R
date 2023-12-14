# Lets assume a poll to predict the winners of an election. Lets assume the pool
# of voters are a collection of red and blue balls in an urn. 
# You can take a sample of this urn to try and predict the winner in the final 
# election, but a sample comes with a cost that is proportional to the size of 
# the sample - to reflect real world costs of conducting a poll. If your answer 
# comes the closest to the actual poll result, you win a prize - relecting the 
# media coverage your company would get in real life. 
# 
# The goal is to optimize the size of the sample to get an overall profit from 
# the whole procedure
#
# This question differs from the probability questions from before as here, 
# p is uncertain, and the goal is to find p. However we can apply the theories 
# we have learnt earlier.
# Suppose the proportion of blue balls is p and then proportion of red is 1-p
# Lets denote the blue balls by 1 and the red balls by 0. Thus a sum of the sample
# with N draws, gives us the total number of blue balls in the sample. And the 
# proportion of blue balls is the sum divided by N == p = (X1 + X2 + X3 ... + XN) / N
# which is also the sample average. This also means N * mean = sum(X)
#
#
# From this we can also infer that the expected value of the mean is the "p", the 
# parameter of interest. E(N * mean) = N * p == E(mean) = p
# And the standard deviation of the mean is SD(mean) = sqrt(p * (1-p) / N)
# From these both, we see that we can get more accurate value of p, if the N is 
# large such that the SD is close to 0. But since we don't know the value of p,
# choosing a large enough N might be hard. i.e. for p = 0.51, the N of 1000, gives 
# an SD of 0.15, which is quite large. 


# Spread is p - (1-p) or otherwise 2p - 1


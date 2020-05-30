Xbar<-0.48
N<-25
se<-sqrt(Xbar*(1-Xbar)/N) #plog-in estimate, p is estimated by Xbar
pnorm(0.01/se)-pnorm(-0.01/se) #probability of Xbar being within 1% of p 

# the margin of error is defined as 2 times the standard error of the estimate Xbar
# there is about 95% chance that Xbar within 2 standard errors of the actual parameter p

##Monte Carlo Simulation
p <- 0.45    # unknown p to estimate
N <- 1000

# simulate one poll of size N and determine Xbar
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
Xbar <- mean(x)

# simulate B polls of size N and determine average Xbar
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
Xbar <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})
Xbar
library(tidyverse)
library(gridExtra)
p1 <- data.frame(Xbar) %>%
  ggplot(aes(Xbar)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(Xbar) %>%
  ggplot(aes(sample = Xbar)) +
  stat_qq(dparams = list(mean = mean(Xbar), sd = sd(Xbar))) +
  geom_abline() +
  ylab("Xbar") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

## The Spread = p-(1-p) = 2p-1
# The spread between two outcomes with probabilities  p  and  1???p  is  2p???1
# The expected value of the spread is  2Xbar???1
# The standard error of the spread is  2se(Xbar)
# The margin of error of the spread is 2 times the margin of error of  Xbar

## Plotting margin of error in an extremely large poll over a range of values of p
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()

## EXERCISE
# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.
take_sample<-function(p,N){
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
}
set.seed(1)
p <- 0.45
N <- 100
take_sample(p,N)
# Distribution of error-1
errors <- replicate(B, {p-take_sample(p,N)})
mean(errors)
hist(errors)
qqnorm(errors)
qqline(errors)
# Average size of error
mean(abs(errors))
# Calculate the standard deviation of `errors`
sqrt(mean(errors^2))

# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
se<-sqrt(p*(1-p)/N)
1-pnorm((0.5-p)/se)
# Calculate the probability that the error is 0.01 or larger
pnorm(-0.01/se_hat)+1-pnorm(0.01/se_hat) 

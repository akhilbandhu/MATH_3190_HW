# Homework 5
# This script contains all the ggplot and other functions related to the homework

library(ggplot2)

# Question 1 
# Using ggplot, plot the Poisson pmf for k=0,1,....,10 when lambda=5.
ggplot(
  transform(
    data.frame(x=c(0:10)), 
    y=dpois(x, 5)), aes(x, y)) +
  geom_line()
  

# Question 2
# Using ggplot, plot the pdf for an individual xi given θ = 0.5 and also for θ = 5
theta_function <- function(x, theta) {
  return(prod(theta*x^(theta-1)))     
}

theta <- c(1:10)/5
theta_values<- NULL
for(i in theta) {
  theta_values <- c(theta_values, theta_function(c(1:9)*0.1, i)) 
}

df <- data.frame(y = theta_values, x = theta)

ggplot(
    df,
    aes(x, y)
  ) + 
  geom_line()

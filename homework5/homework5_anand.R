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


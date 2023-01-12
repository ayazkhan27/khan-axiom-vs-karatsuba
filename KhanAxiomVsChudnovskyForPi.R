# install the required packages
install.packages("microbenchmark")
install.packages("ggplot2")
install.packages("digest")

# load the packages
library(microbenchmark)
library(ggplot2)
library(digest)

# Define the axiom multiplication function
axiom_multiplication <- function(n) {
  # Initialize the variables x and y
  x <- 1
  y <- sqrt(2)
  
  # Iterate through the digits of pi
  for (i in 1:n) {
    # Calculate the next digit of pi using the axiom multiplication method
    x <- x * y / 2
    y <- sqrt(2 + y)
  }
  
  # Calculate the first n digits of pi
  pi <- 2 / x
  
  # Return the first n digits of pi as the final result
  return(pi)
}

# Define the Chudnovsky algorithm function
chudnovsky_algorithm <- function(n) {
  # Initialize the variables a, b, k, and m
  a <- 0
  b <- 0
  k <- 0
  m <- 0
  
  # Iterate through the digits of pi
  for (i in 0:n) {
    # Calculate the next digit of pi using the Chudnovsky algorithm
    a <- (12 * k + 545140134) / (2 * m + 1)
    b <- (-a * k) / (2 * m + 1)
    k <- k + 1
    m <- m + 2
  }
  
  # Calculate the first n digits of pi
  pi <- (sqrt(10005) * a) / b
  
  # Return the first n digits of pi as the final result
  return(pi)
}

# define the number of digits to calculate
n <- 1000000

# measure the execution time of the algorithms
result <- microbenchmark(axiom_multiplication(n), chudnovsky_algorithm(n))

# get a summary of the results
summary <- summary(result)

# compare the mean execution time of the two algorithms
if (summary[1, "mean"] < summary[2, "mean"]) {
  print(paste("The axiom multiplication method is", round(summary[2, "mean"] / summary[1, "mean"], 2), "times faster than the Chudnovsky algorithm."))
} else {
  print(paste("The Chudnovsky algorithm is", round(summary[1, "mean"] / summary[2, "mean"], 2), "times faster than the axiom multiplication method."))
}

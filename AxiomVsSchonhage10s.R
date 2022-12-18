# install the required packages
install.packages("microbenchmark")
install.packages("ggplot2")

# load the packages
library(microbenchmark)
library(ggplot2)

# Define the axiom multiplication function
axiom_multiplication <- function(x, y) {
  # Break down x and y into multiples of 10 and single digits
  x_tens <- floor(x/10)
  x_ones <- x - 10*x_tens
  y_tens <- floor(y/10)
  y_ones <- y - 10*y_tens
  
  # Initialize a variable z to 0
  z <- 0
  
  # Iterate through the digits of x
  for (i in 1:length(x)) {
    # Iterate through the digits of y
    for (j in 1:length(y)) {
      # Calculate the product x[i] * y[j]
      product <- x[i] * y[j]
      
      # Add the product to the corresponding element of z
      z[i+j-1] <- z[i+j-1] + product
    }
  }
  
  # Return z as the final result
  return(z)
}


# define the Toom-Cook multiplication function
toom_cook <- function(x, y) {
  # determine the size of the input vectors
  n <- length(x)
  m <- length(y)
  l <- max(n, m)
  
  # pad the input vectors with zeros to ensure that they have the same length
  if (n < l) x <- c(x, rep(0, l - n))
  if (m < l) y <- c(y, rep(0, l - m))
  
  # perform the multiplication using the Toom-Cook algorithm
  if (l == 1) {
    return(x * y)
  } else {
    l2 <- floor(l / 2)
    a <- x[1:l2]
    b <- x[(l2+1):l]
    c <- y[1:l2]
    d <- y[(l2+1):l]
    p1 <- toom_cook(a, c)
    p2 <- toom_cook(b, d)
    p3 <- toom_cook(a + b, c + d)
    p4 <- toom_cook(a + 2*b, c + 2*d)
    p5 <- toom_cook(2*a + b, 2*c + d)
    return(c(p1 * 10^(3 * l2) + (p4 - p1 - p2 - p3) * 10^(2 * l2) + (p3 - p1 - p2) * 10^l2 + p2, rep(0, l2)))
  }
}

# define the inputs for the multiplication functions
x <- rnorm(50)
y <- rnorm(50)

# measure the execution time of the multiplication functions
result <- microbenchmark(axiom_multiplication(x, y), toom_cook(x, y))

# print the results
print(result)

# visualize the results using a bar chart
ggplot(result, aes(x=expr, y=time, fill=expr)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "none") +
  labs(title = "Comparison of multiplication methods", x = "Method", y = "Execution time (ms)")

# visualize the results using a histogram
ggplot(result, aes(x=time, fill=expr)) + 
  geom_histogram(bins=50) +
  theme(legend.position = "none") +
  labs(title = "Comparison of multiplication methods", x = "Execution time (ms)", y = "Frequency")

# visualize the results using a plot
ggplot(result, aes(x=expr, y=time, color=expr)) + 
  geom_point() +
  labs(title = "Comparison of multiplication methods", x = "Method", y = "Execution time (ms)")

# get a summary of the results
summary <- summary(result)

# print the summary
print(summary)

# compare the mean execution time of the two methods
if (summary[1, "mean"] < summary[2, "mean"]) {
  print(paste("The axiom multiplication method is", round(summary[2, "mean"] / summary[1, "mean"], 2), "times faster than the Toom-Cook method."))
} else {
  print(paste("The Toom-Cook multiplication method is", round(summary[1, "mean"] / summary[2, "mean"], 2), "times faster than the axiom multiplication method."))
}

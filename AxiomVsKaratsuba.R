# install the required packages
install.packages("microbenchmark")
install.packages("ggplot2")

# load the packages
library(microbenchmark)
library(ggplot2)

# define the axiom multiplication function
axiom_multiplication <- function(x, y) {
  z <- 0
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      z[i+j-1] <- z[i+j-1] + x[i] * y[j]
    }
  }
  return(z)
}

# define the Karatsuba multiplication function
karatsuba <- function(x, y) {
  if (length(x) <= 1 || length(y) <= 1) {
    return(axiom_multiplication(x, y))
  }
  m <- max(length(x), length(y))
  m2 <- floor(m/2)
  xl <- x[1:m2]
  xr <- x[(m2+1):m]
  yl <- y[1:m2]
  yr <- y[(m2+1):m]
  z0 <- karatsuba(xl, yl)
  z1 <- karatsuba(xl + xr, yl + yr)
  z2 <- karatsuba(xr, yr)
  c(z0 + (z1 - z0 - z2) * 10^m2, z2)
}

# define the inputs for the multiplication functions
x <- rnorm(100)
y <- rnorm(100)

# measure the execution time of the multiplication functions
result <- microbenchmark(axiom_multiplication(x, y), karatsuba(x, y))

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
  print("The axiom multiplication method is faster on average.")
} else {
  print("The Karatsuba multiplication method is faster on average.")
}


# compare the mean execution time of the two methods
if (summary[1, "mean"] < summary[2, "mean"]) {
  print(paste("The axiom multiplication method is", round(summary[2, "mean"] / summary[1, "mean"], 2), "times faster than the Karatsuba method."))
} else {
  print(paste("The Karatsuba multiplication method is", round(summary[1, "mean"] / summary[2, "mean"], 2), "times faster than the axiom multiplication method."))
}
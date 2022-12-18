# Load required packages
library(microbenchmark)

# Define the "Khan-Axiom" algorithm function
axiom_multiply <- function(x, y) {
  # Break down x and y into multiples of 10 and single digits
  x_tens <- floor(x/10)
  x_ones <- x - 10*x_tens
  y_tens <- floor(y/10)
  y_ones <- y - 10*y_tens
  
  # Use FOIL method to multiply multiples of 10
  tens_product <- x_tens * y_tens
  ones_product <- x_ones * y_ones
  
  # Add products together to obtain final result
  result <- tens_product*100 + x_ones*y_tens + y_ones*x_tens + ones_product
  return(result)
}

# Define the Karatsuba algorithm function
karatsuba <- function(x, y) {
  # Check if x or y is small enough to perform simple multiplication
  if (x < 10 || y < 10) {
    return(x * y)
  }
  
  # Compute the lengths of x and y
  n <- max(nchar(x), nchar(y))
  m <- floor(n / 2)
  
  # Split x and y into high and low digits
  x_low <- x %% 10^m
  x_high <- (x - x_low) / 10^m
  y_low <- y %% 10^m
  y_high <- (y - y_low) / 10^m
  
  # Recursively compute the products of the high and low digits
  z0 <- karatsuba(x_low, y_low)
  z1 <- karatsuba((x_low + x_high), (y_low + y_high))
  z2 <- karatsuba(x_high, y_high)
  
  # Use the distributive property to combine the products
  result <- z2 * 10^(2*m) + (z1 - z2 - z0) * 10^m + z0
  return(result)
}

# Generate random input numbers for the test
set.seed(123)  # Set seed for reproducibility
inputs <- data.frame(x = sample(1e4:1e6, 1000, replace = TRUE),
                     y = sample(1e4:1e6, 1000, replace = TRUE))

# Measure the time it takes for each algorithm to multiply the input numbers
result <- microbenchmark(axiom_multiply = sapply(seq_len(nrow(inputs)), function(i) {
  axiom_multiply(inputs$x[i], inputs$y[i])
}),
karatsuba = sapply(seq_len(nrow(inputs)), function(i) {
  karatsuba(inputs$x[i], inputs$y[i])
}),
times = 1000)

# Compare the performance of the two algorithms
print(result)
boxplot(result)
# Determine which algorithm is faster and by how much
if (median(result$time[result$expr == "axiom_multiply"]) < 
    median(result$time[result$expr == "karatsuba"])) {
  faster <- "Khan-Axiom"
  slower <- "Karatsuba"
  speedup <- median(result$time[result$expr == "karatsuba"]) / 
    median(result$time[result$expr == "axiom_multiply"])
} else {
  faster <- "Karatsuba"
  slower <- "Khan-Axiom"
  speedup <- median(result$time[result$expr == "axiom_multiply"]) / 
    median(result$time[result$expr == "karatsuba"])
}

print(paste(faster, "was faster than", slower, "by a factor of", round(speedup, 2)))

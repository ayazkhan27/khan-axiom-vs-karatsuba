# Define the axiom multiplication function
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

# Define the long multiplication function
long_multiply <- function(x, y) {
  # Initialize result to 0
  result <- 0
  
  # Loop over each digit in y
  for (i in 1:nchar(as.character(y))) {
    # Extract digit from y
    digit <- substr(as.character(y), i, i)
    digit <- as.numeric(digit)
    
    # Multiply x by digit and add to result
    result <- result + x*digit
    
    # Shift x one place to the left
    x <- x*10
  }
  return(result)
}

# Generate a set of random numbers with a large number of digits
n <- 100 # number of random numbers to generate
digits <- 10 # number of digits in each random number
set.seed(123) # set seed for reproducibility
x <- round(runif(n, min = 10^(digits-1), max = 10^digits-1))
y <- round(runif(n, min = 10^(digits-1), max = 10^digits-1))

# Compare the speed of the two multiplication methods
results <- data.frame(method = character(n), time = numeric(n))
for (i in 1:n) {
  results[i, "method"] <- "axiom"
  results[i, "time"] <- system.time(axiom_multiply(x[i], y[i]))[1]
  results[i+n, "method"] <- "long"
  results[i+n, "time"] <- system.time(long_multiply(x[i], y[i]))[1]
}

# Analyze the results
library(ggplot2)
ggplot(results, aes(x = method, y = time)) + geom_boxplot()

# Create a histogram of the results
hist(results$time[results$method == "axiom"], main = "Axiom multiplication", xlab = "Time (seconds)")
hist(results$time[results$method == "long"], main = "Long multiplication", xlab = "Time (seconds)")

# Calculate the mean time for each method
mean_axiom <- mean(results$time[results$method == "axiom"])
mean_long <- mean(results$time[results$method == "long"])

# Compare the mean times and print a conclusion statement
if (mean_axiom < mean_long) {
  print("The axiom multiplication method is faster on average.")
} else if (mean_axiom > mean_long) {
  print("The long multiplication method is faster on average.")
} else {
  print("The two methods are equally fast on average.")
}

# Calculate the percentage difference between the mean times
percent_difference <- 100*(mean_axiom - mean_long)/mean_long

# Print the percentage difference
print(paste("The axiom multiplication method is", percent_difference, "% faster on average."))
axero <- function(x, y) {
  # Convert x and y to character vectors
  x_str <- as.character(x)
  y_str <- as.character(y)
  
  # Convert the character vectors to integer vectors
  x_digits <- as.integer(strsplit(x_str, "")[[1]])
  y_digits <- as.integer(strsplit(y_str, "")[[1]])
  
  # Initialize result to 0
  result <- 0
  
  # Loop through each digit of x
  for (i in 1:length(x_digits)) {
    # Skip the calculation if the x digit is 0
    if (x_digits[i] == 0) {
      next
    }
    
    # Loop through each digit of y
    for (j in 1:length(y_digits)) {
      # Skip the calculation if the y digit is 0
      if (y_digits[j] == 0) {
        next
      }
      
      # Calculate the product of the x digit and the y digit
      product <- x_digits[i] * y_digits[j]
      
      # Calculate the number of zeros to place after the product
      num_zeros <- (length(x_digits) - i) + (length(y_digits) - j)
      
      # Add the product with the appropriate number of zeros to the result
      result <- result + (product * 10^num_zeros)
    }
  }
  
  return(result)
}

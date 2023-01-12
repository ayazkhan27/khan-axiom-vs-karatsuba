axero <- function(x, y) {
  # Break down x and y into their individual digits
  x_digits <- as.integer(unlist(strsplit(as.character(x), "")))
  y_digits <- as.integer(unlist(strsplit(as.character(y), "")))
  
  # Initialize result to 0
  result <- 0
  
  # Loop through each digit of x
  for (i in 1:length(x_digits)) {
    x_digit <- x_digits[i]
    
    # Loop through each digit of y
    for (j in 1:length(y_digits)) {
      y_digit <- y_digits[j]
      
      # Calculate the product of x_digit and y_digit
      product <- x_digit * y_digit
      
      # Calculate the number of zeros to place after the product
      num_zeros <- (length(x_digits) - i) + (length(y_digits) - j)
      
      # Add the product with the appropriate number of zeros to the result
      result <- result + (product * 10^num_zeros)
    }
  }
  
  return(result)
}

# Test with same digit numbers
axero(4, 4)  # Expected output: 16

# Test with different digit numbers
axero(924, 543)  # Expected output: 501732

# Test with extremely large digit numbers
axero(9876543210, 1234567890)  # Expected output: 121932631112635269000000


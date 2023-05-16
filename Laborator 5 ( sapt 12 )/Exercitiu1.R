simulate_variable = function(x, p) 
{
  # Validate inputs
  if (length(x) != length(p)) 
  {
    stop("cele 2 siruri nu au acelasi nr de elem")
  }
  if (any(p < 0) || sum(p) != 1) 
  {
    stop("Suma probabilitatilor p trebuie sa fie pozitiva si egala cu 1.")
  }
  
  # Generate a random number between 0 and 1
  nr = runif(1)
  
  # Find the corresponding value of X based on the probabilities
  prob = cumsum(p)
  index = which(nr <= prob)[1]
  
  return(x[index])
}

# Example usage
x = c(1, 2, 3)
p = c(0.5, 0.4, 0.1)

# Simulate a random variable
value = simulate_variable(x, p)
print(value)
MC_prob_all = function(N) 
{
  count = 0
  for (i in 1:N) 
  {
    infected = 1
    nr_days = 2
    last_errors = c(18, 22, 28)
    nr_errors = 18
    while (nr_errors > 0) 
    {
      lambda = min(last_errors)
      nr_errors = rpois(1, lambda)
      last_errors = c(nr_errors, last_errors[1:2])
      infected = infected + nr_errors
      nr_days = nr_days + 1
    }
    if (infected == 40) 
    {
      count = count + 1
    }
  }
  return(count / N)
}

MC_prob_15 = function(N) 
{
  count = 0
  for (i in 1:N) 
  {
    infected = 1
    nr_days = 2
    last_errors = c(18, 22, 28)
    nr_errors = 18
    while (nr_errors > 0) 
    {
      lambda = min(last_errors)
      nr_errors = rpois(1, lambda)
      last_errors = c(nr_errors, last_errors[1:2])
      infected = infected + nr_errors
      nr_days = nr_days + 1
    }
    if (infected >= 15) 
    {
      count = count + 1
    }
  }
  return(count / N)
}

MC_prob_15_error = function(target_error, confidence) 
{
  N = 10000
  p_hat = MC_prob_15(N)
  error = 1
  while (error > target_error) 
  {
    N = N * 2
    p_hat_prev = p_hat
    p_hat = MC_prob_15(N)
    error = qnorm(1 - (1 - confidence) / 2) * sqrt(p_hat_prev * (1 - p_hat_prev) / N)
  }
  return(list(probability = p_hat, error = error))
}
# (a)
prob_all = MC_prob_all(10000)

cat("Probabilitatea ca intr-o zi toate computerele sa fie infectate: ", prob_all, "\n")

# (b) 
prob_15 = MC_prob_15(10000)

cat("Probabilitatea ca intr-o zi cel putin 15 computere sa fie infectate: ", prob_15, "\n")

# (c) 
target_error = 0.01
confidence = 0.95

result = MC_prob_15_error(target_error, confidence)

cat("Probabilitatea ca intr-o zi cel putin 15 computere sa fie infectate: ", result$probability, "\n")
cat("Eroare estimata :", result$error, "\n")
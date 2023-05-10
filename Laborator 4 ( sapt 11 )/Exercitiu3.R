lambda1=4
lambda2=12
prob_m2=3/4


if(runif(1) < prob_m2) 
{
  X = rexp(1, lambda2)
} else 
{
  X = rexp(1, lambda1)
}

cat("Timp:", X, "ore\n")
n=10000
valori=numeric(n)
for(i in 1:n) 
{
  if(runif(1) < prob_m2) 
  {
    valori[i] <- rexp(1, lambda2)
  } else 
  {
    valori[i] <- rexp(1, lambda1)
  }
}

cat("Timpul estimat:", mean(valori), "hours\n")
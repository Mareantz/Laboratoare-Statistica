#IV.2
test_proportion = function(alfa, n, succeses, p0, type)
{ 
  p_prim = succeses/n
  z_score = (p_prim - p0)/sqrt(p0*(1 - p0)/n)
  cat(z_score, "\n")
  
  if(type == "r")
  {
    critical_z = qnorm(1 - alfa, 0, 1)
    cat(critical_z, "\n")
    if(z_score > critical_z) 
      cat("\nWe reject H0")
    else 
      cat("\nWe don't have enough evidence to reject H0")
  }
  
  if(type == "l")
  {
    critical_z = qnorm(alfa, 0, 1)
    cat(critical_z, "\n")
    if(z_score < critical_z) 
      cat("\nWe reject H0")
    else 
      cat("\nWe don't have enough evidence to reject H0")
  }
  
  if(type=="s")
  {
    critical_z = qnorm(1-alfa/2, 0, 1)
    print(critical_z, "\n")
    if(abs(z_score)>abs(critical_z))
      print("\nWe reject H0")
    else
      print("\nWe don't have enough evidence to reject H0")
  }
}

test_proportion(0.05, 150, 20, 0.1, "r")
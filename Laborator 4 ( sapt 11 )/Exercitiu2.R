exII_2 = function(N) 
{
  sum = 0;
  for(i in 1:N) 
  {
    u = rexp(1, 3);
    sum = sum + exp(-2*u*u)/(3*exp(-3*u));
  }
  cat("Valoare exacta: ", sqrt(pi/8), "\n")
  cat("Eroare absoluta: ",abs(sqrt(pi/8)-sum/N),"\n")
  cat("Eroare relativa: ",(sqrt(pi/8)-sum/N)/abs(sqrt(pi/8)),"\n")
  cat("Valoarea aproximativa: ",sum/N,"\n")
}
exII_2(50000)

#II.1 (b)
MC_integration = function(N) 
{
  sum = 0;
  for(i in 1:N) 
  {
    x = runif(1, 1, 4);
    sum = sum + exp(x);
  }
  return(3*sum/N);
}

MC_integr_average= function(k, N) 
{
  estimates=0;
  for(i in 1:k)
    estimates[i] = MC_integration(N);
  
  f1 = function(x) exp(x);
  exact = exp(4) - exp(1);
  
  mean_est = mean(estimates);
  sd_est = sd(estimates);
  e_absoluta = abs(mean_est - exact);
  e_relativa = e_absoluta / exact;
  cat("Valoare estimata: ", mean_est, "\n");
  cat("Eroare absoluta: ", e_absoluta, "\n");
  cat("Eroare relativa: ", e_relativa, "\n");
}

#II.1 (d)
f2 = function(x) 1/(2*x-1)^2;
exact_area2 = log(3/4);

MC_integration2 = function(N) 
{
  sum = 0;
  for(i in 1:N) 
  {
    x = runif(1, 0, 1);
    sum = sum + f2(x)/(2*x-1)^2;
  }
  return(sum/N);
}

MC_integr_average2= function(k, N) 
{
  estimates=0 
  for(i in 1:k)
    estimates[i] = MC_integration2(N);
  
  mean_est = mean(estimates)
  sd_est = sd(estimates)
  e_absoluta = abs(mean_est - exact_area2)
  e_relativa = e_absoluta / exact_area2
  cat("Valoare estimata: ", mean_est, "\n")
  cat("Eroarea absoluta: ", e_absoluta, "\n")
  cat("Eroarea relativa: ", e_relativa, "\n")
}
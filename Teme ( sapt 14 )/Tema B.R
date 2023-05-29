#B1
B1 = function(valori_n, valori_p) 
{
  for (n in valori_n) 
  {
    for (p in valori_p) 
    {
      x=rgeom(n, p)
      media_rezultata=mean(x)
      media_reala=1/p
      cat("n=",n,"p=",p,"\n")
      cat("Media rezultata:", media_rezultata, "\n")
      cat("Media reala:", media_reala, "\n")
      cat("\n")
    }
  }
}

valori_n = c(5000, 10000, 100000, 500000)
valori_p = c(0.2,0.6, 0.6, 0.8)
B1(valori_n, valori_p)

#B2
B2=function(n, N, z, r) 
{
  media_reala=0
  variatia_reala=r/(r-2)
  sum=0
  for (i in 1:N) 
  {
    x_n=mean(rt(n, r))
    u=z*variatia_reala/sqrt(n)+media_reala
    if (x_n <= u) 
      sum=sum+1
  }
  return(sum/N)
}

valori_N=c(5000, 10000, 20000)
valori_z=c(-1.5, 0, 1.5)

for (N in valori_N)
  for (z in valori_z)
    cat("N=", N, "z=", z, "eroarea relativa:", B2(50, N, z, 3), "\n")

#B3
B3 = function(n, p, h, k) 
{
  np = n * p
  nq = n * (1 - p)

  mu = np
  sigma = sqrt(np * (1 - p))
  
  a = (h - mu + 0.5) / sigma
  b = (k - mu + 0.5) / sigma
  
  prob = pnorm(b) - pnorm(a)
  return(prob)
}

n = 100  
p = 0.5 

h = 20  
k = 40 

aproximare = B3(n, p, h, k)

cat("aproximarea P:", aproximare)
#D1 (a)
D1a= function(x, k) 
{
    n = length(x)
    for (i in 1:k) 
    {
      index = sample(1:n, 1)
      element = x[index]
      
      count = sum(x == element)
      if (count >= n/2 + 1) 
      {
        return(element)  
      }
    }
  
    return("x nu are M-element")  
}

x = c(2, 4, 4, 6, 4, 6, 4, 4)
k = 2
cat("M-element:", D1a(x, k), "\n")

#D1 (b)
k = 1
e = 10^(-7)
while (1/(2^k) > e) 
{
  k = k+1
}
cat("K:", k, "\n")

#D2
D2 = function(i, A) 
{
  n = length(A)
  z = sample(A, 1)
  Amic = A[A < z]
  Amare = A[A > z]
  
  if (length(Amic) > i) 
  {
    return(D2(i, Amic))
  } 
  else if (n > i + length(Amare)) 
  {
    return(z)
  } 
  else 
  {
    return(D2(i - n + length(Amare), Amare))
  }
}

A = c(1, 2, 3, 4, 5, 6)
i = 3

result = D2(i, A)
cat("al", i, "-lea nr:", result,"\n")

#D3 (a)
D3a = function(S, a) 
{
  n = length(S)
  m = floor(a * log(n))
  
  if (m > n) 
  {
    return("m > n")
  }
  
  S_prim = sample(S, m)
  sorted_S_prim = sort(S_prim)
  
  median_index = ceiling(m/2)
  mediana = sorted_S_prim[median_index]
  
  return(mediana)
}

S = c(5, 3, 1, 2, 4)
a = 2

result = D3a(S, a)
cat("median S':", result,"\n")

#D3 (b)
n = 1
while(1 - 2/n^2 <= 1 - 10^(-7))
{
  n = n + 1
}
cat("dimensiunea minima S:", n)
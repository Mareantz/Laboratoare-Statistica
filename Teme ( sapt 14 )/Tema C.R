#C1

mc = function(a, marime) 
{
  count = 0
  
  for (i in 1:marime) 
  {
    x1 = runif(1, -sqrt(a), sqrt(a))
    x2 = runif(1, -sqrt(a), sqrt(a))
    x3 = runif(1, 0, a)
    
    if (x3<=a*sqrt(1-(x1^2+x2^2)/(a^2))) 
    {
      count = count + 1
    }
  }
  
  volum_estimat = (count / marime) * (4 * a^2 * sqrt(a))
  return(volum_estimat)
}

a_values = c(2, 4, 10)
s = c(10000, 20000, 50000)

for (a in a_values) 
{
  cat("Valoarea exacta a volumului cu a =", a, "este:", (pi * a^2)/2, "\n")
  
  for (marime in s) 
  {
    volume_estimat = mc(a, marime)
    eroare_relativa = abs(volume_estimat - (pi * a^2)/2) / ((pi * a^2)/2)
    
    cat("Estimarea volumului cu a =", a, "si dimensiunea esantionului =", marime, "este:", volume_estimat, "\n")
    cat("Eroarea relativa cu a =", a, "si dimensiunea esantionului =", marime, "este:", eroare_relativa, "\n")
    cat("\n")
  }
}


#C2

C2 = function(n) 
{
  count = 0
  
  for (i in 1:n) 
  {
    x = runif(1, 0, 18)
    y = runif(1, 0, 6)
    
    if (x >= 0 && y >= 0 && 3*y <= x + 6 && y <= 12 - 3*x) 
    {
      count = count + 1
    }
  }
  
  area = count / n * (b - a) * (d - c)
  return(area)
}
n = 20000
area = C2(n)

cat("Aria lui T estimata pentru un esantion de dimensiune", n, "este:", area, "\n")


#C3 (a)

n = 20000
count = 0

for (i in 1:n) 
{
  x = runif(1, -1, 1)
  y = (x + 1) / sqrt(4 - x^2)
  count = count + y
}

valoare_estimata = count / n * 2

valoare_exacta = pi / 3
eroare_relativa = abs(valoare_estimata - valoare_exacta) / valoare_exacta * 100

cat("Valoare estimata:", valoare_estimata, "\n")
cat("Valoare exacta:", valoare_exacta, "\n")
cat("Eroare relativa:", eroare_relativa, "%")


#C3 (b)

n = 20000
count = 0

for (i in 1:n) 
{
  x = runif(1, -1, 1)
  y = 1/(x^2+4)
  count = count + y
}

valoare_estimata = count / n 

valoare_exacta = pi / 4
eroare_relativa = abs(valoare_estimata - valoare_exacta) / valoare_exacta * 100

cat("Valoare estimata:", valoare_estimata, "\n")
cat("Valoare exacta:", valoare_exacta, "\n")
cat("Eroare relativa:", eroare_relativa, "%")


#C3 (c)

n = 20000
count = 0

for (i in 1:n) 
{
  x = runif(1, 0, 1)
  y = x*exp(x)
  count = count + y
}

valoare_estimata = count / n 

valoare_exacta = -1
eroare_relativa = abs(valoare_estimata - valoare_exacta) / valoare_exacta * 100

cat("Valoare estimata:", valoare_estimata, "\n")
cat("Valoare exacta:", valoare_exacta, "\n")
cat("Eroare relativa:", eroare_relativa, "%")


#C4 (a)

m = 100000
n = 500
p = 0.5
q = 0.1

simulari = 1000
total_zile = 0

for (i in 1:simulari) 
{
  fake_accounts = m
  zile=0
  
  while (fake_accounts > 0) 
  {
    new_fake_accounts = rbinom(1, n, p)
    deactivated_accounts = rbinom(new_fake_accounts, 1, q)
    fake_accounts = fake_accounts + new_fake_accounts - sum(deactivated_accounts)
    days = days + 1
  }
}

average_days = zile / simulari

cat("Numarul mediu de zile:", average_days, "\n")

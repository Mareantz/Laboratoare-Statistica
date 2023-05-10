f=function(x) {-2*x^2 + 5*x - 2}

a=1/2
b=2
n=10000

x=seq(a, b, length.out=n+1)
y=f(x)
estimat=sum(diff(x)*(y[-1]+y[-n])/2)
cat("Zona estimata:", estimat, "\n")

integrala=function(x) {-2*x^2 + 5*x - 2}
exact=integrate(integrala, a, b)$value
cat("Zona exacta:", exact, "\n")

relativ=abs(estimat - exact) / exact
cat("Eroare relativa:", relativ, "\n")
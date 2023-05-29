#E1

media = 138
deviatia = 11
n = 10
nivel_incredere = c(0.9, 0.95, 0.99)

z_values = qnorm((1 + nivel_incredere) / 2)
intervale_incredere = matrix(NA, nrow = length(nivel_incredere), ncol = 2)

for (i in 1:length(nivel_incredere)) 
{
  interval_incredere = c(media - z_values[i] * (deviatia / sqrt(n)), media + z_values[i] * (deviatia / sqrt(n)))
  intervale_incredere[i, ] = interval_incredere
}
for (i in 1:length(nivel_incredere)) 
{
  cat("Intervalul de incredere", nivel_incredere[i] * 100,"%: (", round(intervale_incredere[i, 1], 2), ",", round(intervale_incredere[i, 2], 2), ")\n")
}


#E2

media = 18
dispersia = 1.44
esantion = 256
nivel_incredere = 0.95

z_value = qnorm((1 + nivel_incredere) / 2) 
interval_incredere = c(media_aleasa - z_value * sqrt(dispersia_aleasa) / sqrt(esantion), media_aleasa + z_value * sqrt(dispersia_aleasa) / sqrt(esantion))

cat("Intervalul de încredere de 95%: (", interval_incredere[1], ",", interval_incredere[2], ")\n")


#E3

n = 153
x = 17
p = x/n
nivel = c(0.01, 0.05)
p_initial = 0.12

q = qbinom(nivel, n, p_initial, lower.tail = TRUE)

if (p <= p_initial) 
{
  cat("Concluzie: Schimbarea a fost eficienta.\n")
} else 
{
  cat("Concluzie: Schimbarea nu a fost eficienta.\n")
}

cat("Valorile critice (q) pentru nivelurile de semnificatie 1% respectiv 5% sunt:", q, "\n")
graf = function(lambda, n) {
  d = dpois(0:n, lambda)
  barplot(d, main = "Densitatea Poisson", xlab = "Valori", ylab = "Probabilitate",
          col = "green", ylim = c(0, max(d)))
}
graf = function(p, n) {
  d = dgeom(0:n, p)
  barplot(d, main = "Densitatea Geometrica", xlab = "Valori", ylab = "Probabilitate",
          col = "red", ylim = c(0, max(d)))
}
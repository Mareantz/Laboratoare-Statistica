plot_normal_pdf <- function(mu, sigmap, delta, a) {
  x = seq(-a, a, delta)
  y = dnorm(x, mu, sigmap)
  plot(x, y, type = "l");
}
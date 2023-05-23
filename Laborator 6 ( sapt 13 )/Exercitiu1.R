#II.6
zconfidence_interval2 = function(alfa, file, sigma)
{
  x = scan(file)
  n = length(x)
  sample_mean = mean(x)
  critical_z = qnorm(1 - alfa/2, 0, 1)
  a = sample_mean - critical_z*sigma/sqrt(n)
  b = sample_mean + critical_z*sigma/sqrt(n)
  interval = c(a, b)
  return(interval)
}
zconfidence_interval2(0.05, "history.txt", 5)
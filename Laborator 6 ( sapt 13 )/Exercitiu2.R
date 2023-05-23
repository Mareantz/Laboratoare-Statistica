#III.4
t_conf_interval2 = function(alfa, file)
{
  x = scan(file)
  n = length(x)
  sample_mean = mean(x)
  s = sd(x)
  se = s/sqrt(n)
  critical_t = qt(1 - alfa/2, n - 1)
  a = sample_mean - critical_t*se
  b = sample_mean + critical_t*se
  interval = c(a, b)
  return(interval)
}
t_conf_interval2(0.1, "3.5.txt")
t_conf_interval2(0.05, "3.5.txt")
t_conf_interval2(0.01, "3.5.txt")
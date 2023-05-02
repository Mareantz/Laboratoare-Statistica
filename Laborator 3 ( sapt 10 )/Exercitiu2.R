LLN_student <- function(r, n) {
  X <- rt(n, df = r)
  sample_mean <- mean(X)
  cat("Sample mean:", sample_mean, "\n")
  cat("Expected value:", 0, "\n")
}
LLN_student(r = 2, n = 1000)
LLN_student(r = 3, n = 10000)
LLN_student(r = 4, n = 100000)
LLN_student(r = 5, n = 1000000)
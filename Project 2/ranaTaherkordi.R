g <- function(x) {
  exp(-x) * cos(x)^2
}

set.seed(123)
u <- runif(10000)
x1 <- (1 / u) - 1
Ihat_transform <- mean(g(x1) / (u^2))
cat("روش تغییر متغیر: ", Ihat_transform, "\n\n")

set.seed(123)
x2 <- rexp(10000)
Ihat_exp <- mean(cos(x2)^2)
cat("روش امید ریاضی Exp(1): ", Ihat_exp, "\n\n")

N_values <- c(10, 100, 1000, 10000)
true_value <- 0.6

cat("روش مونت کارلو برای مقادیر مختلف N:\n")
for (N in N_values) {
  x_mc <- rexp(N)
  estimate_mc <- mean(cos(x_mc)^2)
  abs_error_mc <- abs(estimate_mc - true_value)
  cat("N =", N, "- Estimate:", estimate_mc, "- Absolute Error:", abs_error_mc, "\n")
}
cat("\n")

g2 <- function(x) cos(x)^2
k <- 2
theta <- 1

cat("روش Importance Sampling با توزیع گاما:\n")
for (N in N_values) {
  y <- rgamma(N, shape = k, scale = theta)
  weights <- dexp(y) / dgamma(y, shape = k, scale = theta)
  estimate_is <- mean(weights * g2(y))
  abs_error_is <- abs(estimate_is - true_value)
  cat("N =", N, "- Estimate:", estimate_is, "- Absolute Error:", abs_error_is, "\n")
}


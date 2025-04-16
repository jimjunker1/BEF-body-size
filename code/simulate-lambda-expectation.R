library(stats4)

# Simulate truncated Pareto data, valid for any alpha ≠ 0
rtrunc_pareto_general <- function(n, alpha, x_min, x_max) {
  u <- runif(n)
  CDF_min <- 1 - (x_min / x_min)^alpha  # always 0
  CDF_max <- 1 - (x_min / x_max)^alpha
  x <- x_min / (1 - u * (CDF_max - CDF_min))^(1 / alpha)
  return(x)
}

# Example parameters:
set.seed(1312)
alpha_true <- -1.5  # negative alpha
x_min <- 1
x_max <- 1000
n_samples <- 1e4

# Generate data:
data <- rtrunc_pareto_general(n_samples, alpha_true, x_min, x_max)

# General negative log-likelihood (valid for any alpha ≠ 0):
neg_log_likelihood <- function(alpha) {
  if(alpha == 0) return(Inf) # alpha = 0 undefined
  n <- length(data)
  if(any(data < x_min) || any(data > x_max)) return(Inf)
  term1 <- n * log(abs(alpha))
  term2 <- n * alpha * log(x_min)
  term3 <- -(alpha + 1) * sum(log(data))
  term4 <- -n * log(abs(1 - (x_min / x_max)^alpha))
  return(-(term1 + term2 + term3 + term4))
}

# Estimate alpha numerically:
fit <- mle(minuslogl = neg_log_likelihood,
           start = list(alpha = -1),
           method = "L-BFGS-B",
           lower = -10, upper = 10)
alpha_hat <- coef(fit)["alpha"]
cat("Estimated alpha:", alpha_hat, "\n")

# Expectation for truncated Pareto, generalized (valid for any alpha ≠ 1):
trunc_pareto_expectation_general <- function(alpha, a, b) {
  if(alpha == 1) {
    return((a * b * log(b / a)) / (b - a))
  } else {
    numerator <- alpha * a^alpha * (b^(1 - alpha) - a^(1 - alpha))
    denominator <- (1 - alpha) * ((a / b)^alpha - 1)
    return(numerator / denominator)
  }
}

# Compute expectation at new truncation lower bound:
x_bot <- 0.006
expected_value <- trunc_pareto_expectation_general(alpha_hat, x_bot, x_max)
cat("Predicted expectation at x_bot =", x_bot, ":", expected_value, "\n")

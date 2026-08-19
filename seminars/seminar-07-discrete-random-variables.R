# Workshop 7 — Discrete Random Variables
# Presentation coverage: PMF, CDF, expectation, variance, and sums of independent random variables.
# Base R only.

options(digits = 10)

# PMF from the presentation example.
x <- 0:4
p_x <- c(0.20, 0.20, 0.40, 0.10, 0.10)
stopifnot(all(p_x >= 0), abs(sum(p_x) - 1) < 1e-12)

f_x <- cumsum(p_x)
mu_x <- sum(x * p_x)
var_x <- sum(x^2 * p_x) - mu_x^2

# The CDF is a step function for discrete X.
cdf_at_2 <- sum(p_x[x <= 2])
p_1_to_3 <- sum(p_x[x >= 1 & x <= 3])

# Independent random variables: distribution of X + Y by convolution.
x_values <- 0:2
y_values <- 0:2
p_x_books <- c(0.50, 0.25, 0.25)
p_y_books <- c(0.25, 0.50, 0.25)
p_sum <- convolve(p_x_books, rev(p_y_books), type = "open")
sum_values <- 0:4

stopifnot(abs(sum(p_sum) - 1) < 1e-12,
          abs(p_sum[1] - 0.125) < 1e-12,
          abs(p_sum[2] - 0.3125) < 1e-12)

print(data.frame(x = x, pmf = p_x, cdf = f_x))
print(c(E_X = mu_x, Var_X = var_x, F_at_2 = cdf_at_2, P_1_to_3 = p_1_to_3))
print(data.frame(x_plus_y = sum_values, pmf = p_sum))

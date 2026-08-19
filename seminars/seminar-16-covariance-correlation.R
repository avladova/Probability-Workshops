# Workshop 16 — Covariance & Correlation
# Presentation exercises implemented: Questions 1–7.
# Base R only.

options(digits = 10)

# Question 1: variance of X + Y with equal geometric variances and correlation 0.5.
p_q1 <- 1 / 4
var_q1 <- (1 - p_q1) / p_q1^2
var_sum_q1 <- var_q1 + var_q1 - 2 * 0.5 * sqrt(var_q1 * var_q1)

# Question 2: E[(X + Y)^2] from means, variances, and correlation.
e_sum_squared <- function(e_x, e_y, var_x, var_y, rho) {
  var_x + var_y + 2 * rho * sqrt(var_x * var_y) + (e_x + e_y)^2
}
e_sum_squared_q2 <- e_sum_squared(40, 70, 40, 70, 0.8)

# Question 3: variance of 2X - 3Y for equal Bernoulli variances and correlation 0.7.
var_bernoulli_q3 <- 0.5 - 0.5^2
var_linear_q3 <- 2^2 * var_bernoulli_q3 + 3^2 * var_bernoulli_q3 -
  2 * 2 * 3 * 0.7 * sqrt(var_bernoulli_q3 * var_bernoulli_q3)

# Question 4: recover covariance from correlation and standard deviations.
covariance_q4 <- 0.25 * sqrt(8 * 32)

# Question 5: correlation from variances of S, T, and S + T.
var_s <- 3^2 + 3^2 + 2^2 + 1 + 1
var_t <- 9^2 + 3^2 + 2^2 + 2^2 + 1 + 1
var_s_plus_t <- 12^2 + 6^2 + 4^2 + 2^2 + 2^2 + 1
covariance_q5 <- (var_s_plus_t - var_s - var_t) / 2
correlation_q5 <- covariance_q5 / sqrt(var_s * var_t)

# Question 6: use the helper from Question 2.
e_sum_squared_q6 <- e_sum_squared(8, 8, 80, 80, 0.1)

# Question 7: form a correlation matrix from pairwise correlations.
r_xy <- (68 - 52) / (2 * sqrt(100))
r_xz <- (64 - 50.2) / (2 * sqrt(2 * 50))
r_yz <- (196 - 100) / (2 * sqrt(50 * 50))
correlation_matrix_q7 <- matrix(
  c(1, r_xy, r_xz,
    r_xy, 1, r_yz,
    r_xz, r_yz, 1),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(c("X", "Y", "Z"), c("X", "Y", "Z"))
)

stopifnot(all(diag(correlation_matrix_q7) == 1),
          isTRUE(all.equal(correlation_matrix_q7, t(correlation_matrix_q7))))

print(c(
  question_1 = var_sum_q1,
  question_2 = e_sum_squared_q2,
  question_3 = var_linear_q3,
  question_4_covariance = covariance_q4,
  question_5_correlation = correlation_q5,
  question_6 = e_sum_squared_q6
))
print(correlation_matrix_q7)

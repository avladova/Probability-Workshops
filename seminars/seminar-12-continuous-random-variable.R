# Workshop 12 — Continuous Random Variable
# Presentation exercises implemented: Questions 1–3.
# Base R only: numerical integration is used instead of external symbolic packages.

options(digits = 10)

# Question 1: f(x) = 32 / x^3 for x >= 4.
f1 <- function(x) ifelse(x >= 4, 32 / x^3, 0)
p_4_to_5_q1 <- integrate(f1, lower = 4, upper = 5)$value
stopifnot(abs(integrate(f1, lower = 4, upper = Inf)$value - 1) < 1e-10)

# Question 2: f(x) = x^2 / 18 on [-3, 3].
f2 <- function(x) ifelse(x >= -3 & x <= 3, x^2 / 18, 0)
p_minus_1_5_to_1_5_q2 <- integrate(f2, lower = -1.5, upper = 1.5)$value
stopifnot(abs(integrate(f2, lower = -3, upper = 3)$value - 1) < 1e-10,
          abs(p_minus_1_5_to_1_5_q2 - 0.125) < 1e-10)

# Question 3: f(x) = C x^8 on [-4, 5].
c_q3 <- 9 / (5^9 - (-4)^9)
f3 <- function(x) ifelse(x >= -4 & x <= 5, c_q3 * x^8, 0)
p_minus_3_to_3_q3 <- integrate(f3, lower = -3, upper = 3)$value
mean_q3 <- integrate(function(x) x * f3(x), lower = -4, upper = 5)$value
second_moment_q3 <- integrate(function(x) x^2 * f3(x), lower = -4, upper = 5)$value
variance_q3 <- second_moment_q3 - mean_q3^2
stopifnot(abs(integrate(f3, lower = -4, upper = 5)$value - 1) < 1e-10,
          variance_q3 >= 0)

print(c(
  question_1_P_4_to_5 = p_4_to_5_q1,
  question_2_P_minus_1_5_to_1_5 = p_minus_1_5_to_1_5_q2,
  question_3_C = c_q3,
  question_3_P_minus_3_to_3 = p_minus_3_to_3_q3,
  question_3_mean = mean_q3,
  question_3_variance = variance_q3
))

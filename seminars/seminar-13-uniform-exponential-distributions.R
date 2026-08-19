# Workshop 13 — The Uniform & Exponential Distributions
# Presentation exercises implemented: Questions 1–4.
# Base R only.

options(digits = 10)

# Question 1: f(x) = 1 / x^3 for x >= 1.
f1 <- function(x) ifelse(x >= 1, 1 / x^3, 0)
p_2_to_5_q1 <- integrate(f1, lower = 2, upper = 5)$value
stopifnot(abs(p_2_to_5_q1 - 0.105) < 1e-10)

# Question 2: CDF values on two intervals.
F2 <- function(x) {
  ifelse(x <= 0, 0,
         ifelse(x <= 1, x^2 / 2,
                ifelse(x <= 2, x / 2, 1)))
}
p_0_to_0_5_q2 <- F2(0.5) - F2(0)
p_0_5_to_1_5_q2 <- F2(1.5) - F2(0.5)
p_1_to_2_q2 <- F2(2) - F2(1)
stopifnot(abs(p_0_to_0_5_q2 - 1 / 8) < 1e-12,
          abs(p_0_5_to_1_5_q2 - 5 / 8) < 1e-12,
          abs(p_1_to_2_q2 - 1 / 2) < 1e-12)

# Question 3: f(x) = c(2 - 2x^2) on (0, 1).
c_q3 <- 3 / 4
f3 <- function(x) ifelse(x > 0 & x < 1, c_q3 * (2 - 2 * x^2), 0)
F3 <- function(x) {
  ifelse(x <= 0, 0,
         ifelse(x < 1, 1.5 * x - 0.5 * x^3, 1))
}
p_0_5_to_0_7_q3 <- F3(0.7) - F3(0.5)
mean_q3 <- integrate(function(x) x * f3(x), lower = 0, upper = 1)$value
stopifnot(abs(integrate(f3, lower = 0, upper = 1)$value - 1) < 1e-10,
          abs(p_0_5_to_0_7_q3 - 0.191) < 1e-10,
          abs(mean_q3 - 3 / 8) < 1e-10)

# Question 4: X ~ Uniform(-1, 1), calculate E(|X|^(2/7)).
# The absolute value is required for a real-valued fractional power on [-1, 1].
mean_x_to_two_sevenths_q4 <- integrate(
  function(x) abs(x)^(2 / 7) * dunif(x, min = -1, max = 1),
  lower = -1, upper = 1
)$value
stopifnot(abs(mean_x_to_two_sevenths_q4 - 7 / 9) < 1e-10)

print(c(
  question_1 = p_2_to_5_q1,
  question_2_a = p_0_to_0_5_q2,
  question_2_b = p_0_5_to_1_5_q2,
  question_2_c = p_1_to_2_q2,
  question_3_C = c_q3,
  question_3_probability = p_0_5_to_0_7_q3,
  question_3_mean = mean_q3,
  question_4 = mean_x_to_two_sevenths_q4
))

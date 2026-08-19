# Workshop 14 — Normal Distribution & Sum of Independent Random Variables
# Presentation exercises implemented: Questions 1–9.
# Base R only.

options(digits = 10)

# Question 1: standard-normal one-sided probabilities.
q1 <- c(
  P_Z_less_1_20 = pnorm(1.20),
  P_Z_greater_1_33 = pnorm(1.33, lower.tail = FALSE),
  P_Z_less_minus_1_70 = pnorm(-1.70),
  P_Z_greater_minus_1_00 = pnorm(-1.00, lower.tail = FALSE)
)

# Question 2: standard-normal interval probabilities.
q2 <- c(
  P_1_20_to_1_33 = pnorm(1.33) - pnorm(1.20),
  P_minus_1_70_to_1_20 = pnorm(1.20) - pnorm(-1.70),
  P_minus_1_70_to_minus_1_00 = pnorm(-1.00) - pnorm(-1.70)
)

# Question 3: standard-normal quantiles.
q3 <- c(
  P_Z_less_than_z_is_0_70 = qnorm(0.70),
  P_Z_less_than_z_is_0_25 = qnorm(0.25),
  P_Z_greater_than_z_is_0_20 = qnorm(0.80),
  P_Z_greater_than_z_is_0_60 = qnorm(0.40)
)

# Questions 4–6: normal random variables with given mean and variance.
q4 <- pnorm(16.5, mean = 12, sd = 3) - pnorm(6.9, mean = 12, sd = 3)
q5 <- pnorm(19.4, mean = 15, sd = 4, lower.tail = FALSE)
q6 <- pnorm(21.0, mean = 18, sd = 5)

# Question 7: solve for variance using the stated normal probability.
# pnorm(30, mean = 37.7, sd = 11) = 0.24196.
variance_q7 <- 11^2

# Question 8: solve for the mean when variance is 81 and P(X < 37) = 0.97128.
mean_q8 <- 37 - qnorm(0.97128) * 9

# Question 9: X - Y is normal when X and Y are independent normal variables.
mean_difference_q9 <- 11 - 20.1
sd_difference_q9 <- sqrt(6 + 43)
p_x_less_y_plus_7_q9 <- pnorm(7, mean = mean_difference_q9, sd = sd_difference_q9)

stopifnot(
  abs(q1[1] - 0.88493033) < 1e-8,
  abs(q2[1] - 0.023310535) < 1e-8,
  abs(q4 - 0.888627336) < 1e-8,
  abs(q5 - 0.135666061) < 1e-8,
  abs(q6 - 0.725746882) < 1e-8,
  abs(mean_q8 - 19.9) < 0.05,
  abs(p_x_less_y_plus_7_q9 - 0.9893) < 0.001
)

print(q1)
print(q2)
print(q3)
print(c(question_4 = q4, question_5 = q5, question_6 = q6,
        question_7_variance = variance_q7, question_8_mean = mean_q8,
        question_9 = p_x_less_y_plus_7_q9))

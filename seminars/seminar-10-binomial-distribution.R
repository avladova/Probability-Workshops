# Workshop 10 — Binomial Distribution
# Presentation exercises implemented: Questions 1–3 and selected binomial calculations.
# Base R only.

options(digits = 10)

# Question 1: independent Bernoulli X and Y.
x <- 0:1
y <- 0:1
p_x <- c(0.10, 0.90)
p_y <- c(0.90, 0.10)
e_x <- sum(x * p_x)
e_y <- sum(y * p_y)
e_x_plus_y_squared_q1 <- sum(x^2 * p_x) + sum(y^2 * p_y) + 2 * e_x * e_y
stopifnot(abs(e_x_plus_y_squared_q1 - 1.18) < 1e-12)

# Question 2: 60 independent Bernoulli variables with success probability 0.1.
n_q2 <- 60
p_q2 <- 0.10
# E(S^2) = Var(S) + [E(S)]^2 for S ~ Bin(n, p).
e_sum_squared_q2 <- n_q2 * p_q2 * (1 - p_q2) + (n_q2 * p_q2)^2
stopifnot(abs(e_sum_squared_q2 - 41.4) < 1e-12)

# Question 3: 3 independent Bernoulli variables with P(X_i = 1) = 0.6.
n_q3 <- 3
p_q3 <- 0.60
# E[4^S] for S ~ Bin(n, p) is (1 - p + 4p)^n.
e_four_power_sum_q3 <- (1 - p_q3 + 4 * p_q3)^n_q3
stopifnot(abs(e_four_power_sum_q3 - 21.952) < 1e-12)

# Binomial PMF/CDF examples.
n <- 20
p <- 1 / 6
x_values <- 0:n
pmf <- dbinom(x_values, size = n, prob = p)
cdf_at_5 <- pbinom(5, size = n, prob = p)
p_at_least_4 <- pbinom(3, size = n, prob = p, lower.tail = FALSE)

stopifnot(abs(sum(pmf) - 1) < 1e-12,
          cdf_at_5 >= 0, cdf_at_5 <= 1,
          p_at_least_4 >= 0, p_at_least_4 <= 1)

print(c(
  question_1 = e_x_plus_y_squared_q1,
  question_2 = e_sum_squared_q2,
  question_3 = e_four_power_sum_q3,
  P_X_at_most_5 = cdf_at_5,
  P_X_at_least_4 = p_at_least_4
))

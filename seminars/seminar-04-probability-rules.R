# Workshop 4 — Probability: Rules of Probability
# Presentation exercises implemented: Tasks 12–14.
# Base R only.

options(digits = 10)

# Task 12: probability mass function for X = 3, 4, 5, 6, 7.
x12 <- c(3, 4, 5, 6, 7)
p12 <- c(0.08, 0.24, 0.41, 0.20, 0.07)
stopifnot(abs(sum(p12) - 1) < 1e-12)

p_a12 <- sum(p12[x12 > 4])
p_not_a12 <- 1 - p_a12
p_b12 <- sum(p12[x12 < 6])
p_a_and_b12 <- sum(p12[x12 > 4 & x12 < 6])
p_a_or_b12 <- sum(p12[x12 > 4 | x12 < 6])

# Task 13: interval events represented by a discrete PMF.
x13 <- c(-Inf, -10, 0, 10, 20)
p13 <- c(0.04, 0.14, 0.28, 0.33, 0.21)
stopifnot(abs(sum(p13) - 1) < 1e-12)

p_a13 <- sum(p13[x13 >= 10])
p_b13 <- sum(p13[x13 < 0])
p_not_a13 <- 1 - p_a13
p_a_and_b13 <- sum(p13[x13 >= 10 & x13 < 0])
p_a_or_b13 <- sum(p13[x13 >= 10 | x13 < 0])

# Task 14: elementary equally likely outcomes.
p_a14 <- 4 / 8
p_b14 <- 2 / 8
p_a_and_b14 <- 2 / 8

print(list(
  task_12 = c(A = p_a12, not_A = p_not_a12, B = p_b12,
              A_and_B = p_a_and_b12, A_or_B = p_a_or_b12),
  task_13 = c(A = p_a13, B = p_b13, not_A = p_not_a13,
              A_and_B = p_a_and_b13, A_or_B = p_a_or_b13),
  task_14 = c(A = p_a14, B = p_b14, A_and_B = p_a_and_b14)
))

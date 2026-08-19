# Workshop 11 — PMF & CMF
# Presentation exercises implemented: Questions 1–3.
# Base R only.

options(digits = 10)

pmf_table <- function(x, p) {
  stopifnot(length(x) == length(p), all(p >= 0), abs(sum(p) - 1) < 1e-12)
  data.frame(x = x, pmf = p, cmf = cumsum(p))
}

# Question 1: number of warranty returns.
x1 <- 0:4
p1 <- c(0.28, 0.36, 0.23, 0.09, 0.04)
q1 <- pmf_table(x1, p1)

# Question 2: number of furnace orders.
x2 <- 0:5
p2 <- c(0.10, 0.14, 0.26, 0.28, 0.15, 0.07)
q2 <- pmf_table(x2, p2)
p_at_least_three_q2 <- sum(p2[x2 >= 3])
stopifnot(abs(p_at_least_three_q2 - 0.50) < 1e-12)

# Question 3: number of paper clips per package.
x3 <- 47:53
p3 <- c(0.04, 0.13, 0.21, 0.29, 0.20, 0.10, 0.03)
q3 <- pmf_table(x3, p3)
p_49_to_51_q3 <- sum(p3[x3 >= 49 & x3 <= 51])
p_at_least_one_ge_50_q3 <- 1 - (sum(p3[x3 < 50]))^2
stopifnot(abs(p_49_to_51_q3 - 0.70) < 1e-12,
          abs(p_at_least_one_ge_50_q3 - 0.8556) < 1e-12)

print(q1)
print(q2)
print(c(question_2_P_at_least_3 = p_at_least_three_q2))
print(q3)
print(c(question_3_P_49_to_51 = p_49_to_51_q3,
        question_3_P_at_least_one_ge_50 = p_at_least_one_ge_50_q3))

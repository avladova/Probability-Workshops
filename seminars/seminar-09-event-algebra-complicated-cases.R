# Workshop 9 — Event Algebra: Complicated Cases
# Presentation examples implemented: Questions 1, 2, and 7.
# Base R only.

options(digits = 10)

# Question 1: probability that at least one of three independent events occurs.
p_a <- 0.80
p_b <- 0.70
p_c <- 0.60
p_at_least_one_q1 <- 1 - (1 - p_a) * (1 - p_b) * (1 - p_c)

# Same calculation by inclusion–exclusion for independent events.
p_inclusion_exclusion_q1 <- p_a + p_b + p_c -
  p_a * p_b - p_a * p_c - p_b * p_c + p_a * p_b * p_c
stopifnot(abs(p_at_least_one_q1 - p_inclusion_exclusion_q1) < 1e-12)

# Question 2: P((A and B) or C) for independent events.
p_a <- 0.80
p_b <- 0.70
p_c <- 0.60
p_a_and_b <- p_a * p_b
p_a_and_b_or_c_q2 <- p_a_and_b + p_c - p_a_and_b * p_c

# Question 7: a conditional probability expressed through an intersection and a union.
p_a <- 0.80
p_b <- 0.70
p_c <- 0.60
p_numerator_q7 <- p_a * p_b * p_c
p_denominator_q7 <- p_b + p_c - p_b * p_c
p_conditional_q7 <- p_numerator_q7 / p_denominator_q7

stopifnot(all(c(p_at_least_one_q1, p_a_and_b_or_c_q2, p_conditional_q7) >= 0),
          all(c(p_at_least_one_q1, p_a_and_b_or_c_q2, p_conditional_q7) <= 1))

print(c(
  question_1 = p_at_least_one_q1,
  question_2 = p_a_and_b_or_c_q2,
  question_7 = p_conditional_q7
))

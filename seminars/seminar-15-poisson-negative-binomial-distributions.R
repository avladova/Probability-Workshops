# Workshop 15 — Poisson & Negative Binomial Distributions
# Presentation exercises implemented: Questions 8–13.
# Base R only.

options(digits = 10)

# Question 8: arrivals at a rate of 3 per minute.
p_at_most_two_arrivals_q8 <- ppois(q = 2, lambda = 3)
stopifnot(abs(p_at_most_two_arrivals_q8 - 0.423190081) < 1e-8)

# Question 9: accidents at an average rate of 4.9 per month.
lambda_q9 <- 4.9
p_fewer_than_two_accidents_q9 <- ppois(q = 1, lambda = lambda_q9)
p_more_than_three_accidents_q9 <- ppois(q = 3, lambda = lambda_q9, lower.tail = FALSE)

# Question 10: highway breakdowns at an average rate of 3.2 per day.
lambda_q10 <- 3.2
p_fewer_than_two_breakdowns_q10 <- ppois(q = 1, lambda = lambda_q10)
p_more_than_four_breakdowns_q10 <- ppois(q = 4, lambda = lambda_q10, lower.tail = FALSE)
stopifnot(abs(p_fewer_than_two_breakdowns_q10 - 0.171) < 0.001,
          abs(p_more_than_four_breakdowns_q10 - 0.219) < 0.001)

# Question 11: Poisson approximation to Bin(100, 0.055).
lambda_q11 <- 100 * 0.055
p_fewer_than_three_errors_q11 <- ppois(q = 2, lambda = lambda_q11)
stopifnot(abs(p_fewer_than_three_errors_q11 - 0.0884) < 0.001)

# Question 12: probability that exactly 3 sixes occur before the third non-six.
# R's negative-binomial variable counts successes before the specified number of failures.
p_three_sixes_before_third_non_six_q12 <- dnbinom(x = 3, size = 3, prob = 5 / 6)

# Question 13: illustrative negative-binomial waiting-time calculation.
# Number of failures before the fourth success when p(success) = 0.40.
p_four_failures_before_fourth_success_q13 <- dnbinom(x = 4, size = 4, prob = 0.40)

stopifnot(all(c(
  p_at_most_two_arrivals_q8,
  p_fewer_than_two_accidents_q9,
  p_more_than_three_accidents_q9,
  p_fewer_than_two_breakdowns_q10,
  p_more_than_four_breakdowns_q10,
  p_fewer_than_three_errors_q11,
  p_three_sixes_before_third_non_six_q12,
  p_four_failures_before_fourth_success_q13
) >= 0), all(c(
  p_at_most_two_arrivals_q8,
  p_fewer_than_two_accidents_q9,
  p_more_than_three_accidents_q9,
  p_fewer_than_two_breakdowns_q10,
  p_more_than_four_breakdowns_q10,
  p_fewer_than_three_errors_q11,
  p_three_sixes_before_third_non_six_q12,
  p_four_failures_before_fourth_success_q13
) <= 1))

print(c(
  question_8 = p_at_most_two_arrivals_q8,
  question_9_a = p_fewer_than_two_accidents_q9,
  question_9_b = p_more_than_three_accidents_q9,
  question_10_a = p_fewer_than_two_breakdowns_q10,
  question_10_b = p_more_than_four_breakdowns_q10,
  question_11 = p_fewer_than_three_errors_q11,
  question_12 = p_three_sixes_before_third_non_six_q12,
  question_13 = p_four_failures_before_fourth_success_q13
))

# Workshop 6 — Conditional Probability
# Presentation exercises implemented: Questions 7–14.
# Base R only.

options(digits = 10)

# Question 7: conditional probability after an observed group membership.
p_q7 <- 21 / 60
p_q7_conditional <- 15 / 21

# Question 8: Bayes' theorem for three mutually exclusive sources.
prior_q8 <- c(0.30, 0.50, 0.20)
likelihood_q8 <- c(0.03, 0.02, 0.10)
p_observed_q8 <- sum(prior_q8 * likelihood_q8)
p_source_1_given_observed_q8 <- prior_q8[1] * likelihood_q8[1] / p_observed_q8

# Question 9: total probability and posterior calculations.
p_q9 <- 0.7 * 0.25 + 0.5 * 0.5 + 0.2 * 0.25
p_first_given_event_q9 <- 0.25 * 0.7 / p_q9
p_complement_q9 <- 1 - 0.25 * 0.3 / (1 - 0.475)

# Questions 10–11: conditional probability and total probability.
p_q10 <- (0.4 * 0.9) / (0.4 * 0.9 + 0.6 * 1.0)
p_q11 <- 0.08 * 0.8 + 0.92 * 0.2

# Question 14: total probability and Bayes' theorem.
prior_q14 <- c(0.30, 0.50, 0.20)
likelihood_q14 <- c(0.20, 0.60, 0.80)
p_event_q14 <- sum(prior_q14 * likelihood_q14)
p_first_given_event_q14 <- prior_q14[1] * likelihood_q14[1] / p_event_q14
p_not_first_given_event_q14 <- 1 - p_first_given_event_q14

stopifnot(
  all(c(p_q7, p_q7_conditional, p_source_1_given_observed_q8, p_first_given_event_q9,
        p_complement_q9, p_q10, p_q11, p_event_q14, p_first_given_event_q14,
        p_not_first_given_event_q14) >= 0),
  all(c(p_q7, p_q7_conditional, p_source_1_given_observed_q8, p_first_given_event_q9,
        p_complement_q9, p_q10, p_q11, p_event_q14, p_first_given_event_q14,
        p_not_first_given_event_q14) <= 1)
)

print(list(
  question_7 = c(P_A = p_q7, conditional_probability = p_q7_conditional),
  question_8 = c(P_observed = p_observed_q8, posterior = p_source_1_given_observed_q8),
  question_9 = c(P_event = p_q9, posterior = p_first_given_event_q9,
                 complement = p_complement_q9),
  question_10 = p_q10,
  question_11 = p_q11,
  question_14 = c(P_event = p_event_q14, posterior = p_first_given_event_q14,
                  complement = p_not_first_given_event_q14)
))

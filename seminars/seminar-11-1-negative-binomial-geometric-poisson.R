# Workshop 11.1 — Negative Binomial, Geometric, Poisson Distributions
# Presentation coverage: geometric waiting times, negative-binomial waiting times,
# Poisson probabilities, and a supplementary hypergeometric calculation.
# Base R only.

options(digits = 10)

# Supplementary hypergeometric example: 8 selected from 16, with 8 successes in population.
p_hypergeometric <- dhyper(x = 4, m = 8, n = 8, k = 8)

# Task 3: first success after 19 failures, with success probability 0.07.
p_success <- 0.07
p_19_failures_then_success <- dgeom(x = 19, prob = p_success)
mean_trial_of_first_success <- 1 / p_success

# Task 4: geometric and negative-binomial parameterisations agree when size = 1.
p_task_4_exactly_4_failures <- dgeom(x = 4, prob = 0.03)
p_task_4_at_most_4_failures <- pgeom(q = 4, prob = 0.03)
p_task_4_negbinom_check <- dnbinom(x = 4, size = 1, prob = 0.03)
stopifnot(abs(p_task_4_exactly_4_failures - p_task_4_negbinom_check) < 1e-12)

# Negative-binomial example: number of failures before the third success.
r <- 3
p <- 0.25
p_five_failures_before_third_success <- dnbinom(x = 5, size = r, prob = p)
p_at_most_five_failures <- pnbinom(q = 5, size = r, prob = p)

# Poisson example: count when the mean rate is 2.5.
lambda <- 2.5
p_exactly_4_poisson <- dpois(x = 4, lambda = lambda)
p_at_most_4_poisson <- ppois(q = 4, lambda = lambda)

stopifnot(all(c(p_hypergeometric, p_19_failures_then_success,
                p_task_4_exactly_4_failures, p_task_4_at_most_4_failures,
                p_five_failures_before_third_success, p_at_most_five_failures,
                p_exactly_4_poisson, p_at_most_4_poisson) >= 0),
          all(c(p_hypergeometric, p_19_failures_then_success,
                p_task_4_exactly_4_failures, p_task_4_at_most_4_failures,
                p_five_failures_before_third_success, p_at_most_five_failures,
                p_exactly_4_poisson, p_at_most_4_poisson) <= 1))

print(c(
  supplementary_hypergeometric = p_hypergeometric,
  task_3 = p_19_failures_then_success,
  task_3_mean_trial = mean_trial_of_first_success,
  task_4_exactly_4_failures = p_task_4_exactly_4_failures,
  task_4_at_most_4_failures = p_task_4_at_most_4_failures,
  negative_binomial_example = p_five_failures_before_third_success,
  poisson_exactly_4 = p_exactly_4_poisson,
  poisson_at_most_4 = p_at_most_4_poisson
))

# Probability Theory in R
# Validation script for the presentation-aligned study guide.
# Uses base R only.

options(digits = 10)

cat("\n1. Counting and classical probability\n")
password_count <- 26 * 10^2
permutations_5 <- factorial(5)
ordered_3_from_10 <- factorial(10) / factorial(7)
committees_10_choose_3 <- choose(10, 3)
repeated_combinations <- choose(5 + 4 - 1, 4)
two_aces <- choose(4, 2) / choose(52, 2)

stopifnot(password_count == 2600,
          permutations_5 == 120,
          ordered_3_from_10 == 720,
          committees_10_choose_3 == 120,
          repeated_combinations == 70,
          two_aces > 0,
          two_aces < 1)
print(c(password_count = password_count,
        permutations_5 = permutations_5,
        ordered_3_from_10 = ordered_3_from_10,
        combinations_10_choose_3 = committees_10_choose_3,
        repeated_combinations = repeated_combinations,
        p_two_aces = two_aces))

cat("\n2. Probability rules and geometric probability\n")
p_at_least_one_failure <- 1 - 0.9^3
segment_probability <- (7 - 2) / (10 - 0)
square_circle_probability <- pi * 3^2 / 10^2

stopifnot(abs(p_at_least_one_failure - 0.271) < 1e-12,
          abs(segment_probability - 0.5) < 1e-12,
          square_circle_probability > 0,
          square_circle_probability < 1)
print(c(p_at_least_one_failure = p_at_least_one_failure,
        segment_probability = segment_probability,
        square_circle_probability = square_circle_probability))

cat("\n3. Conditional probability and Bayes theorem\n")
prior <- c(A = 0.50, B = 0.30, C = 0.20)
p_defect_given_supplier <- c(A = 0.02, B = 0.04, C = 0.01)
p_defect <- sum(prior * p_defect_given_supplier)
posterior_given_defect <- prior * p_defect_given_supplier / p_defect

stopifnot(abs(p_defect - 0.024) < 1e-12,
          abs(posterior_given_defect["B"] - 0.5) < 1e-12,
          abs(sum(posterior_given_defect) - 1) < 1e-12)
print(c(p_defect = p_defect, posterior_given_defect))

cat("\n4. Discrete random variables, PMF, CDF, expectation, and variance\n")
x <- c(0, 1, 3, 6)
p_x <- c(0.30, 0.40, 0.10, 0.20)
cdf_x <- cumsum(p_x)
mu_x <- sum(x * p_x)
e_x2 <- sum(x^2 * p_x)
var_x <- e_x2 - mu_x^2
sd_x <- sqrt(var_x)
p_1_to_3 <- sum(p_x[x >= 1 & x <= 3])

stopifnot(all(p_x >= 0),
          abs(sum(p_x) - 1) < 1e-12,
          all(diff(cdf_x) >= 0),
          abs(tail(cdf_x, 1) - 1) < 1e-12,
          abs(mu_x - 1.9) < 1e-12,
          abs(var_x - 4.89) < 1e-12,
          abs(p_1_to_3 - 0.5) < 1e-12)
print(data.frame(x = x, pmf = p_x, cdf = cdf_x))
print(c(expected_value = mu_x,
        variance = var_x,
        standard_deviation = sd_x,
        p_1_to_3 = p_1_to_3))

cat("\n5. Binomial distribution\n")
n <- 20
p <- 0.10
binom_exact_3 <- dbinom(x = 3, size = n, prob = p)
binom_2_to_5 <- pbinom(q = 5, size = n, prob = p) - pbinom(q = 1, size = n, prob = p)
binom_at_least_4 <- pbinom(q = 3, size = n, prob = p, lower.tail = FALSE)

stopifnot(all(c(binom_exact_3, binom_2_to_5, binom_at_least_4) >= 0),
          all(c(binom_exact_3, binom_2_to_5, binom_at_least_4) <= 1),
          abs(n * p - 2) < 1e-12,
          abs(n * p * (1 - p) - 1.8) < 1e-12)
print(c(p_exactly_3 = binom_exact_3,
        p_2_to_5 = binom_2_to_5,
        p_at_least_4 = binom_at_least_4,
        mean = n * p,
        variance = n * p * (1 - p)))

cat("\n6. Geometric and negative-binomial distributions\n")
p <- 0.25
geom_three_failures <- dgeom(x = 3, prob = p)
geom_trial_4 <- dgeom(x = 4 - 1, prob = p)
geom_mean <- (1 - p) / p
geom_variance <- (1 - p) / p^2

r <- 3
negbinom_five_failures <- dnbinom(x = 5, size = r, prob = p)
negbinom_at_most_five_failures <- pnbinom(q = 5, size = r, prob = p)
negbinom_mean <- r * (1 - p) / p
negbinom_variance <- r * (1 - p) / p^2

stopifnot(abs(geom_three_failures - geom_trial_4) < 1e-12,
          abs(geom_mean - 3) < 1e-12,
          abs(geom_variance - 12) < 1e-12,
          negbinom_five_failures >= 0,
          negbinom_at_most_five_failures >= negbinom_five_failures,
          abs(negbinom_mean - 9) < 1e-12,
          abs(negbinom_variance - 36) < 1e-12)
print(c(p_three_failures_before_first_success = geom_three_failures,
        geometric_mean = geom_mean,
        geometric_variance = geom_variance,
        p_five_failures_before_third_success = negbinom_five_failures,
        p_at_most_five_failures_before_third_success = negbinom_at_most_five_failures,
        negative_binomial_mean = negbinom_mean,
        negative_binomial_variance = negbinom_variance))

cat("\n7. Poisson distribution\n")
lambda <- 2.5
poisson_p4 <- dpois(x = 4, lambda = lambda)
poisson_at_most_4 <- ppois(q = 4, lambda = lambda)
poisson_q95 <- qpois(p = 0.95, lambda = lambda)

stopifnot(poisson_p4 >= 0,
          poisson_at_most_4 >= poisson_p4,
          poisson_at_most_4 <= 1,
          poisson_q95 >= 0,
          lambda == 2.5)
print(c(p_exactly_4 = poisson_p4,
        p_at_most_4 = poisson_at_most_4,
        q95 = poisson_q95,
        mean = lambda,
        variance = lambda))

cat("\n8. Uniform, exponential, and normal distributions\n")
uniform_cdf_difference <- punif(q = 38.6, min = 11.5, max = 38.6) -
  punif(q = 14.6, min = 11.5, max = 38.6)
uniform_ratio <- (38.6 - 14.6) / (38.6 - 11.5)

rate <- 0.22
exponential_interval <- pexp(q = 8, rate = rate) - pexp(q = 2, rate = rate)

normal_interval <- pnorm(q = 10.1, mean = 9.6, sd = 2.06) -
  pnorm(q = 9.0, mean = 9.6, sd = 2.06)
normal_q90 <- qnorm(p = 0.90, mean = 100, sd = 15)

stopifnot(abs(uniform_cdf_difference - uniform_ratio) < 1e-12,
          abs(uniform_cdf_difference - 0.885608856089) < 1e-10,
          abs(exponential_interval - 0.47199155726) < 1e-10,
          abs(normal_interval - 0.210462819917) < 1e-10,
          normal_q90 > 100)
print(c(uniform_interval = uniform_cdf_difference,
        exponential_interval = exponential_interval,
        normal_interval = normal_interval,
        normal_q90 = normal_q90))

cat("\n9. Sums of independent normal random variables\n")
mu_x_normal <- 10
sd_x_normal <- 2
mu_y_normal <- 5
sd_y_normal <- 3
mu_sum <- mu_x_normal + mu_y_normal
sd_sum <- sqrt(sd_x_normal^2 + sd_y_normal^2)
p_sum_at_most_18 <- pnorm(q = 18, mean = mu_sum, sd = sd_sum)

stopifnot(mu_sum == 15,
          abs(sd_sum - sqrt(13)) < 1e-12,
          p_sum_at_most_18 > 0,
          p_sum_at_most_18 < 1)
print(c(mean_sum = mu_sum,
        sd_sum = sd_sum,
        p_sum_at_most_18 = p_sum_at_most_18))

cat("\n10. Jointly distributed discrete random variables\n")
x_values <- c(0, 1)
y_values <- c(0, 2)
p_xy <- matrix(c(0.10, 0.20,
                 0.30, 0.40),
               nrow = 2, byrow = TRUE,
               dimnames = list(X = x_values, Y = y_values))
p_x_marginal <- rowSums(p_xy)
p_y_marginal <- colSums(p_xy)
p_x_given_y2 <- p_xy[, "2"] / p_y_marginal["2"]
independence_residuals <- p_xy - outer(p_x_marginal, p_y_marginal)

x_grid <- row(p_xy)
y_grid <- col(p_xy)
x_actual <- matrix(x_values[x_grid], nrow = nrow(p_xy))
y_actual <- matrix(y_values[y_grid], nrow = nrow(p_xy))

mu_x <- sum(x_actual * p_xy)
mu_y <- sum(y_actual * p_xy)
e_x2 <- sum(x_actual^2 * p_xy)
e_y2 <- sum(y_actual^2 * p_xy)
e_xy <- sum(x_actual * y_actual * p_xy)
var_x <- e_x2 - mu_x^2
var_y <- e_y2 - mu_y^2
cov_xy <- e_xy - mu_x * mu_y
cor_xy <- cov_xy / sqrt(var_x * var_y)

a <- 2
b <- 3
mu_z <- a * mu_x + b * mu_y
var_z <- a^2 * var_x + b^2 * var_y + 2 * a * b * cov_xy

stopifnot(abs(sum(p_xy) - 1) < 1e-12,
          abs(sum(p_x_marginal) - 1) < 1e-12,
          abs(sum(p_y_marginal) - 1) < 1e-12,
          abs(sum(p_x_given_y2) - 1) < 1e-12,
          any(abs(independence_residuals) > 1e-12),
          abs(mu_x - 0.7) < 1e-12,
          abs(mu_y - 1.2) < 1e-12,
          abs(var_x - 0.21) < 1e-12,
          abs(var_y - 0.96) < 1e-12,
          abs(cov_xy + 0.04) < 1e-12,
          abs(mu_z - 5) < 1e-12,
          abs(var_z - 9) < 1e-12)
print(p_xy)
print(c(p_x_marginal, p_y_marginal, p_x_given_y2))
print(c(E_X = mu_x, E_Y = mu_y,
        Var_X = var_x, Var_Y = var_y,
        Cov_XY = cov_xy, Cor_XY = cor_xy,
        E_Z = mu_z, Var_Z = var_z, SD_Z = sqrt(var_z)))

cat("\nAll presentation-aligned numerical validation checks passed.\n")

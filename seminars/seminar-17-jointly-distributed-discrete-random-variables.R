# Workshop 17 — Jointly Distributed Discrete Random Variables
# Presentation coverage: joint PMF, marginal PMFs, conditional PMFs, covariance, and correlation.
# Base R only.

options(digits = 10)

joint_moments <- function(p_xy, x_values, y_values) {
  stopifnot(all(p_xy >= 0), abs(sum(p_xy) - 1) < 1e-12,
            nrow(p_xy) == length(x_values), ncol(p_xy) == length(y_values))

  p_x <- rowSums(p_xy)
  p_y <- colSums(p_xy)
  x_grid <- matrix(rep(x_values, times = length(y_values)), nrow = nrow(p_xy))
  y_grid <- matrix(rep(y_values, each = length(x_values)), nrow = nrow(p_xy))

  e_x <- sum(x_grid * p_xy)
  e_y <- sum(y_grid * p_xy)
  e_x2 <- sum(x_grid^2 * p_xy)
  e_y2 <- sum(y_grid^2 * p_xy)
  e_xy <- sum(x_grid * y_grid * p_xy)
  var_x <- e_x2 - e_x^2
  var_y <- e_y2 - e_y^2
  cov_xy <- e_xy - e_x * e_y
  cor_xy <- cov_xy / sqrt(var_x * var_y)

  list(
    p_x = p_x, p_y = p_y,
    E_X = e_x, E_Y = e_y,
    Var_X = var_x, Var_Y = var_y,
    E_XY = e_xy, Cov_XY = cov_xy, Cor_XY = cor_xy
  )
}

# Question 1: joint PMF for X, Y in {0, 1, 2, 3}.
x1 <- 0:3
y1 <- 0:3
p_xy_1 <- matrix(
  c(0.07, 0.07, 0.06, 0.02,
    0.09, 0.06, 0.07, 0.04,
    0.06, 0.07, 0.14, 0.16,
    0.01, 0.01, 0.03, 0.04),
  nrow = 4, byrow = TRUE,
  dimnames = list(X = x1, Y = y1)
)
summary_q1 <- joint_moments(p_xy_1, x1, y1)

# Conditional distribution of Y given X = 3.
p_y_given_x3_q1 <- p_xy_1["3", ] / sum(p_xy_1["3", ])
e_y_given_x3_q1 <- sum(y1 * p_y_given_x3_q1)

# Question 2: a second joint PMF for X, Y in {0, 1, 2}.
x2 <- 0:2
y2 <- 0:2
p_xy_2 <- matrix(
  c(0.09, 0.07, 0.03,
    0.14, 0.23, 0.10,
    0.07, 0.16, 0.11),
  nrow = 3, byrow = TRUE,
  dimnames = list(X = x2, Y = y2)
)
summary_q2 <- joint_moments(p_xy_2, x2, y2)

# Example event from Question 2: P(X + Y <= 2).
x_grid_2 <- matrix(rep(x2, times = length(y2)), nrow = nrow(p_xy_2))
y_grid_2 <- matrix(rep(y2, each = length(x2)), nrow = nrow(p_xy_2))
p_x_plus_y_at_most_2_q2 <- sum(p_xy_2[x_grid_2 + y_grid_2 <= 2])

stopifnot(abs(sum(p_y_given_x3_q1) - 1) < 1e-12,
          is.finite(summary_q1$Cor_XY), is.finite(summary_q2$Cor_XY),
          p_x_plus_y_at_most_2_q2 >= 0, p_x_plus_y_at_most_2_q2 <= 1)

print(summary_q1)
print(c(question_1_E_Y_given_X_3 = e_y_given_x3_q1))
print(summary_q2)
print(c(question_2_P_X_plus_Y_at_most_2 = p_x_plus_y_at_most_2_q2))

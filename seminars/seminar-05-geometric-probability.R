# Workshop 5 — Geometric Probability
# Presentation exercises implemented: Tasks 20–23.
# Base R only.

options(digits = 10)

# Task 20: area ratio for the complement of two squares inside a circle.
radius <- 25
p_task_20 <- 1 - 2 * radius^2 / (pi * radius^2)
stopifnot(abs(p_task_20 - (1 - 2 / pi)) < 1e-12)

# Task 21: favourable length divided by total length.
p_task_21 <- 80 / 160

# Task 22: favourable circle area divided by square area.
p_task_22 <- pi * 3.5^2 / 8^2

# Task 23: favourable angle divided by full angle.
p_task_23 <- 120 / 360
stopifnot(abs(p_task_23 - 1 / 3) < 1e-12)

print(c(
  task_20 = p_task_20,
  task_21 = p_task_21,
  task_22 = p_task_22,
  task_23 = p_task_23
))

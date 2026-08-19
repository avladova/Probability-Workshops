# Workshop 2 — Combinatorics
# Presentation exercises implemented: Tasks 3–7.
# Base R only.

options(digits = 10)

# Task 3: number of permutations of 7 distinct objects.
permutations_7 <- factorial(7)
stopifnot(permutations_7 == 5040)

# Task 4: ordered selection of 2 people from 50.
ordered_pairs_50 <- 50 * 49
stopifnot(abs(ordered_pairs_50 - factorial(50) / factorial(48)) < 1e-8)

# Task 5: select 3 objects from 5.
n <- 5
k <- 3
ordered_5_choose_3 <- factorial(n) / factorial(n - k)
unordered_5_choose_3 <- choose(n, k)

# Task 6: select 3 objects from 6.
n <- 6
k <- 3
ordered_6_choose_3 <- factorial(n) / factorial(n - k)
unordered_6_choose_3 <- choose(n, k)

# Task 7: select 2 objects from 8, where order does not matter.
unordered_8_choose_2 <- choose(8, 2)

print(c(
  task_3_permutations_7 = permutations_7,
  task_4_ordered_pairs_50 = ordered_pairs_50,
  task_5_ordered = ordered_5_choose_3,
  task_5_unordered = unordered_5_choose_3,
  task_6_ordered = ordered_6_choose_3,
  task_6_unordered = unordered_6_choose_3,
  task_7_unordered = unordered_8_choose_2
))

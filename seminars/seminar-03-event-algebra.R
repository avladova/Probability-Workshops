# Workshop 3 — Event Algebra
# Base-R examples for sample spaces, union, intersection, complement, and set identities.

set.seed(2026)
dice <- 1:6

# A sample space from repeated rolls; unique() converts observed rolls into an event set.
rolls_a <- sample(dice, size = 3, replace = FALSE)
rolls_b <- sample(dice, size = 4, replace = FALSE)
rolls_c <- unique(sample(dice, size = 4, replace = TRUE))

# Basic set operations.
intersection_ab <- intersect(rolls_a, rolls_b)
union_ab <- union(rolls_a, rolls_b)
only_a <- setdiff(rolls_a, rolls_b)
only_b <- setdiff(rolls_b, rolls_a)

# Inclusion–exclusion identity for finite sets.
union_from_parts <- union(union(only_a, intersection_ab), only_b)
stopifnot(setequal(union_ab, union_from_parts))

# Event complement in the die sample space.
even <- c(2, 4, 6)
not_even <- setdiff(dice, even)
stopifnot(setequal(union(even, not_even), dice), length(intersect(even, not_even)) == 0L)

# De Morgan's law: (A union B)^c = A^c intersection B^c.
a <- c(1, 2, 3)
b <- c(3, 4, 5)
left_side <- setdiff(dice, union(a, b))
right_side <- intersect(setdiff(dice, a), setdiff(dice, b))
stopifnot(setequal(left_side, right_side))

print(list(
  event_A = rolls_a,
  event_B = rolls_b,
  event_C = rolls_c,
  A_intersection_B = intersection_ab,
  A_union_B = union_ab,
  not_even = not_even,
  de_morgan_verified = setequal(left_side, right_side)
))

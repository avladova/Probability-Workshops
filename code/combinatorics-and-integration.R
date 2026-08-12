# ============================================================
# Combinatorics & Integration / Комбинаторика и интегрирование
# ============================================================

# ------------------------------------------------------------
# Combinatorial Formulas / Комбинаторика
# ------------------------------------------------------------
n <- 16
k <- 14

# Combinations / Сочетания
C1 <- factorial(n) / (factorial(k) * factorial(n - k))
C2 <- choose(n, k)

# Excel: ФАКТР(n) / (ФАКТР(k) * ФАКТР(n-k))

# Permutations / Перестановки
library(combinat)
permn(x = c("A", "B", "C"))
permn(x = 2:5)

# All combinations of n choose k
combn(8, 5)

# Arrangements with repetition
# n^k

# Combinations with repetition
# choose(n + k - 1, k)

# ------------------------------------------------------------
# Basic Statistics / Основные статистики
# ------------------------------------------------------------
a <- 1:30
summary(a)      # stats at a glance
mean(a)
median(a)
var(a)          # variance
sd(a)           # standard deviation
min(a)
max(a)
quantile(a)
IQR(a)          # interquartile range
boxplot(a)      # box and whiskers plot

# ------------------------------------------------------------
# Integration / Интегрирование
# ------------------------------------------------------------

# Workshop 11: Integrate to get F(x) from f(x)
I <- function(x) { 1 / x^2 }
integrate(I, lower = 2, upper = 3)

# Or anonymous function
integrate(function(x) { 1 / x^2 }, 2, 3)

# Find constant C in f(x) = C / x^2
C <- 1 / integrate(function(x) { 1 / x^2 }, 2, 3)$value

# Workshop 12: Integrate if pdf is known distribution
integrand <- function(x) { dunif(x, min = 0, max = 3) }
integrate(integrand, lower = 2, upper = 3)
integrate(integrand, lower = 0, upper = 1)

# Double integral / Двойной интеграл (symbolic)
library(rSymPy)
.jinit()
sympy("var('x')")
sympy("var('y')")
sympy("var('C')")
sympy("integrate(0.5*x + C*y, (y, 0, 2), (x, 0, 1))")

# New version with Ryacas
library(Ryacas)
f3y <- ysym("0.75*(2-2*x^2)")
integrate(f3y, "x")

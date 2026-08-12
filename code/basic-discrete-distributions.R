# ============================================================
# Basic Discrete Distributions / Базовые дискретные распределения
# ============================================================
# Source: vladova.ru/Code
# Справочный файл с функциями R и Excel для дискретных распределений

# ------------------------------------------------------------
# 1. Bernoulli Distribution — Распределение Бернулли
# Modeling the outcome of a single trial with two possible outcomes
# ------------------------------------------------------------
# R
# pmf: dbinom(x, size=1, prob)
# cmf: pbinom(q, size=1, prob)
# quantile: qbinom(q, size=1, prob)

# Excel
# pmf:  BINOM.DIST(number_success, number_trials=1, prob, 0)
# cmf:  BINOM.DIST(number_success, number_trials=1, prob, 1)
# quantile: BINOM.INV(trials=1, probability_s, alpha)

# Example: probability of a head in 1 coin throw
dbinom(1, size=1, prob=0.5)

# ------------------------------------------------------------
# 2. Binomial Distribution — Биномиальное распределение
# Modeling the number of successes in a series of independent trials
# ------------------------------------------------------------
# R
# pmf: dbinom(x, size, prob)
# cmf: pbinom(q, size, prob)
# quantile: qbinom(q, size, prob)

# Excel
# pmf:  BINOM.DIST(number_success, number_trials, prob, 0)
# cmf:  BINOM.DIST(number_success, number_trials, prob, 1)
# quantile: BINOM.INV(trials, probability_s, alpha)

# Example: number of heads in a series of coin throws
n <- 20
p <- 0.4
x <- 0:n
prob <- dbinom(x, size = n, prob = p)
barplot(prob, names.arg = x, col = "steelblue",
        main = "Binomial Distribution (n=20, p=0.4)",
        xlab = "Number of successes", ylab = "Probability")

# ------------------------------------------------------------
# 3. Geometric Distribution — Геометрическое распределение
# Modeling the number of failures before the first success
# ------------------------------------------------------------
# R
# pmf: dgeom(x, prob)
# cmf: pgeom(x, prob)
# quantile: qgeom(q, prob)

# Example: number of misses until first hit
dgeom(3, prob = 0.2)

# ------------------------------------------------------------
# 4. Hypergeometric Distribution — Гипергеометрическое распределение
# Selection without replacement from a finite population
# ------------------------------------------------------------
# R
# pmf: dhyper(x, m, n, k)
# cmf: phyper(q, m, n, k)
# quantile: qhyper(q, m, n, k)

# Excel
# pmf: HYPGEOM.DIST(sample_s, number_sample, population_s, number_pop, 0)
# cmf: HYPGEOM.DIST(sample_s, number_sample, population_s, number_pop, 1)

# Example: ballot selection, lotto
dhyper(2, m = 10, n = 15, k = 5)

# ------------------------------------------------------------
# 5. Poisson Distribution — Распределение Пуассона
# Modeling the number of events in a fixed period of time
# ------------------------------------------------------------
# R
# pmf: dpois(x, lambda)
# cmf: ppois(q, lambda)
# quantile: qpois(q, lambda)

# Excel
# pmf: POISSON.DIST(x, mean, 0)
# cmf: POISSON.DIST(x, mean, 1)

# Example: number of calls per hour
dpois(4, lambda = 3)

# ------------------------------------------------------------
# 6. Negative Binomial Distribution — Отрицательное биномиальное
# Models the number of failures until a specified number of successes
# ------------------------------------------------------------
# R
# pmf: dnbinom(x, size, prob)
# cmf: pnbinom(q, size, prob)
# quantile: qnbinom(q, size, prob)

# Excel
# pmf: NEGBINOM.DIST(number_f, number_s, probability_s, 0)
# cmf: NEGBINOM.DIST(number_f, number_s, probability_s, 1)
# quantile: NEGBINOM.INV(number_f, number_s, probability_s, alpha)

# ------------------------------------------------------------
# 7. Discrete Uniform Distribution — Дискретное равномерное
# Modeling equally likely events over a specified interval
# ------------------------------------------------------------
# Example: a dice
s <- 1:6
p <- rep(1/6, 6)
sum(p)  # check
plot(s, p, col = "red", type = "h", lwd = 2,
     main = "Discrete Uniform: Dice", xlab = "x", ylab = "P(X=x)")

Ms <- sum(s * p)
Ds <- sum(s^2 * p) - Ms^2       # formula 1
DDs <- sum((s - Ms)^2 * p)      # formula 2

# ------------------------------------------------------------
# Summary / Обобщение
# ------------------------------------------------------------
# - Binomial: sampling WITH replacement (probability is constant)
# - Hypergeometric: sampling WITHOUT replacement (probability changes)
# - Geometric: number of trials until first success (including success)
# - Geometric is a discrete analogue of Exponential distribution
# - Negative Binomial: generalization of Geometric — failures before r successes

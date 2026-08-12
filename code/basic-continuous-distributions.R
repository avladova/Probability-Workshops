# ============================================================
# Basic Continuous Distributions / Базовые непрерывные распределения
# ============================================================

# ------------------------------------------------------------
# 1. Uniform Distribution — Равномерное распределение
# ------------------------------------------------------------
# PDF
curve(dunif(x, min = 1, max = 2), from = -1, to = 3,
      xlab = "x", ylab = "f(x)", main = "PDF for Unif(1,2)", col = "blue")

# CDF
curve(punif(x, min = 1, max = 2), from = -1, to = 3,
      xlab = "x", ylab = "F(x)", main = "CDF for Unif(1,2)", col = "red")

# ------------------------------------------------------------
# 2. Exponential Distribution — Экспоненциальное распределение
# ------------------------------------------------------------
x <- seq(0, 10, by = 0.1)
rate <- 5  # lambda = 1 / E(x)
plot(x, dexp(x, rate), type = "l", col = "darkgreen",
     main = "Exponential PDF (rate=5)", xlab = "x", ylab = "f(x)")
plot(x, pexp(x, rate), type = "l", col = "purple",
     main = "Exponential CDF (rate=5)", xlab = "x", ylab = "F(x)")

# ------------------------------------------------------------
# 3. Normal Distribution — Нормальное распределение
# ------------------------------------------------------------
# R
# pdf: dnorm(x, mean, sd)
# cdf: pnorm(p, mean, sd)
# quantile: qnorm(q, mean, sd)

# Excel
# pdf/cdf: NORM.DIST(x, mean, sd, cumulative)
# quantile: NORM.INV(p, mean, sd)

# Example: standardization
# If X ~ N(45, 18), find P(X > 52.5)
library(mosaic)
plotDist("norm")

integrand <- function(x) { dnorm(x, mean = 0, sd = 1) }
integrate(integrand, lower = (52.5 - 45) / sqrt(18), upper = Inf)

# ------------------------------------------------------------
# 4. Log-Normal Distribution — Логнормальное распределение
# ------------------------------------------------------------
# Describes a variable whose logarithm is normally distributed
# Example: income, stock prices
# dlnorm(x, meanlog, sdlog)

# ------------------------------------------------------------
# 5. Student's t-Distribution — Распределение Стьюдента
# ------------------------------------------------------------
# Used in statistical tests with small samples or unknown variance
# R
# pdf: dt(x, df)
# cdf: pt(q, df)
# quantile: qt(q, df)

# Excel
# pdf/cdf: T.DIST(x, degrees_freedom, cumulative)
# Right tail: T.DIST.RT(x, degrees_freedom)
# Two tails: T.DIST.2T(x, degrees_freedom)
# quantile: T.INV(probability, degrees_freedom)

# ------------------------------------------------------------
# 6. Pareto Distribution — Распределение Парето
# ------------------------------------------------------------
# A model for rare but impactful events (wealth, crises, disasters)
library(actuar)
# pdf: dpareto(x, shape = alpha, scale = x_min)
# cdf: ppareto(q, shape = alpha, scale = x_min)
# quantile: qpareto(p, shape = alpha, scale = x_min)

# ------------------------------------------------------------
# General / Обобщение
# ------------------------------------------------------------
# Standard Normal: mean = 0, sd = 1
# pdf: dnorm(x); cdf: pnorm(p); quantile: qnorm(q)

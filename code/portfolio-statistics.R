# ============================================================
# Portfolio Analysis & Advanced Statistics
# Портфельный анализ и продвинутая статистика
# ============================================================

# ------------------------------------------------------------
# Correlation Matrix / Корреляционная матрица
# ------------------------------------------------------------
library(corrplot)
# cormat <- cor(as.matrix(dataset_name))
# corrplot(cormat, method = "number", order = "FPC")

# Alternative with corrgram
# library(corrgram)
# corrgram(dataset_name, font.labels = 6,
#          lower.panel = panel.ellipse,
#          upper.panel = panel.cor,
#          diag.panel = panel.density)

# ------------------------------------------------------------
# Nonlinear correlation / Нелинейная связь
# ------------------------------------------------------------
library(devtools)
# devtools::install_github("r-lib/remotes")
# install_github("ProcessMiner/nlcor", force = TRUE)
library(nlcor)

a <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
b <- c(1, 1, 2, 3, 4, 5, 7, 5, 4, 3, 2, 1, 1)
plot(a, b, lwd = 10)
cor(a, b)
ab <- nlcor(a, b, plt = TRUE)
ab$cor.estimate
print(ab$cor.plot)

# ------------------------------------------------------------
# Portfolio of 2 stocks / Портфель двух активов
# ------------------------------------------------------------
risk <- function(x1, x2, s1 = 0.05, s2 = 0.14, ro = 0.36) {
  (s1^2 * x1^2 + s2^2 * x2^2 + 2 * ro * s1 * s2 * x1 * x2)
}

gb_risk <- function(x) risk(x[1], x[2])

constraint.mat <- rbind(
  c(-1, -1),    # x1 + x2 = 1
  c(1, 0),      # x1 >= 0
  c(0, 1),      # x2 >= 0
  c(0.16, 0.23) # expected return constraint
)
b <- c(-1, 0, 0, 0.1)

constrOptim(c(0.4, 0.4), gb_risk, NULL, constraint.mat, b)

# ------------------------------------------------------------
# Distribution plotting with mosaic / Функции распределения
# ------------------------------------------------------------
library(mosaic)
plotDist("norm", mean = 1, sd = 1, col = "red", kind = "density", under = TRUE)
plotDist("norm", kind = "cdf")
plotDist("exp", kind = "histogram")
plotDist("binom", 25, 0.25)

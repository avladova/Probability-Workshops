# 📊 Probability-Workshops
### R-based educational materials for Data Analysis course | Fall Semester

[![R](https://img.shields.io/badge/R-4.x-blue?logo=r)](https://www.r-project.org/)
[![Course Site](https://img.shields.io/badge/Course%20Site-vladova.ru%2FProbability-green)](http://vladova.ru/Probability)

> **Educational repository** with R implementations of core Probability Theory concepts,  
> designed for undergraduate students in Data Analysis. Covers combinatorics, distributions,  
> conditional probability, random variables, CLT, and applied computational projects.

---

## 🎓 Course Overview

This repository contains workshop materials for the **Probability Theory** module  
of the *Data Analysis* course. Materials are used in classroom seminars and include:

- **17 seminar sessions** with practical R exercises
- **2 midterm examples** with solutions
- **2 exam variants** with detailed solutions
- **1 computational project** (stock market analysis)

📖 **Supplementary materials:**
- [Course website — games, activities, predictions](http://vladova.ru/Probability)
- [Bilingual Glossary](glossary/autumn-term-glossary.md)
- [Distribution cheat-sheets](code/)
- [Books, manuals & external resources](resources/records-and-links.md)

---

## 📂 Repository Structure

| Folder | Contents |
|--------|----------|
| `seminars/` | 14 seminar sessions covering the full curriculum |
| `tests/` | Exam and midterm variants with solutions (anonymized) |
| `code/` | R cheat-sheets: distributions, combinatorics, integration, portfolio analysis |
| `glossary/` | Bilingual glossary of probability & statistics terms |
| `resources/` | Books, manuals, creative topics, conference info, tool links |

---

## 🗺️ Curriculum Map

| # | Topic | File |
|---|-------|------|
| 04 | Event Algebra | `seminars/seminar-04-event-algebra.R` |
| 05 | Introduction to Probability | `seminars/seminar-05-intro-to-probability.R` |
| 06 | Combinatorics | `seminars/seminar-06-combinatorics.R` |
| 06a | Geometric Probability | `seminars/seminar-06a-geometric-probability.R` |
| 07 | Conditional Probability & Bayes | `seminars/seminar-07-conditional-probability.R` |
| 08 | PMF & CDF | `seminars/seminar-08-pmf-cdf.R` |
| 09 | Bernoulli & Binomial Distributions | `seminars/seminar-09-bernoulli-binomial.R` |
| 10 | Hypergeometric & Poisson Distributions | `seminars/seminar-10-hypergeometric-poisson.R` |
| 11 | Covariance and Correlation | `seminars/seminar-11-covariance-correlation.R` |
| 12 | Continuous Random Variables | `seminars/seminar-12-continuous-random-variables.R` |
| 14 | Jointly Distributed Discrete RVs | `seminars/seminar-14-jointly-distributed-discrete.R` |
| 17 | Central Limit Theorem | `seminars/seminar-17-central-limit-theorem.R` |

---

## 🚀 Quick Start

```r
# Example: Binomial distribution visualization from Seminar 9
n <- 20
p <- 0.4
x <- 0:n
prob <- dbinom(x, size = n, prob = p)
barplot(prob, names.arg = x, col = "steelblue",
        main = "Binomial Distribution (n=20, p=0.4)",
        xlab = "Number of successes", ylab = "Probability")
```

---

## 🛠️ Tools & Technologies

- **R** — statistical computing and graphics
- **Base R graphics** — distribution visualizations
- **GitKraken** — version control

---

## 👩‍🏫 About the Course

These materials support the *Data Analysis* course.  
The course combines theoretical foundations with hands-on R programming.

🔗 **External resources:**
- [Interactive course site](http://vladova.ru/Probability) — games, forms, predictions
- [Probability & Game Theory Glossary](http://vladova.ru/Glossary) — bilingual terms (EN/RU)
- [R Code Reference](http://vladova.ru/Code) — distributions, Excel functions, portfolio formulas

---

## ✉️ Contact

For questions about the course materials, please reach out via GitHub Issues  
or visit [vladova.ru](http://vladova.ru).

# Seminar alignment map

**Reference:** the ordered presentation list at [vladova.ru/Probability](http://vladova.ru/Probability), downloaded and compared with the seminar source files on 19 August 2026.

## Renaming and coverage decisions

| Presentation | Required seminar filename | Current source or action | Exercise correspondence and correction |
|---|---|---|---|
| Workshop 2 — Combinatorics | `seminar-02-combinatorics.R` | Rename `Seminar 6 Combinatorics Intro.R` | Existing comments `task 3`–`task 7` match the combinatorics exercise sequence. Replace `library(combinat)` with base R `choose()` so the file is self-contained. |
| Workshop 3 — Event Algebra | `seminar-03-event-algebra.R` | Convert and rename `Seminar 4 Event Algebra.txt` | Preserve event-set examples; change extension to `.R`, add the workshop number/title header, and correct misleading dice comments. |
| Workshop 4 — Probability: Rules of Probability | `seminar-04-probability-rules.R` | Rename `Seminar 5 Intro to probability.R` | Retain tasks 12–14 and make every event-probability calculation explicit and reproducible. |
| Workshop 5 — Geometric Probability | `seminar-05-geometric-probability.R` | Rename `Seminar 6a Geometric Probability.R` | Retain tasks 20–23; express the area/length ratios clearly. |
| Workshop 6 — Conditional Probability | `seminar-06-conditional-probability.R` | Rename `Seminar 7 Conditional probabilities.R` | Retain questions 7–14; clarify Bayes/total-probability calculations. |
| Workshop 7 — Discrete Random Variables | `seminar-07-discrete-random-variables.R` | Add a focused file | The presentation gives PMF/CDF, expectation, variance, and sums. Add a self-contained code example from these slides rather than mislabel Workshop 11 code as Workshop 7. |
| Workshop 9 — Event Algebra: Complicated Cases | `seminar-09-event-algebra-complicated-cases.R` | Split from `Seminar 6a & 8a geom probability&event algebra.R` | The combined file conflates Workshops 5 and 9. Keep only its complicated-event algebra examples in a new Workshop 9 file; remove the duplicate combined file. |
| Workshop 10 — Binomial Distribution | `seminar-10-binomial-distribution.R` | Rename and repair `Seminar 9 Bernoulli&Binom distributions.R` | Existing questions 1 onward correspond to Workshop 10; remove invalid `dbinom()` calls and complete the probability arguments. |
| Workshop 11 — PMF & CMF | `seminar-11-pmf-cmf.R` | Rename and repair `Seminar 8 pmf&cdf.R`; remove duplicate `pmf&cdf.R` | Existing tasks 1–3 reproduce Workshop 11 tables exactly. Replace invalid standalone chained inequalities with valid R probability expressions; remove later topic drift. |
| Workshop 11.1 — Negative Binomial, Geometric, Poisson | `seminar-11-1-negative-binomial-geometric-poisson.R` | Rename and repair `Seminar 10 Heom&Hyper&Poisson distributions.R` | Correct the title typo and variables; retain the geometric, negative-binomial and Poisson tasks. Hypergeometric material remains as an explicitly labelled supplementary example present in the source code. |
| Workshop 12 — Continuous Random Variable | `seminar-12-continuous-random-variable.R` | Rename and repair `Seminar 12 Continuous random variables.R` | Keep questions 1–3, replace non-base `rSymPy` calls with base R `integrate()`, and finish the truncated second-moment calculation. |
| Workshop 13 — Uniform & Exponential Distributions | `seminar-13-uniform-exponential-distributions.R` | Rewrite from the Workshop 13 portion of `Seminar 13.R` | Keep questions 1–4, remove unlabelled normal-distribution spillover, and implement all computations in base R. |
| Workshop 14 — Normal Distribution & Sum of Random Variables | `seminar-14-normal-distribution-sums.R` | Add a dedicated file | The current unnamed normal examples inside `Seminar 13.R` are incomplete and mixed. Add a focused base-R implementation for Workshop 14 questions 1–9. |
| Workshop 15 — Poisson & Negative Binomial Distributions | `seminar-15-poisson-negative-binomial-distributions.R` | Replace misaligned and broken `Seminar 15.R` | Current file is a mixture/discontinuous-topic script and fails to parse. Replace it with code for Workshop 15 questions 8–13, including Poisson and negative-binomial calculations. |
| Workshop 16 — Covariance & Correlation | `seminar-16-covariance-correlation.R` | Rename `Seminar 11 Covariance and correlation.R` | Existing questions 1–7 match Workshop 16; correct variable naming and preserve matrix calculation. |
| Workshop 17 — Jointly Distributed Discrete Random Variables | `seminar-17-jointly-distributed-discrete-random-variables.R` | Rename and repair `Seminar 14 Jointly Distributed Discrete Random variables.R` | Existing questions 1–2 are aligned. Remove the interactive `View()` call and state marginals/conditional distributions correctly. |

## Legacy content

`Seminar 17 Central Limit Theorem.R` does not match the presentation list: Workshop 17 is jointly distributed discrete random variables. Preserve it outside `seminars/` as an archived legacy script instead of presenting it as Workshop 17.

## Global corrections

The revised branch will use lowercase, hyphenated names and the actual workshop numbers. All R files will begin with an explicit workshop title and identify the presentation question numbers that they implement. Duplicate code, broken syntax, unlabelled topic drift, interactive calls, and non-standard library requirements will be removed from the seminar scripts.

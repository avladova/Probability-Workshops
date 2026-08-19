# Repository structure: Probability-Workshops

```text
Probability-Workshops/
├── archive/                            # Legacy material kept outside the current seminar sequence
│   └── legacy-seminar-17-central-limit-theorem.R
├── code/                               # Reusable R reference scripts
├── glossary/                           # Bilingual terminology
├── resources/                          # External resources and course links
├── seminars/                           # Presentation-aligned probability workshops
│   ├── seminar-02-combinatorics.R
│   ├── seminar-03-event-algebra.R
│   ├── seminar-04-probability-rules.R
│   ├── seminar-05-geometric-probability.R
│   ├── seminar-06-conditional-probability.R
│   ├── seminar-07-discrete-random-variables.R
│   ├── seminar-09-event-algebra-complicated-cases.R
│   ├── seminar-10-binomial-distribution.R
│   ├── seminar-11-pmf-cmf.R
│   ├── seminar-11-1-negative-binomial-geometric-poisson.R
│   ├── seminar-12-continuous-random-variable.R
│   ├── seminar-13-uniform-exponential-distributions.R
│   ├── seminar-14-normal-distribution-sums.R
│   ├── seminar-15-poisson-negative-binomial-distributions.R
│   ├── seminar-16-covariance-correlation.R
│   └── seminar-17-jointly-distributed-discrete-random-variables.R
├── tests/                              # Assessment examples
├── README.md                           # Repository overview
├── index.md                            # Course navigation
└── REPO_STRUCTURE.md                   # This file
```

## Naming rules

Seminar filenames use lower-case letters, hyphens, and the actual workshop number from the presentation collection at [vladova.ru/Probability](http://vladova.ru/Probability). Workshop `11.1` is represented as `seminar-11-1-...R` because a filename cannot use a decimal point as part of its extension.

Each seminar script must begin with the exact presentation title, identify the corresponding presentation exercise numbers where applicable, and run with base R unless a dependency is explicitly documented.

## Legacy materials

A Central Limit Theorem script was previously labelled as Seminar 17. The presentation collection now identifies Workshop 17 as **Jointly Distributed Discrete Random Variables**, so the former script is preserved in `archive/` rather than being included in the active seminar sequence.

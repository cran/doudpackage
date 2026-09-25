
<!-- README.md is generated from README.Rmd. Please edit that file -->

# doudpackage

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/doudpackage)](https://CRAN.R-project.org/package=doudpackage)
<!-- badges: end -->

doudpackage builds the “table one” of a bio-medical paper: one row per
variable, one column per sub-group, the right statistical test picked
for you, and an HTML or LaTeX table ready to paste into the manuscript.

You give it a `data.frame` and the name of the grouping variable. It
works out which variables are quantitative and which are qualitative,
describes each of them, compares them across the groups, and renders the
result with [kableExtra](https://CRAN.R-project.org/package=kableExtra).

## Installation

``` r
install.packages("doudpackage")
```

Development version:

``` r
# install.packages("devtools")
devtools::install_github("tiago972/doudpackage")
```

## Getting started

Two functions do the work: `descTab()` computes the table,
`parseClassFun()` renders it.

``` r
library(doudpackage)

data(iris)
iris$treated <- factor(sample(c("no", "yes"), 150, replace = TRUE))
iris$dose <- runif(150, min = 0, max = 100)
iris$dose[sample(1:150, 5)] <- NA

tab <- descTab(iris, group = "Species")
tab["table"]
#>                      var      setosa  versicolor virginica       Total pvalue
#> 1         dose mean (SD) 56.3 (24.6) 58.4 (27.9) 51.5 (31) 55.4 (27.9)  0.464
#> 2 Petal.Length mean (SD)   1.5 (0.2)   4.3 (0.5) 5.6 (0.6)   3.8 (1.8)  0.000
#> 3  Petal.Width mean (SD)   0.2 (0.1)   1.3 (0.2)   2 (0.3)   1.2 (0.8)  0.000
#> 4 Sepal.Length mean (SD)     5 (0.4)   5.9 (0.5) 6.6 (0.6)   5.8 (0.8)  0.000
#> 5  Sepal.Width mean (SD)   3.4 (0.4)   2.8 (0.3)   3 (0.3)   3.1 (0.4)  0.000
#> 6            treated, no     23 (46)     26 (52)   28 (56)   77 (51.3)  0.602
#> 7           treated, yes     27 (54)     24 (48)   22 (44)   73 (48.7)  0.602
```

`descTab()` returns an S4 object; the raw data frame is under
`["table"]` and `parseClassFun()` turns it into the formatted table:

``` r
parseClassFun(tab)
```

## What goes into a row

| Variable | Described as | Compared with |
|----|----|----|
| numeric / integer, normal | mean (SD) | t test (2 groups), ANOVA (3+) |
| numeric / integer, non normal | median (IQR) | Wilcoxon (2 groups), Kruskal-Wallis (3+) |
| factor / ordered factor | n (%) per level | Chi-squared, Fisher when the approximation is unreliable |

Variables of any other type (character, `Date`, logical) are ignored
with a warning. Set `normality` to choose how quantitative variables are
handled:

``` r
descTab(iris, group = "Species", normality = "normal")      # mean (SD) everywhere
descTab(iris, group = "Species", normality = "non normal")  # median (IQR) everywhere
descTab(iris, group = "Species", normality = "assess")      # Shapiro-Wilk, variable by variable
```

## Shaping the output

``` r
tab <- descTab(iris, group = "Species", na.print = TRUE)

parseClassFun(
  tab,
  # only one line for a binary variable, and choose which one
  levels_to_keep = list("treated" = "yes"),
  # regroup rows under labels, in this order
  group_rows_labels = list("Petal" = c("Petal.Length", "Petal.Width"),
                           "Sepal" = c("Sepal.Length", "Sepal.Width")),
  # column order
  col.order = c("Total", "setosa", "versicolor", "virginica"),
  font = "Times New Roman"
)
```

Useful `descTab()` arguments:

| Argument | Effect |
|----|----|
| `group` | the factor defining the columns; omit it for a single Total column |
| `na.print` | add a “Missing values” row under each variable |
| `pvalue` | drop the p value column |
| `quanti`, `quali` | restrict the table to one kind of variable |
| `digits.p`, `digits.qt`, `digits.ql` | rounding for p values, mean/SD, proportions |
| `parallel`, `mc.cores` | compute the analyses with `parallel::mclapply()` |

## Notes on missing and degenerate data

- Rows with a missing value in `group` are dropped, with a warning.
- Unused levels of `group` (typically left behind by a subset) are
  dropped, with a warning, rather than producing an empty column.
- When a test cannot be computed — a constant variable, an empty level,
  too few observations — the p value is `NA` and a warning names the
  variable. The rest of the table is still produced.

## Bugs

Please report them at <https://github.com/tiago972/doudpackage/issues>.

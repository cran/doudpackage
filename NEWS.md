# doudpackage 2.2.0

## Bug fixes

* A statistical test that cannot be computed no longer aborts the whole table.
  The p value is `NA`, a warning names the variable, and the rest of the table
  is produced. This used to fail with `non-numeric argument to mathematical
  function` for a constant variable, a factor with a single or an empty level,
  an all-`NA` column, a quantitative variable with a single observation, or a
  group with a single level.
* Unused levels of the group variable — typically left behind by a subset of the
  data — are now dropped with a warning instead of producing a whole column of
  `NaN (NA)`.
* `group_rows_labels` now reorders the rows so that the variables of a label are
  contiguous and the labels follow the order they were given in. Row spans no
  longer overlap.
* `group_rows_labels` now matches whole variable names: a label on `age` no
  longer drags in the rows of `age_group`.
* `group_rows_labels` referring to a variable that has no row in the table used
  to silently empty the whole table; it is now an error naming the variables
  available.
* `parseClassFun()` failed with `levels_to_keep` because `filter()` resolved to
  `stats::filter`.
* `digits.ql` was ignored for the Total column of the missing values of a
  quantitative variable.
* A `tibble` is accepted, as the documentation already claimed.
* `["table"]` is always a `data.frame`; without a group it used to be a tibble.
* `group = NULL` now means the same as no group.
* `quali = FALSE` together with `quanti = FALSE` is reported as such instead of
  failing with `object 'lst_VarGroup.quanti' not found`.
* `col.order` no longer fails when the table has no group.
* Variables of an unhandled type (character, `Date`, logical...) raise a warning
  and are ignored instead of throwing an error.
* Ordered factors are handled as factors.
* Rows with a missing value in the group variable are removed, with a warning.
* Bug correction when parsing the table if only the group variable had two levels.
* The row names of `["table"]` are no longer the leftovers of intermediate filtering.
* Argument checking gives an explicit message for an unknown group variable, a
  group that is not a factor, a group with fewer than two non empty levels, an
  empty dataset, and duplicated columns in `col.order`.

## New features

* `normality = "assess"` is implemented: a Shapiro-Wilk test decides variable by
  variable between mean (SD) with a parametric test, and median (IQR) with a non
  parametric one.
* `anaBiv()` on a `data.frame` works; it accepts `normality` and `digits.p`. It
  previously failed for every input.
* `[` on a `VarGroup` (the elements returned by `anaBiv()`) also gives access to
  `"name"`, `"type"` and `"normal"`.
* `parseClassFun()` takes a `font` argument, instead of always using arial.
* Parallel processing added as an option (`parallel`, `mc.cores`).

## Breaking changes

* `normality = "manual"` is no longer accepted. It has never done anything but
  raise an error; use `"assess"`, `"normal"` or `"non normal"`.

## Other

* A `testthat` suite covers the analyses, the rendering and every bug above.
* The package no longer imports `stringi`.

# doudpackage 2.0.1
* CRAN availability

## Major changes
* Added more than 2 sub-groups
* Choice between normal and non normal variables to use either t test/Wilcoxon or ANOVA/Kruskal-Wallis

## Minor changes
* Faster implementation (no loop used)

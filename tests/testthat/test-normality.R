test_that("normality = 'normal' gives mean (SD), 'non normal' gives median (IQR)", {
  expect_true(any(grepl("mean (SD)", descTab(iris, group = "Species")["table"]$var,
                        fixed = TRUE)))
  expect_true(any(grepl("median (IQR)",
                        descTab(iris, group = "Species",
                                normality = "non normal")["table"]$var, fixed = TRUE)))
})

test_that("normality = 'assess' decides per variable", {
  set.seed(42)
  d <- data.frame(g = factor(rep(c("a", "b"), each = 100)),
                  gaussian = rnorm(200),
                  skewed = rexp(200, 1))
  res <- descTab(d, group = "g", normality = "assess")
  expect_true(any(grepl("gaussian mean (SD)", res["table"]$var, fixed = TRUE)))
  expect_true(any(grepl("skewed median (IQR)", res["table"]$var, fixed = TRUE)))
})

test_that("anaBiv works on a data.frame", {
  res <- anaBiv(iris, group = "Species", parallel = FALSE)
  expect_type(res, "list")
  expect_s4_class(res[[1]], "VarGroup")
  expect_true(all(vapply(res, function(x) x@name != "Species", logical(1))))
})

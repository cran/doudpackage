# Every one of these used to abort with
# "non-numeric argument to mathematical function"
test_that("a test that cannot be computed gives NA, not an error", {
  d <- iris
  d$oneLevel <- factor(rep("a", 150))
  expect_warning(res <- descTab(d, group = "Species"), "No p value")
  expect_true(is.na(res["table"]$pvalue[res["table"]$var == "oneLevel, a"]))
})

test_that("a factor with an empty level does not abort", {
  d <- iris
  d$emptyLvl <- factor(rep("a", 150), levels = c("a", "b"))
  expect_warning(res <- descTab(d, group = "Species"), "No p value")
  expect_s4_class(res, "parseClass")
})

test_that("an all-NA column does not abort", {
  d <- iris
  d$allNA <- NA_real_
  expect_warning(res <- descTab(d, group = "Species"), "No p value")
  expect_s4_class(res, "parseClass")
})

test_that("a constant numeric column does not abort", {
  d <- iris
  d$constant <- 5
  expect_s4_class(suppressWarnings(descTab(d, group = "Species")), "parseClass")
})

test_that("a quantitative variable with a single observation does not abort", {
  d <- iris
  d$Sepal.Length[1:149] <- NA
  expect_s4_class(suppressWarnings(descTab(d, group = "Species")), "parseClass")
})

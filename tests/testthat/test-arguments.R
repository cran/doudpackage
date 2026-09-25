test_that("arguments are checked with a readable message", {
  d <- testData()
  expect_error(descTab(d, group = "PasLa"), "group is not a variable of data")
  expect_error(descTab(d, group = "Sepal.Length"), "group needs to be a factor")
  expect_error(descTab(d, group = NA), "group must be a single variable name")
  expect_error(descTab(d[d$Species == "setosa", ], group = "Species"),
               "at least two non empty levels")
  expect_error(descTab(d, group = "Species", quali = FALSE, quanti = FALSE),
               "cannot both be FALSE")
  expect_error(descTab(d, group = "Species", normality = "manual"),
               "normality must be one of")
  expect_error(descTab(data.frame()), "data is empty")
})

test_that("quali and quanti restrict the table", {
  d <- testData()
  quanti_only <- descTab(d, group = "Species", quali = FALSE)
  expect_false(any(grepl(", ", quanti_only["table"]$var, fixed = TRUE)))
  quali_only <- descTab(d, group = "Species", quanti = FALSE)
  expect_false(any(grepl("mean (SD)", quali_only["table"]$var, fixed = TRUE)))
})

test_that("digits.ql applies to the Total column too", {
  d <- iris
  d$num <- runif(150, 0, 100)
  d$num[1:13] <- NA                       # 13 / 150 = 8.6667 %
  res <- descTab(d, group = "Species", na.print = TRUE, digits.ql = 3)
  total <- res["table"]$Total[res["table"]$var == "num.Missing values"]
  expect_equal(total, "13 (8.667)")
})

test_that("digits.qt applies to mean and sd", {
  res <- descTab(iris, group = "Species", digits.qt = 3)
  expect_match(res["table"]$Total[res["table"]$var == "Sepal.Length mean (SD)"],
               "^5\\.843 \\(0\\.828\\)$")
})

test_that("pvalue = FALSE removes the column", {
  res <- descTab(testData(), group = "Species", pvalue = FALSE)
  expect_false("pvalue" %in% colnames(res["table"]))
})

test_that("na.print controls the missing value rows", {
  expect_false(any(grepl("Missing values",
                         descTab(testData(), group = "Species")["table"]$var)))
  expect_true(any(grepl("Missing values",
                        descTab(testData(), group = "Species", na.print = TRUE)["table"]$var)))
})

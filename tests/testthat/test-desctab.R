test_that("descTab describes numeric and factor variables", {
  res <- descTab(testData(), group = "Species")
  expect_s4_class(res, "parseClass")
  expect_true(all(c("var", "setosa", "versicolor", "virginica", "Total", "pvalue")
                  %in% colnames(res["table"])))
  expect_true(any(grepl("Sepal.Length mean (SD)", res["table"]$var, fixed = TRUE)))
  expect_true(any(res["table"]$var == "bin, 1"))
})

test_that("descTab works without a group", {
  res <- descTab(testData())
  expect_s4_class(res, "parseClass")
  expect_equal(colnames(res["table"]), c("var", "Total"))
})

test_that("group = NULL is the same as no group", {
  expect_equal(descTab(testData(), group = NULL)["table"],
               descTab(testData())["table"])
})

test_that("a tibble is accepted", {
  expect_s4_class(descTab(tibble::as_tibble(iris), group = "Species"), "parseClass")
})

test_that("the table is a plain data.frame, with or without a group", {
  for (res in list(descTab(testData()), descTab(testData(), group = "Species"),
                   descTab(tibble::as_tibble(testData())),
                   descTab(testData(), group = "Species", quali = FALSE)))
    expect_identical(class(res["table"]), "data.frame")
})

test_that("unused levels of the group are dropped instead of yielding a NaN column", {
  sub <- iris[iris$Species != "setosa", ]
  expect_warning(res <- descTab(sub, group = "Species"), "Unused level")
  expect_false("setosa" %in% colnames(res["table"]))
  expect_false(any(grepl("NaN", unlist(res["table"]))))
})

test_that("rows with a missing group value are dropped with a warning", {
  d <- iris
  d$Species[1:5] <- NA
  expect_warning(descTab(d, group = "Species"), "5 rows have been deleted")
})

test_that("variables of an unhandled type warn instead of aborting", {
  d <- iris
  d$chr <- "a"
  d$date <- Sys.Date()
  expect_warning(res <- descTab(d, group = "Species"), "Type unrecognised")
  expect_false(any(grepl("chr|date", res["table"]$var)))
})

test_that("ordered factors are handled as factors", {
  d <- iris
  d$ord <- factor(sample(1:3, 150, TRUE), ordered = TRUE)
  expect_silent(res <- descTab(d, group = "Species"))
  expect_true(any(grepl("^ord, ", res["table"]$var)))
})

test_that("a data set with no usable variable is an error", {
  expect_error(suppressWarnings(descTab(data.frame(a = letters[1:5]))),
               "No variable of a handled type")
})

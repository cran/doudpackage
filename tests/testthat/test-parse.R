test_that("parseClassFun returns a kable", {
  res <- parseClassFun(descTab(testData(), group = "Species"))
  expect_s3_class(res, "knitr_kable")
})

test_that("levels_to_keep picks the level of a binary variable", {
  tab <- descTab(testData(), group = "Species")
  expect_true(any(doudpackage:::parseQuali(tab, NULL)$var == "bin, 1"))
  expect_true(any(doudpackage:::parseQuali(tab, list("bin" = "0"))$var == "bin, 0"))
  expect_error(parseClassFun(tab, levels_to_keep = list("bin" = "9")),
               "not a valid level")
})

test_that("col.order reorders the columns, with or without a group", {
  tab <- descTab(testData(), group = "Species")
  expect_s3_class(parseClassFun(tab, col.order = c("Total", "setosa", "versicolor",
                                                   "virginica")), "knitr_kable")
  expect_error(parseClassFun(tab, col.order = c("setosa")),
               "needs to contain all levels of group")
  expect_error(parseClassFun(tab, col.order = c("Total", "Total", "setosa",
                                                "versicolor", "virginica")),
               "duplicated columns")
  # used to fail with "undefined columns selected"
  expect_s3_class(parseClassFun(descTab(testData()), col.order = c("Total")),
                  "knitr_kable")
})

test_that("group_rows_labels follows the order given by the user", {
  tab <- descTab(testData(), group = "Species")
  ordered <- doudpackage:::orderRowForGroupLabels(
    tab, list("B" = c("Sepal.Length", "num"), "A" = c("Petal.Length", "age_group")))
  expect_equal(ordered$table$var[1:2],
               c("Sepal.Length mean (SD)", "num mean (SD)"))
  expect_equal(unname(ordered$spans[, "B"]), c(1, 2))
  expect_equal(unname(ordered$spans[, "A"]), c(3, 5))
})

test_that("group_rows_labels spans never overlap", {
  tab <- descTab(testData(), group = "Species")
  spans <- doudpackage:::orderRowForGroupLabels(
    tab, list("B" = c("Sepal.Length", "num"), "A" = c("Petal.Length", "bin")))$spans
  starts <- spans[1, ]; ends <- spans[2, ]
  expect_true(all(starts <= ends))
  expect_true(all(diff(c(rbind(starts, ends))) > 0))
})

test_that("group_rows_labels matches whole variable names only", {
  # "age" used to drag the rows of "age_group" in with it
  tab <- descTab(testData(), group = "Species")
  ordered <- doudpackage:::orderRowForGroupLabels(tab, list("Age" = c("age")))
  expect_equal(ordered$table$var[1], "age mean (SD)")
  expect_equal(unname(ordered$spans[, "Age"]), c(1, 1))
})

test_that("group_rows_labels on an undescribed variable is an error, not an empty table", {
  tab <- descTab(testData(), group = "Species")
  expect_error(parseClassFun(tab, group_rows_labels = list("X" = c("Species"))),
               "not described in the table")
  expect_error(parseClassFun(tab, group_rows_labels = list("X" = c("nimporte"))),
               "not described in the table")
  expect_error(parseClassFun(tab, group_rows_labels = list(c("num"))),
               "needs to be a named list")
  expect_error(parseClassFun(tab, group_rows_labels = list("A" = "num", "B" = "num")),
               "same variable to several labels")
})

test_that("group_rows_labels never drops rows", {
  tab <- descTab(testData(), group = "Species")
  n <- nrow(doudpackage:::parseQuali(tab, NULL))
  ordered <- doudpackage:::orderRowForGroupLabels(tab, list("A" = c("num")))
  expect_equal(nrow(ordered$table), nrow(tab["table"]))
  expect_setequal(ordered$table$var, tab["table"]$var)
})

test_that("na.print and group_rows_labels work together", {
  tab <- descTab(testData(), group = "Species", na.print = TRUE)
  expect_s3_class(parseClassFun(tab, group_rows_labels = list("S" = c("Sepal.Length",
                                                                     "Sepal.Width"))),
                  "knitr_kable")
})

test_that("font is an option", {
  tab <- descTab(iris, group = "Species")
  expect_match(as.character(parseClassFun(tab, font = "Times New Roman")),
               "Times New Roman")
})

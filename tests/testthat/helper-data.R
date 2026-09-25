# A data set exercising every kind of variable the package meets
testData <- function(seed = 1) {
  set.seed(seed)
  d <- iris
  d$bin <- factor(sample(c("0", "1"), 150, TRUE))
  d$fact3 <- factor(sample(c("a", "b", "c"), 150, TRUE))
  d$age <- runif(150, 0, 100)
  d$age_group <- factor(sample(c("x", "y"), 150, TRUE))
  d$num <- runif(150, 0, 100)
  d$num[sample(150, 13)] <- NA
  d
}

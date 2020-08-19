
context("inspect_data_binomial")

test_that("inspect_data_binomial silent 1", {
  expect_silent(inspect_data_binomial(c(1, 0, 0, 1, 0), success = 1))
})

test_that("inspect_data_binomial silent 2", {
  x1 <- c(1, 0, 0, 1, 0)
  expect_silent(inspect_data_binomial(x1, success = 1))
})

test_that("inspect_data_binomial silent 3", {
  x2 <- c(FALSE, FALSE, TRUE)
  expect_silent(inspect_data_binomial(x2, success = TRUE))
})

test_that("inspect_data_binomial silent 4", {
  x3 <- c("yes", "no", "yes")
  expect_silent(inspect_data_binomial(x3, success = "yes"))
})

test_that("inspect_data_binomial silent 5", {
  x4 <- factor(c("yes", "no", "yes"))
  expect_silent(inspect_data_binomial(x4, success = "yes"))
})

test_that("inspect_data_binomial silent 6", {
  x5 <- c(1, 0, 0, 1, 0, NA)
  expect_silent(inspect_data_binomial(x5, success = 1))
})

test_that("inspect_data_binomial silent 7", {
  x5 <- c(1, 0, 0, 1, 0, NA)
  expect_silent(inspect_data_binomial(x5, success = 1, warning_nas = FALSE))
})

test_that("inspect_data_binomial warning 1", {
  y1 <- c(1, 1, NA, 0, 0)
  expect_warning(
    inspect_data_binomial(y1, success = 1, warning_nas = TRUE),
    "There are NA or NaN values in 'y1'."
  )
})

test_that("inspect_data_binomial warning 2", {
  y2 <- c(0, 0)
  expect_warning(inspect_data_binomial(y2, success = 1),
                 "'1' not observed in 'y2'")
})

test_that("inspect_data_binomial error 1", {
  expect_error(inspect_data_binomial(NULL, 1),
                 "'NULL' is NULL.")
})

test_that("inspect_data_binomial error 2", {
  expect_error(inspect_data_binomial(c(1,0), NULL),
               "'NULL' is NULL.")
})

test_that("inspect_data_binomial error 3", {
  x <- list(1,0)
  expect_error(inspect_data_binomial(x, 1),
               "Invalid argument: 'x' must be atomic.")
})

test_that("inspect_data_binomial error 4", {
  x <- list(1)
  expect_error(inspect_data_binomial(c(1,0), x),
               "Invalid argument: 'x' must be atomic.")
})

test_that("inspect_data_binomial error 5", {
  x <- numeric(0)
  expect_error(inspect_data_binomial(x, 0),
               "Invalid argument: 'x' is empty.")
})

test_that("inspect_data_binomial error 6", {
  x <- numeric(0)
  expect_error(inspect_data_binomial(1, x),
               "Invalid argument: 'x' must be atomic and have length 1.")
})

test_that("inspect_data_binomial error 7", {
  expect_error(inspect_data_binomial(NaN, 1),
               "Invalid argument: all elements of 'NaN' are NA or NaN.")
})

test_that("inspect_data_binomial error 8", {
  expect_error(inspect_data_binomial(NA, 1),
               "Invalid argument: all elements of 'NA' are NA or NaN.")
})

test_that("inspect_data_binomial error 9", {
  expect_error(inspect_data_binomial(c(1,0), NA),
               "Invalid argument: 'NA' is NA or NaN")
})

test_that("inspect_data_binomial error 10", {
  expect_error(inspect_data_binomial(c(1,0), NaN),
               "Invalid argument: 'NaN' is NA or NaN")
})

test_that("inspect_data_binomial error 11", {
  expect_error(inspect_data_binomial(c(1,0), 2),
               "nvalid argument: there are more than two levels'")
})

context("inspect_data_multinomial")

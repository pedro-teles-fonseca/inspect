
context("inspect_data_dichotomous")

test_that("inspect_data_dichotomous silent 1", {
  expect_silent(inspect_data_dichotomous(c(1, 0, 0, 1, 0), success = 1))
})

test_that("inspect_data_dichotomous silent 2", {
  x <- c(1, 0, 0, 1, 0)
  expect_silent(inspect_data_dichotomous(x, success = 1))
})

test_that("inspect_data_dichotomous silent 3", {
  x <- c(FALSE, FALSE, TRUE)
  expect_silent(inspect_data_dichotomous(x, success = TRUE))
})

test_that("inspect_data_dichotomous silent 4", {
  x <- c("yes", "no", "yes")
  expect_silent(inspect_data_dichotomous(x, success = "yes"))
})

test_that("inspect_data_dichotomous silent 5", {
  x <- factor(c("yes", "no", "yes"))
  expect_silent(inspect_data_dichotomous(x, success = "yes"))
})

test_that("inspect_data_dichotomous silent 6", {
  x <- c(1, 0, 0, 1, 0, NA)
  expect_silent(inspect_data_dichotomous(x, success = 1))
})

test_that("inspect_data_dichotomous silent 7", {
  x <- c(1, 0, 0, 1, 0, NA)
  expect_silent(inspect_data_dichotomous(x, success = 1, warning_nas = FALSE))
})

test_that("inspect_data_dichotomous warning 1", {
  x <- c(1, 1, NA, 0, 0)
  expect_warning(
    inspect_data_dichotomous(x, success = 1, warning_nas = TRUE),
    "There are NA or NaN values in 'x'."
  )
})

test_that("inspect_data_dichotomous warning 2", {
  x <- c(0, 0)
  expect_warning(inspect_data_dichotomous(x, success = 1),
                 "'1' not observed in 'x'")
})

test_that("inspect_data_dichotomous error 1", {
  expect_error(inspect_data_dichotomous(NULL, 1),
                 "'NULL' is NULL.")
})

test_that("inspect_data_dichotomous error 2", {
  expect_error(inspect_data_dichotomous(c(1,0), NULL),
               "'NULL' is NULL.")
})

test_that("inspect_data_dichotomous error 3", {
  x <- list(1,0)
  expect_error(inspect_data_dichotomous(x, 1),
               "Invalid argument: 'x' must be atomic.")
})

test_that("inspect_data_dichotomous error 4", {
  x <- list(1)
  expect_error(inspect_data_dichotomous(c(1,0), x),
               "Invalid argument: 'x' must be atomic.")
})

test_that("inspect_data_dichotomous error 5", {
  x <- numeric(0)
  expect_error(inspect_data_dichotomous(x, 0),
               "Invalid argument: 'x' is empty.")
})

test_that("inspect_data_dichotomous error 6", {
  x <- numeric(0)
  expect_error(inspect_data_dichotomous(1, x),
               "Invalid argument: 'x' must be atomic and have length 1.")
})

test_that("inspect_data_dichotomous error 7", {
  expect_error(inspect_data_dichotomous(NaN, 1),
               "Invalid argument: all elements of 'NaN' are NA or NaN.")
})

test_that("inspect_data_dichotomous error 8", {
  expect_error(inspect_data_dichotomous(NA, 1),
               "Invalid argument: all elements of 'NA' are NA or NaN.")
})

test_that("inspect_data_dichotomous error 9", {
  expect_error(inspect_data_dichotomous(c(1,0), NA),
               "Invalid argument: 'NA' is NA or NaN")
})

test_that("inspect_data_dichotomous error 10", {
  expect_error(inspect_data_dichotomous(c(1,0), NaN),
               "Invalid argument: 'NaN' is NA or NaN")
})

test_that("inspect_data_dichotomous error 11", {
  expect_error(inspect_data_dichotomous(c(1,0), 2),
               "nvalid argument: there are more than two levels'")
})

test_that("inspect_data_dichotomous error 12", {
  x <- complex(1)
  expect_error(inspect_data_dichotomous(x, 1),
               "Invalid argument: the type of 'x' must be 'logical', 'integer', 'double' or 'character'.")
})

test_that("inspect_data_dichotomous error 13", {
  x <- complex(1)
  expect_error(inspect_data_dichotomous(c(1,0), x),
               "Invalid argument: the type of 'x' must be 'logical', 'integer', 'double' or 'character'.")
})

context("inspect_data_dichotomous 2")

test_that("inspect_data_dichotomous a", {
  x <- c(1, 0, 0, 1, 0)
  expect_silent(inspect_data_dichotomous(x, success = 1))
})

test_that("inspect_data_dichotomous b", {
  x <- c(FALSE, FALSE, TRUE)
  expect_silent(inspect_data_dichotomous(x, success = TRUE))
})

test_that("inspect_data_dichotomous c", {
  x <- c("yes", "no", "yes")
  expect_silent(inspect_data_dichotomous(x, success = "yes"))
})

test_that("inspect_data_dichotomous d", {
  x <- factor(c("yes", "no", "yes"))
  expect_silent(inspect_data_dichotomous(x, success = "yes"))
})

test_that("inspect_data_dichotomous e", {
  x <- c(1, 0, 0, 1, 0, NA)
  expect_silent(inspect_data_dichotomous(x, success = 1))
})

test_that("inspect_data_dichotomous f", {
  y <- c(1, 1, NA, 0, 0)
  expect_warning(inspect_data_dichotomous(y, success = 1, warning_nas = TRUE),
                 "There are NA or NaN values in 'y'")
})

test_that("inspect_data_dichotomous g", {
  y <- c(0, 0)
  success <- 1
  expect_warning(inspect_data_dichotomous(y, success = success),
                 "'success' not observed in 'y'")
})

test_that("inspect_data_dichotomous h", {
  expect_error(inspect_data_dichotomous(NULL, 1),
               "Invalid argument: 'NULL' is NULL.")
})

test_that("inspect_data_dichotomous i", {
  expect_error(inspect_data_dichotomous(c(1,0), NULL),
               "Invalid argument: 'NULL' is NULL.")
})

test_that("inspect_data_dichotomous j", {
  x <- list(1,0)
  expect_error(inspect_data_dichotomous(x, 1),
               "Invalid argument: 'x' must be atomic.")
})

test_that("inspect_data_dichotomous k", {
  x <- list(1)
  expect_error(inspect_data_dichotomous(c(1,0),x),
               "Invalid argument: 'x' must be atomic and have length 1.")
})

test_that("inspect_data_dichotomous l", {
  x <- numeric(0)
  expect_error(inspect_data_dichotomous(x, 0),
               "Invalid argument: 'x' is empty.")
})

test_that("inspect_data_dichotomous m", {
  x <- numeric(0)
  expect_error(inspect_data_dichotomous(1, x),
               "Invalid argument: 'x' must be atomic and have length 1.")
})

test_that("inspect_data_dichotomous n", {
  expect_error(inspect_data_dichotomous(NaN, 1),
               "Invalid argument: all elements of 'NaN' are NA or NaN.")
})

test_that("inspect_data_dichotomous o", {
  expect_error(inspect_data_dichotomous(c(1,0), NA),
               "Invalid argument: 'NA' is NA or NaN")
})

test_that("inspect_data_dichotomous p", {
  expect_error(inspect_data_dichotomous(c(1,0), NaN),
               "Invalid argument: 'NaN' is NA or NaN")
})

test_that("inspect_data_dichotomous q", {
  expect_error(inspect_data_dichotomous(c(1,0), 2),
               "Invalid argument: there are more than two levels'.")
})

context("inspect_data_categorical")

test_that("inspect_data_categorical silent 1", {
  expect_silent(inspect_data_categorical(c(1:3, 1:3)))
})

test_that("inspect_data_categorical silent 2", {
  x <- c(1:3, 1:3)
  expect_silent(inspect_data_categorical(x))
})

test_that("inspect_data_categorical silent 3", {
  x <- c(FALSE, FALSE, TRUE)
  expect_silent(inspect_data_categorical(x))
})

test_that("inspect_data_categorical silent 4", {
  x <- c("yes", "no", "yes", "kind-of")
  expect_silent(inspect_data_categorical(x))
})

test_that("inspect_data_categorical silent 5", {
  x <- factor(c("yes", "no", "yes", "maybe"))
  expect_silent(inspect_data_categorical(x))
})

test_that("inspect_data_categorical silent 6", {
  x <- c(1, 0, 0, 1, 0, NA, 2)
  expect_silent(inspect_data_categorical(x))
})

test_that("inspect_data_categorical silent 7", {
  x <- c(1, 0, 0, 1, 0, NA, 2)
  expect_silent(inspect_data_categorical(x, warning_nas = FALSE))
})

test_that("inspect_data_categorical warning 1", {
  x <- c(1, 1, NA, 0, 0, 2)
  expect_warning(
    inspect_data_categorical(x, warning_nas = TRUE),
    "There are NA or NaN values in 'x'."
  )
})

test_that("inspect_data_categorical error 1", {
  expect_error(inspect_data_categorical(NULL),
               "'NULL' is NULL.")
})

test_that("inspect_data_categorical error 2", {
  x <- list(1,0)
  expect_error(inspect_data_categorical(x),
               "Invalid argument: 'x' must be atomic.")
})

test_that("inspect_data_categorical error 3", {
  x <- numeric(0)
  expect_error(inspect_data_categorical(x),
               "Invalid argument: 'x' is empty.")
})

test_that("inspect_data_categorical error 4", {
  expect_error(inspect_data_categorical(NaN),
               "Invalid argument: all elements of 'NaN' are NA or NaN.")
})

test_that("inspect_data_categorical error 5", {
  expect_error(inspect_data_categorical(NA),
               "Invalid argument: all elements of 'NA' are NA or NaN.")
})

test_that("inspect_data_categorical error 6", {
  x <- complex(1)
  expect_error(inspect_data_categorical(x),
               "Invalid argument: the type of 'x' must be 'logical', 'integer', 'double' or 'character'.")
})

context("inspect_data_multinom_as_bern")

test_that("inspect_data_multinom_as_bern silent 1", {
  x <- c(1, 0, 0, 1, 0)
  expect_silent(inspect_data_dichotomous(x, success = 1))
})

test_that("inspect_data_multinom_as_bern silent 2", {
  x <- c(FALSE, FALSE, TRUE)
  expect_silent(inspect_data_dichotomous(x, success = TRUE))
})

test_that("inspect_data_multinom_as_bern silent 3", {
  x <- c("yes", "no", "yes")
  expect_silent(inspect_data_dichotomous(x, success = "yes"))
})

test_that("inspect_data_multinom_as_bern silent 4", {
  x <- factor(c("yes", "no", "yes"))
  expect_silent(inspect_data_dichotomous(x, success = "yes"))
})

test_that("inspect_data_multinom_as_bern silent 4", {
  x <- c(1, 0, 0, 1, 0, NA)
  expect_silent(inspect_data_dichotomous(x, success = 1))
})

test_that("inspect_data_multinom_as_bern warning 1", {
  x <- c(1, 1, NA, 0, 0)
  expect_warning(inspect_data_multinom_as_bern(x, success = 1, warning_nas = TRUE),
                 "There are NA or NaN values in 'x'.")
})

test_that("inspect_data_multinom_as_bern warning 2", {
  x <- c(0, 0)
  success <- 1
  expect_warning(inspect_data_multinom_as_bern(x, success = success),
                 "'success' not observed in 'x'.")
})

test_that("inspect_data_multinom_as_bern warning 3", {
  x <- c(1, 0)
  expect_warning(inspect_data_multinom_as_bern(x, 2),
               "'2' not observed in 'x'.")
})

test_that("inspect_data_multinom_as_bern error 1", {
  x <- c(1, 1, NA, 0, 0)
  expect_error(inspect_data_multinom_as_bern(x, 1, allow_nas = FALSE),
                 "There are NA or NaN values in  'x'.")
})

test_that("inspect_data_multinom_as_bern error 2", {
  expect_error(inspect_data_multinom_as_bern(NULL, 1),
               "'NULL' is NULL.")
})

test_that("inspect_data_multinom_as_bern error 3", {
  expect_error(inspect_data_multinom_as_bern(c(1,0), NULL),
               "'NULL' is NULL.")
})

test_that("inspect_data_multinom_as_bern error 4", {
  x <- list(1, 0)
  expect_error(inspect_data_multinom_as_bern(x, 1),
               "Invalid argument: 'x' must be atomic.")
})

test_that("inspect_data_multinom_as_bern error 5", {
  x <- list(1)
  expect_error(inspect_data_multinom_as_bern(c(1,0), x),
               "Invalid argument: 'x' must be atomic.")
})

test_that("inspect_data_multinom_as_bern error 6", {
  x <- numeric(0)
  expect_error(inspect_data_multinom_as_bern(x, 0),
               "Invalid argument: 'x' is empty.")
})

test_that("inspect_data_multinom_as_bern error 7", {
  x <- numeric(0)
  expect_error(inspect_data_multinom_as_bern(1, x),
               "Invalid argument: 'x' must be atomic and have length 1.")
})

test_that("inspect_data_multinom_as_bern error 8", {
  expect_error(inspect_data_multinom_as_bern(NaN, 1),
               "Invalid argument: all elements of 'NaN' are NA or NaN.")
})

test_that("inspect_data_multinom_as_bern error 9", {
  expect_error(inspect_data_multinom_as_bern(NA, 1),
               "Invalid argument: all elements of 'NA' are NA or NaN.")
})

test_that("inspect_data_multinom_as_bern error 10", {
  expect_error(inspect_data_multinom_as_bern(c(1,0), NA),
               "Invalid argument: 'NA' is NA or NaN.")
})

test_that("inspect_data_multinom_as_bern error 11", {
  expect_error(inspect_data_multinom_as_bern(c(1,0), NaN),
               "Invalid argument: 'NaN' is NA or NaN.")
})




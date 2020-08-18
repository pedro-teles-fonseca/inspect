

context("inspect_prob")

test_that("inspect_prob 1", {
  expect_silent(inspect_bf(0))
})

test_that("inspect_prob 2", {
  expect_silent(inspect_bf(0.5))
})

test_that("inspect_prob 3", {
  expect_silent(inspect_bf(100))
})

test_that("inspect_prob 4", {
  expect_silent(inspect_bf(c(0, 0.5, 1, 10, 50, 100)))
})

test_that("inspect_prob 5", {
  expect_error(inspect_prob(NULL),
               "Invalid argument: 'NULL' is NULL.")
})

test_that("inspect_prob 6", {
  x <- NULL
  expect_error(inspect_prob(x),
               "Invalid argument: 'x' is NULL.")
})

test_that("inspect_prob 7", {
  x <- factor(1)
  expect_error(inspect_prob(x),
               "Invalid argument: 'x' must be an atomic vector.")
})

test_that("inspect_prob 8", {
  x <- list(1)
  expect_error(inspect_prob(x),
               "Invalid argument: 'x' must be an atomic vector.")
})

test_that("inspect_prob 9", {
  x <- numeric(0)
  expect_error(inspect_prob(x),
               "Invalid argument: 'x' is empty.")
})

test_that("inspect_prob 10", {
  x <- NA
  expect_error(inspect_prob(x),
               "Invalid argument: all elements of 'x' are NA or NaN.")
})

test_that("inspect_prob 11", {
  x <- NaN
  expect_error(inspect_prob(x),
               "Invalid argument: all elements of 'x' are NA or NaN.")
})

test_that("inspect_prob 12", {
  x <- c(NA_real_, NA_real_, NA_real_)
  expect_error(inspect_prob(x),
               "Invalid argument: all elements of 'x' are NA or NaN.")
})

test_that("inspect_prob 13", {
  x <- "1"
  expect_error(inspect_prob(x),
               "Invalid argument: the type of 'x' must be numeric")
})

test_that("inspect_prob 14", {
  x <- -1
  expect_error(
    inspect_prob(x),
    "Invalid argument: all elements of 'x' must be in the \\[0, 1\\] interval."
  )
})

test_that("inspect_prob 15", {
  x <- 1.1
  expect_error(
    inspect_prob(x),
    "Invalid argument: all elements of 'x' must be in the \\[0, 1\\] interval."
  )
})

test_that("inspect_prob 16", {
  expect_error(
    inspect_prob(
      c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5),
      "Invalid argument: all elements of 'x' must be in the \\[0, 1\\] interval."
    )
  )
})

test_that("inspect_prob 17", {
  expect_warning(inspect_prob(c(0.1, 0.2, NA, .4, 0.5), warning_nas = TRUE))
})

test_that("inspect_prob 18", {
  expect_error(inspect_prob(c(0.1, 0.2, NA, 0.4, 0.5), allow_nas = FALSE))
})

context("inspect_bf")

test_that("inspect_bf 1", {
  expect_error(inspect_bf(NULL),
               "Invalid argument: 'NULL' is NULL.")
})

test_that("inspect_bf 2", {
  x <- NULL
  expect_error(inspect_bf(x),
               "Invalid argument: 'x' is NULL.")
})

test_that("inspect_bf 3", {
  x <- factor(1)
  expect_error(inspect_bf(x),
               "Invalid argument: 'x' must be an atomic vector.")
})

test_that("inspect_bf 4", {
  x <- list(1)
  expect_error(inspect_bf(x),
               "Invalid argument: 'x' must be an atomic vector.")
})

test_that("inspect_bf 5", {
  x <- numeric(0)
  expect_error(inspect_bf(x),
               "Invalid argument: 'x' is empty.")
})

test_that("inspect_bf 6", {
  x <- NA
  expect_error(inspect_bf(x),
               "Invalid argument: all elements of  'x' are NA or NaN.")
})

test_that("inspect_bf 7", {
  x <- NaN
  expect_error(inspect_bf(x),
               "Invalid argument: all elements of  'x' are NA or NaN.")
})

test_that("inspect_bf 8", {
  x <- c(NA_real_, NA_real_, NA_real_)
  expect_error(inspect_bf(x),
               "Invalid argument: all elements of  'x' are NA or NaN.")
})

test_that("inspect_bf 9", {
  x <- "1"
  expect_error(inspect_bf(x),
               "Invalid argument: the type of 'x' must be numeric")
})

test_that("inspect_bf 10", {
  expect_equal(inspect_bf(10),
               inspect_bf(2))
})

test_that("inspect_bf 11", {
  expect_equal(inspect_bf(exp(1)),
               inspect_bf(2))
})

test_that("inspect_bf 12", {
  expect_error(inspect_bf(-2))
})

test_that("inspect_bf 13", {
  expect_equal(inspect_bf(2),
               NULL)
})

test_that("inspect_bf 14", {
  expect_silent(inspect_bf(2))
})

test_that("inspect_bf 15", {
  expect_error(inspect_bf(c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)))
})

test_that("inspect_bf 16", {
  expect_warning(inspect_bf(c(0.1, 2, NA, 40, 0.5)))
})

test_that("inspect_bf 17", {
  expect_error(inspect_bf(c(0.1, 2, NA, 4, 0.5), allow_nas = FALSE))
})

test_that("inspect_bf 18", {
  expect_warning(inspect_bf(c(0.1, 2, NA, 4, 0.5), warning_nas = TRUE))
})

context("inspect_log_bf")

test_that("inspect_log_bf 1", {
  expect_error(inspect_log_bf(NULL),
               "Invalid argument: 'NULL' is NULL.")
})

test_that("inspect_log_bf 2", {
  x <- NULL
  expect_error(inspect_log_bf(x),
               "Invalid argument: 'x' is NULL.")
})

test_that("inspect_log_bf 3", {
  x <- factor(1)
  expect_error(inspect_log_bf(x),
               "Invalid argument: 'x' must be an atomic vector.")
})

test_that("inspect_log_bf 4", {
  x <- list(1)
  expect_error(inspect_log_bf(x),
               "Invalid argument: 'x' must be an atomic vector.")
})

test_that("inspect_log_bf 5", {
  x <- numeric(0)
  expect_error(inspect_log_bf(x),
               "Invalid argument: 'x' is empty.")
})

test_that("inspect_log_bf 6", {
  x <- NA
  expect_error(inspect_log_bf(x),
               "Invalid argument: all elements of  'x' are NA or NaN.")
})

test_that("inspect_log_bf 7", {
  x <- NaN
  expect_error(inspect_log_bf(x),
               "Invalid argument: all elements of  'x' are NA or NaN.")
})

test_that("inspect_log_bf 8", {
  x <- c(NA_real_, NA_real_, NA_real_)
  expect_error(inspect_log_bf(x),
               "Invalid argument: all elements of  'x' are NA or NaN.")
})

test_that("inspect_log_bf 9", {
  x <- "1"
  expect_error(inspect_log_bf(x),
               "Invalid argument: the type of 'x' must be numeric")
})

test_that("inspect_log_bf 10", {
  expect_equal(inspect_log_bf(10),
               inspect_log_bf(2))
})

test_that("inspect_log_bf 11", {
  expect_equal(inspect_log_bf(exp(1)),
               inspect_log_bf(2))
})

test_that("inspect_log_bf 12", {
  expect_equal(inspect_log_bf(10),
               inspect_log_bf(-10))
})

test_that("inspect_log_bf 13", {
  expect_equal(inspect_log_bf(2),
               NULL)
})

test_that("inspect_log_bf 14", {
  expect_silent(inspect_log_bf(2))
})

test_that("inspect_log_bf 15", {
  expect_warning(inspect_log_bf(c(0.1, 2, NA, 4, 0.5), warning_nas = TRUE))
})

test_that("inspect_log_bf 16", {
  expect_error(inspect_log_bf(c(0.1, 2, NA, 4, 0.5), allow_nas = FALSE))
})

context("inspect_log_base")

test_that("inspect_log_base 1", {
  expect_error(inspect_log_base(NULL),
               "Invalid argument: 'NULL' is NULL.")
})

test_that("inspect_log_base 2", {
  x <- NULL
  expect_error(inspect_log_base(x),
               "Invalid argument: 'x' is NULL.")
})

test_that("inspect_log_base 3", {
  x <- factor(1)
  expect_error(inspect_log_base(x),
               "Invalid argument: 'x' must be an atomic vector of length 1.")
})

test_that("inspect_log_base 4", {
  x <- list(1)
  expect_error(inspect_log_base(x),
               "Invalid argument: 'x' must be an atomic vector of length 1.")
})

test_that("inspect_log_base 5", {
  x <- numeric(0)
  expect_error(inspect_log_base(x),
               "Invalid argument: 'x' must be an atomic vector of length 1.")
})

test_that("inspect_log_base 6", {
  x <- c(10, 2)
  expect_error(inspect_log_base(x),
               "Invalid argument: 'x' must be an atomic vector of length 1.")
})

test_that("inspect_log_base 7", {
  x <- NA
  expect_error(inspect_log_base(x),
               "Invalid argument: 'x' is NA or NaN.")
})

test_that("inspect_log_base 8", {
  x <- NaN
  expect_error(inspect_log_base(x),
               "Invalid argument: 'x' is NA or NaN.")
})

test_that("inspect_log_base 9", {
  x <- "1"
  expect_error(inspect_log_base(x),
               "Invalid argument: the type of 'x' must be numeric")
})

test_that("inspect_log_base 10", {
  expect_equal(inspect_log_base(10),
               inspect_log_base(2))
})

test_that("inspect_log_base 11", {
  expect_equal(inspect_log_base(exp(1)),
               inspect_log_base(2))
})

test_that("inspect_log_base 12", {
  expect_equal(inspect_log_base(2),
               NULL)
})

test_that("inspect_log_base 13", {
  expect_silent(inspect_log_base(2))
})

test_that("inspect_log_base 14", {
  x <- -1
  expect_error(inspect_log_base(x),
               "Invalid argument: 'x' must be positive.")
})

context("inspect_scale")

test_that("inspect_scale 1", {
  expect_error(inspect_scale(NULL),
               "Invalid argument: 'NULL' is NULL.")
})

test_that("inspect_scale 2", {
  x <- NULL
  expect_error(inspect_scale(x),
               "Invalid argument: 'x' is NULL.")
})

test_that("inspect_scale 3", {
  x <- factor(1)
  expect_error(inspect_scale(x),
               "Invalid argument: 'x' must be an atomic vector of length 1.")
})

test_that("inspect_scale 4", {
  x <- list(1)
  expect_error(inspect_scale(x),
               "Invalid argument: 'x' must be an atomic vector of length 1.")
})

test_that("inspect_scale 5", {
  x <- character(0)
  expect_error(inspect_scale(x),
               "Invalid argument: 'x' must be an atomic vector of length 1.")
})

test_that("inspect_scale 6", {
  x <- c(TRUE, FALSE)
  expect_error(inspect_scale(x),
               "Invalid argument: 'x' must be an atomic vector of length 1.")
})

test_that("inspect_scale 7", {
  x <- NA
  expect_error(inspect_scale(x),
               "Invalid argument: 'x' is NA or NaN.")
})

test_that("inspect_scale 8", {
  x <- NaN
  expect_error(inspect_scale(x),
               "Invalid argument: 'x' is NA or NaN.")
})

test_that("inspect_scale 9", {
  x <- "TRUE"
  expect_error(inspect_scale(x),
               "Invalid argument: 'x' must be either 'jeffreys' or 'kass-raftery'")
})

test_that("inspect_scale 10", {
  expect_equal(inspect_scale("Jeffreys"),
               inspect_scale("Kass-Raftery"))
})

test_that("inspect_scale 11", {
  expect_equal(inspect_scale("Jeffreys"),
               inspect_scale("jeffreys"))
})

test_that("inspect_scale 12", {
  expect_equal(inspect_scale("Jeffreys"),
               NULL)
})

test_that("inspect_scale 13", {
  expect_silent(inspect_scale("Jeffreys"))
})

test_that("inspect_scale 13", {
  x <- 1
  expect_error(inspect_scale(x),
               "Invalid argument: the type of 'x' must be character.")
})

context("inspect_true_or_false")

test_that("inspect_true_or_false 1", {
  expect_error(inspect_true_or_false(NULL),
               "Invalid argument: 'NULL' is NULL.")
})

test_that("inspect_true_or_false 2", {
  x <- NULL
  expect_error(inspect_true_or_false(x),
               "Invalid argument: 'x' is NULL.")
})

test_that("inspect_true_or_false 3", {
  x <- factor(1)
  expect_error(inspect_true_or_false(x),
               "Invalid argument: 'x' must be an atomic vector of length 1.")
})

test_that("inspect_true_or_false 4", {
  x <- list(1)
  expect_error(inspect_true_or_false(x),
               "Invalid argument: 'x' must be an atomic vector of length 1.")
})

test_that("inspect_true_or_false 5", {
  x <- logical(0)
  expect_error(inspect_true_or_false(x),
               "Invalid argument: 'x' must be an atomic vector of length 1.")
})

test_that("inspect_true_or_false 6", {
  x <- c(TRUE, FALSE)
  expect_error(inspect_true_or_false(x),
               "Invalid argument: 'x' must be an atomic vector of length 1.")
})

test_that("inspect_true_or_false 7", {
  x <- NA
  expect_error(inspect_true_or_false(x),
               "Invalid argument: 'x' is NA or NaN.")
})

test_that("inspect_true_or_false 8", {
  x <- NaN
  expect_error(inspect_true_or_false(x),
               "Invalid argument: 'x' is NA or NaN.")
})

test_that("inspect_true_or_false 9", {
  x <- "TRUE"
  expect_error(inspect_true_or_false(x),
               "Invalid argument: the type of 'x' must be logical.")
})

test_that("inspect_true_or_false 10", {
  x <- "TRUE"
  expect_equal(inspect_true_or_false(TRUE),
               inspect_true_or_false(FALSE))
})

test_that("inspect_true_or_false 11", {
  x <- "TRUE"
  expect_equal(inspect_true_or_false(TRUE),
               NULL)
})

test_that("inspect_true_or_false 12", {
  x <- "TRUE"
  expect_silent(inspect_true_or_false(TRUE))
})

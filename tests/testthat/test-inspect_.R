
context("inspect_prob")

test_that("inspect_prob 1", {
  expect_silent(inspect_bfactor(0))
})

test_that("inspect_prob 2", {
  expect_silent(inspect_bfactor(0.5))
})

test_that("inspect_prob 3", {
  expect_silent(inspect_bfactor(100))
})

test_that("inspect_prob 4", {
  expect_silent(inspect_bfactor(c(0, 0.5, 1, 10, 50, 100)))
})

test_that("inspect_prob 5", {
  expect_error(
    inspect_prob(NULL),
    "Invalid argument: NULL is NULL."
  )
})

test_that("inspect_prob 6", {
  x <- NULL
  expect_error(
    inspect_prob(x),
    "Invalid argument: x is NULL."
  )
})

test_that("inspect_prob 7", {
  x <- factor(1)
  expect_error(
    inspect_prob(x),
    "Invalid argument: x must be an atomic vector."
  )
})

test_that("inspect_prob 8", {
  x <- list(1)
  expect_error(
    inspect_prob(x),
    "Invalid argument: x must be an atomic vector."
  )
})

test_that("inspect_prob 9", {
  x <- numeric(0)
  expect_error(
    inspect_prob(x),
    "Invalid argument: x is empty."
  )
})

test_that("inspect_prob 10", {
  x <- NA
  expect_error(
    inspect_prob(x),
    "Invalid argument: all elements of x are NA or NaN."
  )
})

test_that("inspect_prob 11", {
  x <- NaN
  expect_error(
    inspect_prob(x),
    "Invalid argument: all elements of x are NA or NaN."
  )
})

test_that("inspect_prob 12", {
  x <- c(NA_real_, NA_real_, NA_real_)
  expect_error(
    inspect_prob(x),
    "Invalid argument: all elements of x are NA or NaN."
  )
})

test_that("inspect_prob 13", {
  x <- "1"
  expect_error(
    inspect_prob(x),
    "Invalid argument: the type of x must be numeric"
  )
})

test_that("inspect_prob 14", {
  x <- -1
  expect_error(
    inspect_prob(x),
    "Invalid argument: all elements of x must be in the \\[0, 1\\] interval."
  )
})

test_that("inspect_prob 15", {
  x <- 1.1
  expect_error(
    inspect_prob(x),
    "Invalid argument: all elements of x must be in the \\[0, 1\\] interval."
  )
})

test_that("inspect_prob 16", {
  expect_error(
    inspect_prob(
      c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5),
      "Invalid argument: all elements of x must be in the \\[0, 1\\] interval."
    )
  )
})

test_that("inspect_prob 17", {
  expect_warning(inspect_prob(c(0.1, 0.2, NA, .4, 0.5), warning_nas = TRUE))
})

test_that("inspect_prob 18", {
  expect_error(inspect_prob(c(0.1, 0.2, NA, 0.4, 0.5), allow_nas = FALSE))
})

context("inspect_bfactor")

test_that("inspect_bfactor 1", {
  expect_error(
    inspect_bfactor(NULL),
    "Invalid argument: NULL is NULL."
  )
})

test_that("inspect_bfactor 2", {
  x <- NULL
  expect_error(
    inspect_bfactor(x),
    "Invalid argument: x is NULL."
  )
})

test_that("inspect_bfactor 3", {
  x <- factor(1)
  expect_error(
    inspect_bfactor(x),
    "Invalid argument: x must be an atomic vector."
  )
})

test_that("inspect_bfactor 4", {
  x <- list(1)
  expect_error(
    inspect_bfactor(x),
    "Invalid argument: x must be an atomic vector."
  )
})

test_that("inspect_bfactor 5", {
  x <- numeric(0)
  expect_error(
    inspect_bfactor(x),
    "Invalid argument: x is empty."
  )
})

test_that("inspect_bfactor 6", {
  x <- NA
  expect_error(
    inspect_bfactor(x),
    "Invalid argument: all elements of  x are NA or NaN."
  )
})

test_that("inspect_bfactor 7", {
  x <- NaN
  expect_error(
    inspect_bfactor(x),
    "Invalid argument: all elements of  x are NA or NaN."
  )
})

test_that("inspect_bfactor 8", {
  x <- c(NA_real_, NA_real_, NA_real_)
  expect_error(
    inspect_bfactor(x),
    "Invalid argument: all elements of  x are NA or NaN."
  )
})

test_that("inspect_bfactor 9", {
  x <- "1"
  expect_error(
    inspect_bfactor(x),
    "Invalid argument: the type of x must be numeric"
  )
})

test_that("inspect_bfactor 10", {
  expect_equal(
    inspect_bfactor(10),
    inspect_bfactor(2)
  )
})

test_that("inspect_bfactor 11", {
  expect_equal(
    inspect_bfactor(exp(1)),
    inspect_bfactor(2)
  )
})

test_that("inspect_bfactor 12", {
  expect_error(inspect_bfactor(-2))
})

test_that("inspect_bfactor 13", {
  expect_equal(
    inspect_bfactor(2),
    NULL
  )
})

test_that("inspect_bfactor 14", {
  expect_silent(inspect_bfactor(2))
})

test_that("inspect_bfactor 15", {
  expect_error(inspect_bfactor(c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)))
})

test_that("inspect_bfactor 16", {
  expect_warning(inspect_bfactor(c(0.1, 2, NA, 40, 0.5)))
})

test_that("inspect_bfactor 17", {
  expect_error(inspect_bfactor(c(0.1, 2, NA, 4, 0.5), allow_nas = FALSE))
})

test_that("inspect_bfactor 18", {
  expect_warning(inspect_bfactor(c(0.1, 2, NA, 4, 0.5), warning_nas = TRUE))
})

context("inspect_bfactor_log")

test_that("inspect_bfactor_log 1", {
  expect_error(
    inspect_bfactor_log(NULL),
    "Invalid argument: NULL is NULL."
  )
})

test_that("inspect_bfactor_log 2", {
  x <- NULL
  expect_error(
    inspect_bfactor_log(x),
    "Invalid argument: x is NULL."
  )
})

test_that("inspect_bfactor_log 3", {
  x <- factor(1)
  expect_error(
    inspect_bfactor_log(x),
    "Invalid argument: x must be an atomic vector."
  )
})

test_that("inspect_bfactor_log 4", {
  x <- list(1)
  expect_error(
    inspect_bfactor_log(x),
    "Invalid argument: x must be an atomic vector."
  )
})

test_that("inspect_bfactor_log 5", {
  x <- numeric(0)
  expect_error(
    inspect_bfactor_log(x),
    "Invalid argument: x is empty."
  )
})

test_that("inspect_bfactor_log 6", {
  x <- NA
  expect_error(
    inspect_bfactor_log(x),
    "Invalid argument: all elements of  x are NA or NaN."
  )
})

test_that("inspect_bfactor_log 7", {
  x <- NaN
  expect_error(
    inspect_bfactor_log(x),
    "Invalid argument: all elements of  x are NA or NaN."
  )
})

test_that("inspect_bfactor_log 8", {
  x <- c(NA_real_, NA_real_, NA_real_)
  expect_error(
    inspect_bfactor_log(x),
    "Invalid argument: all elements of  x are NA or NaN."
  )
})

test_that("inspect_bfactor_log 9", {
  x <- "1"
  expect_error(
    inspect_bfactor_log(x),
    "Invalid argument: the type of x must be numeric"
  )
})

test_that("inspect_bfactor_log 10", {
  expect_equal(
    inspect_bfactor_log(10),
    inspect_bfactor_log(2)
  )
})

test_that("inspect_bfactor_log 11", {
  expect_equal(
    inspect_bfactor_log(exp(1)),
    inspect_bfactor_log(2)
  )
})

test_that("inspect_bfactor_log 12", {
  expect_equal(
    inspect_bfactor_log(10),
    inspect_bfactor_log(-10)
  )
})

test_that("inspect_bfactor_log 13", {
  expect_equal(
    inspect_bfactor_log(2),
    NULL
  )
})

test_that("inspect_bfactor_log 14", {
  expect_silent(inspect_bfactor_log(2))
})

test_that("inspect_bfactor_log 15", {
  expect_warning(inspect_bfactor_log(c(0.1, 2, NA, 4, 0.5), warning_nas = TRUE))
})

test_that("inspect_bfactor_log 16", {
  expect_error(inspect_bfactor_log(c(0.1, 2, NA, 4, 0.5), allow_nas = FALSE))
})

context("inspect_log_base")

test_that("inspect_log_base 1", {
  expect_error(
    inspect_log_base(NULL),
    "Invalid argument: NULL is NULL."
  )
})

test_that("inspect_log_base 2", {
  x <- NULL
  expect_error(
    inspect_log_base(x),
    "Invalid argument: x is NULL."
  )
})

test_that("inspect_log_base 3", {
  x <- factor(1)
  expect_error(
    inspect_log_base(x),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_log_base 4", {
  x <- list(1)
  expect_error(
    inspect_log_base(x),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_log_base 5", {
  x <- numeric(0)
  expect_error(
    inspect_log_base(x),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_log_base 6", {
  x <- c(10, 2)
  expect_error(
    inspect_log_base(x),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_log_base 7", {
  x <- NA
  expect_error(
    inspect_log_base(x),
    "Invalid argument: x is NA or NaN."
  )
})

test_that("inspect_log_base 8", {
  x <- NaN
  expect_error(
    inspect_log_base(x),
    "Invalid argument: x is NA or NaN."
  )
})

test_that("inspect_log_base 9", {
  x <- "1"
  expect_error(
    inspect_log_base(x),
    "Invalid argument: the type of x must be numeric"
  )
})

test_that("inspect_log_base 10", {
  expect_equal(
    inspect_log_base(10),
    inspect_log_base(2)
  )
})

test_that("inspect_log_base 11", {
  expect_equal(
    inspect_log_base(exp(1)),
    inspect_log_base(2)
  )
})

test_that("inspect_log_base 12", {
  expect_equal(
    inspect_log_base(2),
    NULL
  )
})

test_that("inspect_log_base 13", {
  expect_silent(inspect_log_base(2))
})

test_that("inspect_log_base 14", {
  x <- -1
  expect_error(
    inspect_log_base(x),
    "Invalid argument: x must be positive."
  )
})

context("inspect_bfactor_scale")

test_that("inspect_bfactor_scale 1", {
  expect_error(
    inspect_bfactor_scale(NULL),
    "Invalid argument: NULL is NULL."
  )
})

test_that("inspect_bfactor_scale 2", {
  x <- NULL
  expect_error(
    inspect_bfactor_scale(x),
    "Invalid argument: x is NULL."
  )
})

test_that("inspect_bfactor_scale 3", {
  x <- factor(1)
  expect_error(
    inspect_bfactor_scale(x),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_bfactor_scale 4", {
  x <- list(1)
  expect_error(
    inspect_bfactor_scale(x),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_bfactor_scale 5", {
  x <- character(0)
  expect_error(
    inspect_bfactor_scale(x),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_bfactor_scale 6", {
  x <- c(TRUE, FALSE)
  expect_error(
    inspect_bfactor_scale(x),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_bfactor_scale 7", {
  x <- NA
  expect_error(
    inspect_bfactor_scale(x),
    "Invalid argument: x is NA or NaN."
  )
})

test_that("inspect_bfactor_scale 8", {
  x <- NaN
  expect_error(
    inspect_bfactor_scale(x),
    "Invalid argument: x is NA or NaN."
  )
})

test_that("inspect_bfactor_scale 9", {
  x <- "TRUE"
  expect_error(
    inspect_bfactor_scale(x),
    "Invalid argument: x must be either 'jeffreys' or 'kass-raftery'"
  )
})

test_that("inspect_bfactor_scale 10", {
  expect_equal(
    inspect_bfactor_scale("Jeffreys"),
    inspect_bfactor_scale("Kass-Raftery")
  )
})

test_that("inspect_bfactor_scale 11", {
  expect_equal(
    inspect_bfactor_scale("Jeffreys"),
    inspect_bfactor_scale("jeffreys")
  )
})

test_that("inspect_bfactor_scale 12", {
  expect_equal(
    inspect_bfactor_scale("Jeffreys"),
    NULL
  )
})

test_that("inspect_bfactor_scale 13", {
  expect_silent(inspect_bfactor_scale("Jeffreys"))
})

test_that("inspect_bfactor_scale 13", {
  x <- 1
  expect_error(
    inspect_bfactor_scale(x),
    "Invalid argument: the type of x must be character."
  )
})

context("inspect_true_or_false")

test_that("inspect_true_or_false 1", {
  expect_error(
    inspect_true_or_false(NULL),
    "Invalid argument: NULL is NULL."
  )
})

test_that("inspect_true_or_false 2", {
  x <- NULL
  expect_error(
    inspect_true_or_false(x),
    "Invalid argument: x is NULL."
  )
})

test_that("inspect_true_or_false 3", {
  x <- factor(1)
  expect_error(
    inspect_true_or_false(x),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_true_or_false 4", {
  x <- list(1)
  expect_error(
    inspect_true_or_false(x),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_true_or_false 5", {
  x <- logical(0)
  expect_error(
    inspect_true_or_false(x),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_true_or_false 6", {
  x <- c(TRUE, FALSE)
  expect_error(
    inspect_true_or_false(x),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_true_or_false 7", {
  x <- NA
  expect_error(
    inspect_true_or_false(x),
    "Invalid argument: x is NA or NaN."
  )
})

test_that("inspect_true_or_false 8", {
  x <- NaN
  expect_error(
    inspect_true_or_false(x),
    "Invalid argument: x is NA or NaN."
  )
})

test_that("inspect_true_or_false 9", {
  x <- "TRUE"
  expect_error(
    inspect_true_or_false(x),
    "Invalid argument: the type of x must be logical."
  )
})

test_that("inspect_true_or_false 10", {
  x <- "TRUE"
  expect_equal(
    inspect_true_or_false(TRUE),
    inspect_true_or_false(FALSE)
  )
})

test_that("inspect_true_or_false 11", {
  x <- "TRUE"
  expect_equal(
    inspect_true_or_false(TRUE),
    NULL
  )
})

test_that("inspect_true_or_false 12", {
  x <- "TRUE"
  expect_silent(inspect_true_or_false(TRUE))
})

context("inspect_categories")

test_that("inspect_categories 1", {
  expect_silent(inspect_categories(1:5))
})

test_that("inspect_categories 2", {
  expect_silent(inspect_categories(c("yes", "no")))
})

test_that("inspect_categories 3", {
  expect_silent(inspect_categories(factor(c(
    "smoker", "non-smoker"
  ))))
})

test_that("inspect_categories 4", {
  x <- factor(c("yes", "no", "yes"))
  expect_silent(inspect_categories(levels(x)))
})

test_that("inspect_categories 5", {
  x <- c(1, 1:5)
  expect_error(
    inspect_categories(x),
    "Invalid argument: all element of x must be unique."
  )
})

test_that("inspect_categories 6", {
  x <- c("yes", "no", "yes")
  expect_error(
    inspect_categories(x),
    "Invalid argument: all element of x must be unique."
  )
})

test_that("inspect_categories 7", {
  x <- factor(c("yes", "no", "yes"))
  expect_error(
    inspect_categories(x),
    "Invalid argument: all element of x must be unique."
  )
})

test_that("inspect_categories 8", {
  x <- NULL
  expect_error(
    inspect_categories(x),
    "Invalid argument: x is NULL."
  )
})

test_that("inspect_categories 9", {
  x <- numeric(0)
  expect_error(
    inspect_categories(x),
    "Invalid argument: x is empty"
  )
})

test_that("inspect_categories 10", {
  x <- complex(1)
  expect_error(
    inspect_categories(x),
    "Invalid argument:
    the type of x must be 'logical', 'integer', 'double' or 'character'."
  )
})

test_that("inspect_categories 11", {
  x <- list(10)
  expect_error(
    inspect_categories(x),
    "Invalid argument: x must be of an atomic type."
  )
})

test_that("inspect_categories 12", {
  x <- NA
  expect_error(
    inspect_categories(x),
    "Invalid argument: there are NA or NaN values in x."
  )
})

test_that("inspect_categories 13", {
  x <- NaN
  expect_error(
    inspect_categories(x),
    "Invalid argument: there are NA or NaN values in x."
  )
})

context("inspect_character_match")

test_that("inspect_character_match 1", {
  x <- "Kass"
  expect_silent(inspect_character_match(x, allowed = c("Kass", "Raftery")))
})

test_that("inspect_character_match 2", {
  x <- "kass"
  expect_silent(inspect_character_match(x, allowed = c("Kass", "Raftery")))
})

test_that("inspect_character_match 3", {
  x <- "kasss"
  expect_error(
    inspect_character_match(x, allowed = c("Kass", "Raftery")),
    "Invalid argument: x = 'kasss' is not allowed."
  )
})

test_that("inspect_character_match 4", {
  x <- "kass"
  expect_error(
    inspect_character_match(
      x,
      allowed = c("Kass", "Raftery"),
      case_sensitive = TRUE
    ),
    "Invalid argument: x = 'kass' is not allowed."
  )
})

test_that("inspect_character_match 5", {
  x <- NULL
  expect_error(
    inspect_character_match(x, allowed = "abc"),
    "Invalid argument: x is NULL."
  )
})

test_that("inspect_character_match 6", {
  x <- character(0)
  expect_error(
    inspect_character_match(x, allowed = "abc"),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_character_match 7", {
  x <- c("abc", "abcd")
  expect_error(
    inspect_character_match(x, allowed = "abc"),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_character_match 8", {
  x <- c("abc", "abc")
  expect_error(
    inspect_character_match(x, allowed = "abc"),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_character_match 9", {
  x <- "ab"
  expect_error(
    inspect_character_match(x, allowed = "abc"),
    "Invalid argument: x = 'ab' is not allowed."
  )
})

test_that("inspect_character_match 10", {
  x <- list("abc")
  expect_error(
    inspect_character_match(x, allowed = "abc"),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_character_match 11", {
  x <- factor("abc")
  expect_error(
    inspect_character_match(x, allowed = "abc"),
    "Invalid argument: x must be an atomic vector of length 1."
  )
})

test_that("inspect_character_match 11", {
  x <- NA
  expect_error(
    inspect_character_match(x, allowed = "abc"),
    "Invalid argument: x is NA or NaN"
  )
})

test_that("inspect_character_match 12", {
  x <- NaN
  expect_error(
    inspect_character_match(x, allowed = "abc"),
    "Invalid argument: x is NA or NaN"
  )
})

test_that("inspect_character_match 13", {
  x <- complex(1)
  expect_error(
    inspect_character_match(x, allowed = "abc"),
    "Invalid argument: the type of x must be character."
  )
})

context("inspect_character")

test_that("inspect_character 1", {
  x <- "Kass"
  expect_silent(inspect_character(x))
})

test_that("inspect_character 2", {
  x <- c("Kass", "Raftery")
  expect_silent(inspect_character(x))
})

test_that("inspect_character 3", {
  x <- c("Kass", "Raftery", NA)
  expect_silent(inspect_character(x))
})

test_that("inspect_character 4", {
  x <- letters
  expect_silent(inspect_character(x))
})

test_that("inspect_character 5", {
  x <- c("Kass", "Raftery", NA)
  expect_warning(inspect_character(x, warning_nas = TRUE))
})

test_that("inspect_character 6", {
  x <- NULL
  expect_error(
    inspect_character(x),
    "Invalid argument: x is NULL."
  )
})

test_that("inspect_character 7", {
  x <- character(0)
  expect_error(
    inspect_character(x),
    "Invalid argument: x is empty."
  )
})

test_that("inspect_character 8", {
  x <- 1
  expect_error(
    inspect_character(x),
    "Invalid argument: the type of x must be character."
  )
})

test_that("inspect_character 9", {
  x <- c(1, 2)
  expect_error(
    inspect_character(x),
    "Invalid argument: the type of x must be character."
  )
})

test_that("inspect_character 10", {
  x <- factor(c(1, 2))
  expect_error(
    inspect_character(x),
    "Invalid argument: x must be an atomic vector."
  )
})

test_that("inspect_character 11", {
  x <- list(c(1, 2))
  expect_error(
    inspect_character(x),
    "Invalid argument: x must be an atomic vector."
  )
})

test_that("inspect_character 12", {
  x <- NaN
  expect_error(
    inspect_character(x),
    "Invalid argument: all elements of x are NA or NaN."
  )
})

test_that("inspect_character 13", {
  x <- NA
  expect_error(
    inspect_character(x),
    "Invalid argument: all elements of x are NA or NaN."
  )
})

test_that("inspect_character 13", {
  x <- c("abc", NA)
  expect_error(
    inspect_character(x, allow_nas = FALSE),
    "Invalid argument: There are NA or NaN values in x."
  )
})


context("inspect_par_bernoulli")

test_that("inspect_par_bernoulli 1", {
  expect_silent(inspect_par_bernoulli(0.5))
})

test_that("inspect_par_bernoulli 2", {
  x <- NULL
  expect_error(inspect_par_bernoulli(x),
               "Invalid argument: x is NULL.")
})

test_that("inspect_par_bernoulli 3", {
  x <- TRUE
  expect_error(inspect_par_bernoulli(x),
               "Invalid argument: x must be numeric.")
})

test_that("inspect_par_bernoulli 4", {
  x <- factor(0.5)
  expect_error(inspect_par_bernoulli(x),
               "Invalid argument: x must be an atomic vector.")
})

test_that("inspect_par_bernoulli 5", {
  x <- matrix(0.5)
  expect_error(inspect_par_bernoulli(x),
               "Invalid argument: x must be an atomic vector.")
})

test_that("inspect_par_bernoulli 6", {
  x <- "0.5"
  expect_error(inspect_par_bernoulli(x),
               "Invalid argument: x must be numeric.")
})

test_that("inspect_par_bernoulli 7", {
  x <- list(0.5)
  expect_error(inspect_par_bernoulli(x),
               "Invalid argument: x must be an atomic vector.")
})

test_that("inspect_par_bernoulli 8", {
  x <- NA
  expect_error(inspect_par_bernoulli(x),
               "Invalid argument: x is NA or NaN.")
})

test_that("inspect_par_bernoulli 9", {
  x <- NaN
  expect_error(inspect_par_bernoulli(x),
               "Invalid argument: x is NA or NaN.")
})

test_that("inspect_par_bernoulli 10", {
  x <- numeric(0)
  expect_error(inspect_par_bernoulli(x),
               "Invalid argument: x must be of length 1.")
})

test_that("inspect_par_bernoulli 11", {
  x <- c(0.1, 0.5)
  expect_error(inspect_par_bernoulli(x),
               "Invalid argument: x must be of length 1.")
})

test_that("inspect_par_bernoulli 12", {
  x <- -0.1
  expect_error(inspect_par_bernoulli(x),
               "Invalid argument: x must be in the \\(0, 1\\) interval.")
})

test_that("inspect_par_bernoulli 13", {
  x <- 1.1
  expect_error(inspect_par_bernoulli(x),
               "Invalid argument: x must be in the \\(0, 1\\) interval.")
})

context("inspect_par_beta")

test_that("inspect_par_beta 1", {
  x <- c(1, 1)
  expect_silent(inspect_par_beta(x))

})

test_that("inspect_par_beta 2", {
  x <- c(2, 5)
  expect_silent(inspect_par_beta(x))

})

test_that("inspect_par_beta 3", {
  x <- NULL
  expect_error(inspect_par_beta(x),
               "Invalid argument: x is NULL.")

})

test_that("inspect_par_beta 4", {
  x <- 1
  expect_error(inspect_par_beta(x),
               "Invalid argument: x must be of length 2.")

})

test_that("inspect_par_beta 5", {
  x <- factor(1, 1)
  expect_error(inspect_par_beta(x),
               "Invalid argument: x must be an atomic vector.")

})

test_that("inspect_par_beta 6", {
  x <-  matrix(c(1, 1))
  expect_error(inspect_par_beta(x),
               "Invalid argument: x must be an atomic vector.")

})

test_that("inspect_par_beta 7", {
  x <-  c("1", "1")
  expect_error(inspect_par_beta(x),
               "Invalid argument: x must be numeric.")

})

test_that("inspect_par_beta 8", {
  x <- list(1, 1)
  expect_error(inspect_par_beta(x),
               "Invalid argument: x must be an atomic vector.")

})

test_that("inspect_par_beta 10", {
  x <- c(1, NaN)
  expect_error(inspect_par_beta(x),
               "Invalid argument: there are NA or NaN values in x.")

})

test_that("inspect_par_beta 11", {
  x <- c(TRUE, FALSE)
  expect_error(inspect_par_beta(x),
               "Invalid argument: x must be numeric.")

})

test_that("inspect_par_beta 12", {
  x <- numeric(0)
  expect_error(inspect_par_beta(x),
               "Invalid argument: x must be of length 2.")

})

test_that("inspect_par_beta 13", {
  x <- c(-1, 1)
  expect_error(inspect_par_beta(x),
               "Invalid argument: elements of x must be greather than 0.")

})

test_that("inspect_par_beta 14", {
  x <- c(1, NA)
  expect_error(inspect_par_beta(x),
               "Invalid argument: there are NA or NaN values in x.")

})

context("inspect_par_dirichlet")

test_that("inspect_par_dirichlet 1", {
  expect_silent(inspect_par_dirichlet(c(1, 1, 1)))
})

test_that("inspect_par_dirichlet 2", {
  expect_silent(inspect_par_dirichlet(c(2, 5)))
})

test_that("inspect_par_dirichlet 1", {
  x <- NULL
  expect_error(inspect_par_dirichlet(x),
               "Invalid argument: x is NULL.")

})

test_that("inspect_par_dirichlet 2", {
  x <- factor(1, 1, 1)
  expect_error(inspect_par_dirichlet(x),
               "Invalid argument: x must be an atomic vector.")

})

test_that("inspect_par_dirichlet 3", {
  x <- matrix(c(1, 1, 1))
  expect_error(inspect_par_dirichlet(x),
               "Invalid argument: x must be an atomic vector.")

})

test_that("inspect_par_dirichlet 4", {
  x <- c("1", "1", "1")
  expect_error(inspect_par_dirichlet(x),
               "Invalid argument: x must be numeric.")

})

test_that("inspect_par_dirichlet 5", {
  x <- list(1, 1, 1)
  expect_error(inspect_par_dirichlet(x),
               "Invalid argument: x must be an atomic vector.")

})

test_that("inspect_par_dirichlet 6", {
  x <- c(1, NA)
  expect_error(inspect_par_dirichlet(x),
               "Invalid argument: there are NA or NaN values in x.")

})

test_that("inspect_par_dirichlet 7", {
  x <- c(1, NaN, 1)
  expect_error(inspect_par_dirichlet(x),
               "Invalid argument: there are NA or NaN values in x.")

})

test_that("inspect_par_dirichlet 8", {
  x <- c(TRUE, FALSE)
  expect_error(inspect_par_beta(x),
               "Invalid argument: x must be numeric.")

})

test_that("inspect_par_dirichlet 9", {
  x <- numeric(0)
  expect_error(inspect_par_dirichlet(x),
               "Invalid argument: x is empty.")

})

test_that("inspect_par_dirichlet 9", {
  x <- c(-1, 1, 1)
  expect_error(inspect_par_dirichlet(x),
               "Invalid argument: elements of x must be greather than 0.")

})

context("inspect_par_haldane")

test_that("inspect_par_haldane 1", {
  expect_silent(inspect_par_haldane(c(0, 0, 0)))
})

test_that("inspect_par_haldane 2", {
  expect_silent(inspect_par_haldane(c(0, 0)))
})

test_that("inspect_par_haldane 3", {
  x <- NULL
  expect_error(inspect_par_haldane(x),
               "Invalid argument: x is NULL.")

})

test_that("inspect_par_haldane 4", {
  x <- factor(0, 0, 0)
  expect_error(inspect_par_haldane(x),
               "Invalid argument: x must be an atomic vector.")

})

test_that("inspect_par_haldane 5", {
  x <- matrix(c(0, 0, 0))
  expect_error(inspect_par_haldane(x),
               "Invalid argument: x must be an atomic vector.")

})

test_that("inspect_par_haldane 6", {
  x <- c("0", "0", "0")
  expect_error(inspect_par_haldane(x),
               "Invalid argument: x must be numeric.")

})

test_that("inspect_par_haldane 7", {
  x <- list(0, 0, 0)
  expect_error(inspect_par_haldane(x),
               "Invalid argument: x must be an atomic vector.")

})

test_that("inspect_par_haldane 8", {
  x <- c(0, NA)
  expect_error(inspect_par_haldane(x),
               "Invalid argument: there are NA or NaN values in x.")

})

test_that("inspect_par_haldane 9", {
  x <- c(0, NaN, 0)
  expect_error(inspect_par_haldane(x),
               "Invalid argument: there are NA or NaN values in x.")

})

test_that("inspect_par_haldane 10", {
  x <- c(TRUE, FALSE)
  expect_error(inspect_par_haldane(x),
               "Invalid argument: x must be numeric.")

})

test_that("inspect_par_haldane 11", {
  x <- numeric(0)
  expect_error(inspect_par_haldane(x),
               "Invalid argument: x is empty")

})

test_that("inspect_par_haldane 12", {
  x <- c(1, 0, 0)
  expect_error(inspect_par_haldane(x),
               "Invalid argument: all elements of x must be 0.")
})

context("inspect_par_multinomial")

test_that("inspect_par_multinomial 1", {
  expect_silent(inspect_par_multinomial(c(0.5, 0.5)))
})

test_that("inspect_par_multinomial 2", {
  expect_silent(inspect_par_multinomial(rep(1 / 5, 5)))
})

test_that("inspect_par_multinomial 3", {
  x <- NULL
  expect_error(inspect_par_multinomial(x),
               "Invalid argument: x is NULL.")
})

test_that("inspect_par_multinomial 4", {
  x <- TRUE
  expect_error(inspect_par_multinomial(x),
               "Invalid argument: x must be numeric.")
})

test_that("inspect_par_multinomial 5", {
  x <- factor(0.5, 0.5)
  expect_error(inspect_par_multinomial(x),
               "Invalid argument: x must be an atomic vector.")
})

test_that("inspect_par_multinomial 6", {
  x <- matrix(c(0.5, 0.5))
  expect_error(inspect_par_multinomial(x),
               "Invalid argument: x must be an atomic vector.")
})

test_that("inspect_par_multinomial 7", {
  x <- c("0.5", "0.5")
  expect_error(inspect_par_multinomial(x),
               "Invalid argument: x must be numeric.")
})

test_that("inspect_par_multinomial 8", {
  x <- list(0.5, 0.5)
  expect_error(inspect_par_multinomial(x),
               "Invalid argument: x must be an atomic vector.")

})

test_that("inspect_par_multinomial 9", {
  x <- c(0.9, NA)
  expect_error(inspect_par_multinomial(x),
               "Invalid argument: there are NA or NaN values in x.")

})

test_that("inspect_par_multinomial 10", {
  x <- c(0.9, NaN)
  expect_error(inspect_par_multinomial(x),
               "Invalid argument: there are NA or NaN values in x.")

})

test_that("inspect_par_multinomial 11", {
  x <- c(0.9, 0.6)
  expect_error(inspect_par_multinomial(x),
               "Invalid argument: x must sum to 1.")

})

test_that("inspect_par_multinomial 12", {
  x <- c(-0.1, 0.9)
  expect_error(
    inspect_par_multinomial(x),
    "Invalid argument: all elements of x values must be in the \\(0, 1\\) interval."
  )
})

test_that("inspect_par_multinomial 13", {
  x <- NA
  expect_error(inspect_par_multinomial(x),
               "there are NA or NaN values in x")
})

test_that("inspect_par_multinomial 14", {
  x <- numeric(0)
  expect_error(inspect_par_multinomial(x),
               "Invalid argument: x is empty.")
})

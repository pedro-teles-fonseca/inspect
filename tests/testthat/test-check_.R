
context("inspect_prob")

test_that("check silent 1", {expect_silent(inspect_prob(0))})
test_that("check silent 2", {expect_silent(inspect_prob(1))})
test_that("check silent 3", {expect_silent(inspect_prob(c(0.1, 0.2, 0.3, 0.4, 0.5)))})

test_that("check warning", {expect_warning(inspect_prob(c(0.1, 0.2, NA, 0.4, 0.5)))})

test_that("check error message 1", {expect_error(inspect_prob(NULL))})
test_that("check error message 2", {expect_error(inspect_prob(TRUE))})
test_that("check error message 3", {expect_error(inspect_prob("0.5"))})
test_that("check error message 4", {expect_error(inspect_prob(factor(0.5)))})
test_that("check error message 5", {expect_error(inspect_prob(matrix(0.5)))})
test_that("check error message 6", {expect_error(inspect_prob(list(0.5)))})
test_that("check error message 7", {expect_error(inspect_prob(NA))})
test_that("check error message 8", {expect_error(inspect_prob(NaN))})
test_that("check error message 9", {expect_error(inspect_prob(1.1))})
test_that("check error message 10", {expect_error(inspect_prob(-0.5))})
test_that("check error message 11", {expect_error(inspect_prob(c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)))})
test_that("check error message 12", {expect_error(inspect_prob())})

context("inspect_bf")

test_that("check silent 1", {expect_silent(inspect_bf(0))})
test_that("check silent 2", {expect_silent(inspect_bf(0.5))})
test_that("check silent 2", {expect_silent(inspect_bf(1))})
test_that("check silent 2", {expect_silent(inspect_bf(100))})
test_that("check silent 3", {expect_silent(inspect_bf(c(0, 0.5, 1, 10, 50, 100)))})
test_that("check silent 4", {expect_error(inspect_bf(-0.5))})
test_that("check silent 5", {expect_error(inspect_bf(c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)))})

test_that("check warning", {expect_warning(inspect_bf(c(0.1, 2, NA, 40, 0.5)))})

test_that("check error message 1", {expect_error(inspect_bf(NULL))})
test_that("check error message 2", {expect_error(inspect_bf(TRUE))})
test_that("check error message 3", {expect_error(inspect_bf("0.5"))})
test_that("check error message 4", {expect_error(inspect_bf(factor(0.5)))})
test_that("check error message 5", {expect_error(inspect_bf(matrix(0.5)))})
test_that("check error message 6", {expect_error(inspect_bf(list(0.5)))})
test_that("check error message 7", {expect_error(inspect_bf(NA))})
test_that("check error message 8", {expect_error(inspect_bf(NaN))})
test_that("check error message 12", {expect_error(inspect_bf())})

context("miscellaneous")

test_that("miscellaneous 1", {expect_error(inspect_prob(numeric(0)))})
test_that("miscellaneous 2", {expect_error(inspect_bf(numeric(0)))})
test_that("miscellaneous 3", {expect_error(inspect_log_bf(numeric(0)))})
test_that("miscellaneous 4", {expect_error(inspect_log_base(-1))})




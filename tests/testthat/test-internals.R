
context("inspect_equality")

test_that("inspect_equality 1", {
  expect_true(inspect_equality(sum(rep(1 / 5, 5)), 1))
})

test_that("inspect_equality 2", {
  expect_true(inspect_equality(sum(rep(1 / 3, 3)), 1))
})

test_that("inspect_equality 3", {
  expect_false(inspect_equality(sum(rep(1 / 9, 9)) + 0.00001, 1))
})

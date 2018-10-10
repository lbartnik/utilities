context("runtime")

test_that("try_load", {
  expect_true(try_load(utilities))
  expect_true(try_load("utilities"))
  expect_false(try_load(deadbeef))
})

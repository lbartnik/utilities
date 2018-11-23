context("counter")

test_that("counter returns subsequent numbers", {
  c <- counter()
  expect_equal(c(), 0)
  expect_equal(c(), 1)
  expect_equal(c(), 2)
})

test_that("counter starts with non-zero", {
  c <- counter(100)
  expect_equal(c(), 100)
  expect_equal(c(), 101)
})

test_that("counter printout", {
  expect_output({ print(counter()) }, '<counter, current value 0>')
})

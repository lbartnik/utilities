context("lists")

test_that("combine merges lists", {
  x <- combine(list(a=1), list(b=2))
  expect_equal(x, list(a=1, b=2))

  x <- combine(list(a=1), list(a=2))
  expect_equal(x, list(a=1))

  x <- combine(list(a=1), list(a=2, b=2), list(a=2, b=3, c=3))
  expect_equal(x, list(a=1, b=2, c=3))
})

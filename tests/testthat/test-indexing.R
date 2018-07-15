context("indexing")

test_that("nth gives n-th element", {
  expect_equal(nth(1:10, 3), 3)
  expect_equal(nth(list(1, 2, 3), 2), 2)
})

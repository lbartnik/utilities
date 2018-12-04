context("runtime")

test_that("visible", {
  x <- 1
  expect_true(is_visible(x))

  x <- set_invisible(x)
  expect_false(is_visible(x))
})

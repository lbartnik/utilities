context("debug")

test_that("stopif", {
  expect_error(stopif(T))
  expect_silent(stopif(F))
  expect_error(stopif(F, F, length(1)), ".*length\\(1\\)")
})

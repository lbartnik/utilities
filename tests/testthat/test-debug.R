context("debug")

test_that("stopif", {
  expect_error(stopif(T))
  expect_silent(stopif(F))
  expect_error(stopif(F, F, length(1)), ".*length\\(1\\)")
})

test_that("guard from a function", {
  opt <- options(utilities.debug = TRUE)
  on.exit(options(opt), add = TRUE)
  
  f <- function () guard()
  expect_output(f(), "\\[DEBUG\\] -> f\\(\\)\n\\[DEBUG\\] <- f\\(\\)")
})

test_that("guard from a double-colon function", {
  opt <- options(utilities.debug = TRUE)
  on.exit(options(opt), add = TRUE)
  
  expect_output(utilities::test_guard(),
                "\\[DEBUG\\] -> utilities::test_guard\\(\\)\n\\[DEBUG\\] <- utilities::test_guard\\(\\)")
})

test_that("guard from a closure", {
  opt <- options(utilities.debug = TRUE)
  on.exit(options(opt), add = TRUE)
  
  e <- environment()
  f <- function () guard()
  expect_output(do.call(f, list(), envir = e),
                "\\[DEBUG\\] -> <unnamed>\\(\\)\n\\[DEBUG\\] <- <unnamed>\\(\\)")
})

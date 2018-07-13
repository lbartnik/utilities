context("map")

test_that("map assigns names", {
  x <- map(list(a = 1), I)
  expect_named(x, 'a')
  
  x <- map('a', I)
  expect_named(x, 'a')
})

test_that("imap passes names and values", {
  imap(list(a = 1), function (...) {
    args <- list(...)
    expect_length(args, 2)
    expect_equal(nth(args, 1), 1)
    expect_equal(nth(args, 2), 'a')
  })
})


test_that("imap handles edge cases", {
  expect_length(imap(NULL, print), 0)
})

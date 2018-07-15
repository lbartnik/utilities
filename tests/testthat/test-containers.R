context("containers")

test_that("vector erase", {
  v <- new_vector(data = 1:10)
  expect_equal(v$size(), 10)
  
  v$erase(3L)
  expect_equal(v$size(), 9)
  expect_false(3L %in% v$data())
})

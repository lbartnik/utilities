context("containers")

test_that("vector erase", {
  v <- new_vector(data = 1:10)
  expect_equal(v$size(), 10)
  
  v$erase(3L)
  expect_equal(v$size(), 9)
  expect_false(3L %in% v$data())
})

test_that("constructor makes elements unique", {
  s <- new_set(1, 1, 2)
  expect_equal(s$data(), as.list(1:2))
})

test_that("set holds unique elements", {
  s <- new_set()
  
  s$insert(1)
  expect_equal(s$data(), as.list(1))
  
  s$insert(1)
  expect_equal(s$data(), as.list(1))
  
  s$insert(2)
  expect_equal(s$data(), as.list(1:2))
})

test_that("set support inserting multiple elements", {
  s <- new_set()
  
  s$insert(c(1,2))
  expect_equal(s$data(), as.list(1:2))
  
  s$insert(c(1,2))
  expect_equal(s$data(), as.list(1:2))
})

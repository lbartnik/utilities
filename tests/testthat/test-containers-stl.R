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

test_that("set size", {
  s <- new_set(1, 2)
  expect_equal(s$size(), 2)
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

test_that("set contains", {
  s <- new_set(1, 2)
  expect_true(s$contains(1))
  expect_true(s$contains(2))
  expect_false(s$contains(3))
})

test_that("set remove", {
  s <- new_set(1, 2)
  s$remove(1)
  expect_false(s$contains(1))
  expect_true(s$contains(2))
})

test_that("set pop_front", {
  s <- new_set(1, 2)
  e <- s$pop_front()
  expect_false(s$contains(e))
  expect_equal(s$size(), 1)
})

test_that("map at", {
  m <- new_map()
  expect_error(m$at('1'))
  expect_silent(m$at('1', 2))
  expect_equal(m$at('1'), 2)
})

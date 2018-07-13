context("properties")

test_that("empty is recognized", {
  expect_true(is_empty(new.env()))
  
  e <- as.environment(list(a = 1))
  expect_false(is_empty(e))
  expect_silent(is_empty(e))
  
  expect_true(is_empty(NULL))
  expect_true(is_empty(NA))
  expect_true(is_empty(list()))
  expect_true(is_empty(character()))
  expect_true(is_empty(numeric()))
  expect_true(is_empty(""))
  
  expect_false(is_empty("a"))
  expect_false(is_empty(0))
})


test_that("error is recognized", {
  a <- try(stop("e"), silent = TRUE)
  expect_true(is_error(a))
  
  a <- tryCatch(stop("e"), error = function(e)e)
  expect_true(is_error(a))
})


test_that("atomic class", {
  expect_true(is_atomic_class("numeric"))
  expect_true(is_atomic_class("integer"))
  expect_true(is_atomic_class("character"))
  expect_true(is_atomic_class("logical"))
  expect_true(is_atomic_class("complex"))
})


test_that("is_all_named requires names", {
  expect_true(is_all_named(list(a = 1, b = 2)))
  expect_false(is_all_named(list(a = 1, 2)))
})

test_that("is index of", {
  expect_true(is_index_of(1, 1))
  expect_true(is_index_of(1, 1:10))
  
  expect_false(is_index_of(0, 1))
  expect_false(is_index_of(3, 1:2))
  
  expect_true(is_index_of('a', list(a = 1)))
  expect_false(is_index_of('b', list(a = 1)))
})

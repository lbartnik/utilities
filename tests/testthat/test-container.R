context("container")

test_that("container is a container", {
  c <- as_container(as.list(1:3))
  expect_true(is_container(c))
})

test_that("container can be turned to string", {
  c <- as_container(as.list(1:3))
  expect_equal(toString(c), "<container of 3 elements of class integer>")
})

test_that("container can be printed", {
  c <- as_container(as.list(1:5))
  expect_output_file(print(c), "expected-output/print-container.txt")
})

test_that("container with no print method", {
  c <- as_container(list(structure(1, class = 'xyz')))
  expect_output_file(print(c), "expected-output/print-container-no-print-method.txt")
})

test_that("sort container", {
  c <- as_container(list(list(time = 5), list(time = 2), list(time = 1)))
  x <- container_sort(c, time)
  expect_equal(map_int(x, `[[`, 'time'), c(1, 2, 5))

  y <- sort(c, decreasing = FALSE, time)
  expect_equal(y, x)
})

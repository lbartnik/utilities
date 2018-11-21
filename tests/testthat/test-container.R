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


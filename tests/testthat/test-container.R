context("container")

test_that("container is a container", {
  c <- as_container(as.list(1:3))
  expect_true(is_container(c))
})

test_that("container can be printed", {
  c <- as_container(as.list(1:3))
  expect_output(print(c), "<container, 3 element\\(s\\)>")
})

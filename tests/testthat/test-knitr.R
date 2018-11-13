context("knitr")

test_that("trimming hook shows chosen lines", {
  skip_if_not_installed('fansi')
  require(fansi, quietly = TRUE, warn.conflicts = FALSE)

  th <- create_trimming_hook(function(x, ...)x)

  # show first n
  op <- list(output.lines = 3)
  expect_equal(fansi::strip_sgr(th("a\nb\nc\nd\ne", op)),
               "a\nb\nc\n# ... with 2 more line(s)\n")

  # show range
  op <- list(output.lines = 2:4)

  expect_equal(fansi::strip_sgr(th("a\nb\nc\nd\ne", op)),
               "# ... with 1 more line(s)\nb\nc\nd\n# ... with 1 more line(s)\n")
})


context("cat")

test_that("break text", {
  expect_length(break_text("text with words", "with"), 3)
  expect_length(break_text("longer text with many words", c("text", "many")), 5)
})

test_that("colourize", {
  input  <- "text with words"
  output <- colorize(input, yellow = "with")

  if (crayon::has_color()) {
    # crayon turns colors off in non-interactive runs
    expect_equal(output, "text \033[33mwith\033[39m words")
  } else {
    expect_equal(output, input)
  }
})

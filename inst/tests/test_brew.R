test_that("code runs without output", {
  env <- new.env()
  output <- capture.output(brew(text="<% x <- 123 %>", envir=env))
  expect_equal(output, "")
  expect_equal(env$x, 123)
})

test_that("regular text at the beginning of a line with code", {
  env <- new.env()
  output <- capture.output(brew(text="foo <% x <- 123 %>", envir=env))
  expect_equal(output, "foo ")
})

test_that("regular text at the end of a line with code", {
  env <- new.env()
  output <- capture.output(brew(text="<% x <- 123 %> foo", envir=env))
  expect_equal(output, " foo")
})

test_that("comments are swallowed", {
  output <- capture.output(brew(text="<%# foo %>"))
  expect_equal(output, "")
})

test_that("code runs with output", {
  output <- capture.output(brew(text="<%= 123 %>"))
  expect_equal(output, "123")
})

test_that("templates are output correctly", {
  output <- capture.output(brew(text="<%%= 123 %%>"))
  expect_equal(output, "<%= 123 %>")
})

test_that("templates with extra percents are reduced correctly", {
  output <- capture.output(brew(text="<%%%= 123 %%%>"))
  expect_equal(output, "<%%= 123 %%>")
})

test_that("incomplete expressions are executed properly", {
  expected <- c("Incomplete expression tests", "1 2 3 4 5 OK")
  actual <- capture.output(brew('../incomplete.brew'))
  expect_equal(actual, expected)
})

test_that("brew recursion", {
  expected <- c("Recursive brew test: depth 0", "Recursive brew test: depth 1",
    "Recursive brew test: depth 2", "Recursive brew test: depth 3",
    "Recursive brew test: depth 4")
  actual <- capture.output(brew('../recursion.brew'))
  expect_equal(actual, expected)
})

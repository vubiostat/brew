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

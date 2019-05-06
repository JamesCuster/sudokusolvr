context("Testing the solver functions")

test_that("initArray returns correct list object", {
  expect_is(initArray(puzzle1), "list")
  expect_is(initArray(puzzle1)[[1]], "matrix")
  expect_is(initArray(puzzle1)[[2]], "array")
})


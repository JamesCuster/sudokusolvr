context("Testing the solver functions")


# Todo notes. I changed what initArray outputs, need to update test

test_that("initArray returns correct list object", {
  expect_is(initArray(puzzle1), "list")
  expect_is(initArray(puzzle1)[[1]], "matrix")
  expect_equal(dim(initArray(puzzle1)[[1]]), c(9, 9))
  expect_is(initArray(puzzle1)[[2]], "array")
  expect_is(dim(initArray(puzzle1)[[2]]), c(9, 9, 9))
})


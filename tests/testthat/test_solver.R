context("Testing the solver functions")


# Todo notes. I changed what initArray outputs, need to update test

test_that("initArray returns 9x9x9 logical array", {
  expect_is(initArray(puzzle1), "array")
  expect_equal(typeof(initArray(puzzle1)), "logical")
  expect_equal(dim(initArray(puzzle1)), c(9, 9, 9))
})

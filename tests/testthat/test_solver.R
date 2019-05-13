context("Testing the solver functions")


# sudokuPuzzle ------------------------------------------------------------

test_that("sodukuPuzzle class constructor function", {
  expect_is(sudokuPuzzle(puzzle1), "sudokuPuzzle")
  expect_equal(sudokuPuzzle(puzzle1)$puzzle, puzzle1)
  expect_equal(sudokuPuzzle(puzzle1)$array, puzzle1Array)
})


# initArray ---------------------------------------------------------------

test_that("initArray returns 9x9x9 logical array", {
  expect_is(initArray(puzzle1), "array")
  expect_equal(typeof(initArray(puzzle1)), "logical")
  expect_equal(dim(initArray(puzzle1)), c(9, 9, 9))
  expect_equal(initArray(puzzle1), puzzle1Array)
})


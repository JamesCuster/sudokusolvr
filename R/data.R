#' A 9x9 matrix representation of a sudoku puzzle
#'
#' a matrix object with 9 rows and 9 columns which is a representation of a
#' sudoku puzzle. Cells in the soduku puzzle which do not contain a number are
#' filled with \code{NA}.
#'
#' @format a matrix with 9 rows and 9 columns
"puzzle1"

#' A 9x9x9 array representing solutions of a sudoku puzzle
#'
#' a logical array object where the first and second dimensions represent the
#' rows and columns of a sudoku puzzle and the third dimension represnts which
#' numbers 1-9 can go in a particular row/column combination. Cells in the array
#' in which it is unknown whether that number can go in the row/column
#' combination are filled with \code{NA}
#'
#' @format a 9x9x9 array
"puzzle1Array"

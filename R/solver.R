# These fucntions all work together to create a sudoku solver algorithm.


#' Creates a sudokuPuzzle class object
#'
#' The function \code{sudokuPuzzle} creates a sudoku puzzle object, which is a
#' list that contains the sudoku puzzle as the first list item and a 9x9x9
#' logical array which is used to represent and find the solutions to the puzzle
#'
#' @param puz a 9x9 matrix representation of a sudoku puzzle. Puzzle cells which
#'   do not contain a number should be coded as \code{NA}.
#'
#' @return A sudokuPuzzle object is a list object with two elements. The first
#'   element is a 9x9 matrix representation of a sudoku puzzle. This is the same
#'   as the input \code{puz}. The second element is a 9x9x9 logical array which
#'   stores information about what values can and cannot go into specific cells
#'   of the sudoku puzzle. The array is created using \code{initArray}.
#'
#' @export

sudokuPuzzle <- function(puz){
  if (!is.matrix(puz)) {
    stop("puz is not a 9x9 matrix")
  } else if (!all.equal(dim(puz), c(9, 9))) {
    stop("puz is not a 9x9 matrix")
  }
  a <- initArray(puz)
  return(structure(list(puzzle = puz, array = a), class = "sudokuPuzzle"))
}


#' Create puzzle array
#'
#' \code{initArray} takes a 9x9 matrix representation of a sudodu puzzle and
#' creates a 9x9x9 logical array, which contains the information about which
#' numbers can and cannot go in particular places based on the initial values in
#' the puzzle.
#'
#' @param puz a 9x9 matrix representation of a sudoku puzzle. Puzzle cells which
#'   do not contain a number should be coded as \code{NA}.
#'
#' @return Returns a 9x9x9 logical array which captures all information
#'   regarding which numbers can and cannot go in each cell of the sudoku
#'   puzzle. The dimensions of the array are represented by i, j, and k. The i
#'   and j dimensions correspond to the \code{[i, j]} cell of the 9x9 sudoku
#'   puzzle matrix, and the kth dimension represents the values of 1-9 which can
#'   possibly go into the cells of the sudoku puzzle.
#'
#'   The \code{[i, j, ]} elements of the array represent which values of 1-9 can
#'   possibly go into the \code{[i, j]} cell of the puzzle. If there is 1
#'   \code{TRUE} and 8 \code{FALSE} then the kth element which is \code{TRUE} is
#'   the number that goes into the \code{[i, j]} cell of the sudoku puzzle. If
#'   there are 2 or more \code{NA}s in these 9 elements, then there is not
#'   enough information to fill in the cell.
#'
#'   The \code{[i, , k]} elements of the array represent where a particular
#'   value, k, can go in the ith row of the puzzle. Similar to above, if there
#'   is 1 \code{TRUE} and 8 \code{FALSE} then the jth element which is
#'   \code{TRUE} gives the \code{[i, j]} cell of the sudoku puzzle is the number
#'   k. If there are 2 or more \code{NA}s in these 9 elements, then there is not
#'   enough information to fill in the number k in the ith row.
#'
#'   The \code{[, j, k]} elements of the array represent where a particular
#'   value, k, can go in the jth column of the puzzle. Similar to above, if
#'   there is 1 \code{TRUE} and 8 \code{FALSE} then the ith element which is
#'   \code{TRUE} gives the \code{[i, j]} cell of the sudoku puzzle is the number
#'   k. If there are 2 or more \code{NA}'s in these 9 elements, then there is
#'   not enough information to fill in the number k in the jth column.
#'
#' @examples
#'  initArray(puzzle1)
#'
#' @export

initArray <- function(puz){
  puzArr <- array(NA, c(9, 9, 9))
  for (i in 1:9){
    for (j in 1:9){
      if (!is.na(puz[i, j])){
        k <- puz[i, j]
        puzArr <- upArray(puzArr, i, j, k)
      }
    }
  }
  return(puzArr)
}


#' Update puzzle array given values i, j, and k
#'
#' This function takes a puzzle array and a solution for the puzzle given as an
#' \code{[i, j, k]} index and update the array given the provided solution.
#'
#' @param puzArr 9x9x9 array corresponding to the sudoku puzzle to be solved.
#' @param i,j integer between 1 and 9 referrencing the \code{[i, j]} element of
#'   the sudoku puzzle matrix and puzzle array
#' @param k integer between 1 and 9. Represents the value that goes in the
#'   \code{[i, j]} cell of the puzzle matrix and is the third dimension index of
#'   the puzzle array.
#'
#' @return Returns an updated puzzle array. Given that k goes in the \code{[i,
#'   j]} element of the puzzle matrix, the puzzle array is updated accordingly
#'   by placing \code{TRUE}/\code{FALSE} values in the appropriate places in the
#'   puzzle array.
#'
#'   Function is used within \code{initArray} to initialize the puzzle array
#'   given the starting values of the sudoku puzzle.
#'
#' @examples
#' \dontrun{upArray(b, 1, 1, 1)}
#'
#' @export

upArray <- function(puzArr, i, j, k) {
  puzArr[i, j, k] <- TRUE
  # makes all other numbers the i, jth box false
  puzArr[i, j, -k] <- FALSE
  # makes the number puz[i, j] false in the rest of the row
  puzArr[-i, j, k] <- FALSE
  # makes the number puz[i, j] false in the rest of the column
  puzArr[i, -j, k] <- FALSE
  # Indicies to fill in the appropriate 3x3 boxes with false
  ibox <- ((3*((i-1) %/% 3)) + 1:3)
  ibox <- ibox[which(ibox != i)]
  jbox <- ((3*((j-1) %/% 3)) + 1:3)
  jbox <- jbox[which(jbox != j)]
  # fills the appropriate 3x3 with falses
  puzArr[ibox, jbox, k] <- FALSE
  return(puzArr)
}


# Function implementing logic1 --------------------------------------------
#' Function to implement first logical reasoning to find solutions
#'
#' This function searches over the i rows and j columns of each kth dimension of
#' the 9x9x9 logical array. It searches for rows and columns where there is only
#' one \code{NA} value. If a row or column that meets this criteria is found the
#' function returns the \code{[i, j, k]} indicies where it occurs.
#'
#' @param puzList a \code{sudokuPuzzle} object which contains a 9x9 matrix
#'   representation of a sudoku puzzle as the first list element and a 9x9x9
#'   logical array representing the solutions of the sudoku puzzle
#'
#' @return The function searches the logical array of the sudokuPuzzle object
#'   for new solutions. If a solution is found, the function returns the
#'   \code{[i, j, k]} index where it occurs. If no new solution is found
#'   \code{NULL} is returned. This function is meant to be used recursively
#'   inside of the \code{solver} function in conjunction with \code{upPuz} which
#'   takes the output of \code{logic1} as input to update the 9x9 puzzle matrix
#'   and the 9x9x9 logical array of the sudokuPuzzle object.
#'
#' @export

logic1 <- function(puzList){
  # check the ith dimension for rows with one NA
  for (k in 1:9) {
    if (any(rowSums(is.na(puzList[[2]][, , k])) == 1)){
      # Selects the i index (only the first one if there are multiples)
      i <- which(rowSums(is.na(puzList[[2]][, , k])) == 1)[1]
      # Selects the i index (only the first one if there are multiples)
      j <- which(is.na(puzList[[2]][i, , k]))
      return(c(i, j, k))
    }
    # check the jth dimension for columns with one NA
    if (any(colSums(is.na(puzList[[2]][, , k])) == 1)){
      # Selects the j index (only the first one if there are multiples)
      j <- which(colSums(is.na(puzList[[2]][, , k])) == 1)[1]
      # Selects the j index (only the first one if there are multiples)
      i <- which(is.na(puzList[[2]][, j , k]))
      return(c(i, j, k))
    }
  }
  return(NULL)
}


# Function Implementing Logic2 --------------------------------------------
#' Function to implement second logical reasoning to update puzzle array
#'
#' This function looks for locations that a number cannot go, and will update
#' the 9x9x9 logical array from \code{NA} to \code{FALSE}.
#'
#' @inheritParams logic1
#'
#' @return This function looks within each of the smaller 9 3x3 boxes within the
#'   sudoku puzzle to search if there is a number that must go in a particular
#'   row or column within the 3x3 box. If such a number is found this means that
#'   this number cannot go in any part of the row or column that is not within
#'   the given 3x3 box. Therefore, the function will update the corresponding
#'   row or column in the array to reflect this.
#'
#' @export

logic2 <- function(puzList){
  for (k in 1:9){
    for (i in 1:3){
      for (j in 1:3){
        ibox <- (3*(i-1)) + 1:3
        jbox <- (3*(j-1)) + 1:3
        if (sum(rowSums(is.na(puzList[[2]][ibox, jbox, k])) > 0) == 1) {
          ro <- which(rowSums(is.na(puzList[[2]][ibox, jbox, k]) > 0) > 0)
          puzList[[2]][ibox[ro], -jbox, k] <- FALSE
        }
        if (sum(colSums(is.na(puzList[[2]][ibox, jbox, k])) > 0) == 1) {
          co <- which(colSums(is.na(puzList[[2]][ibox, jbox, k]) > 0) > 0)
          puzList[[2]][-ibox, jbox[co], k] <- FALSE
        }
      }
    }
  }
  return(puzList)
}



# Function to update puzzle and array with solution -----------------------
#' Function to update puzzle and puzzle array when solution is found
#'
#' Takes as input the \code{sudokuPuzzle} object and a solution produced from
#' the \code{logic1} function
#'
#' @inheritParams logic1
#' @param solution this is a vector of three numerics which are the index of the
#'   solution found
#'
#' @return This function takes a sudokuPuzzle object and a solution produced
#'   from \code{logic1} and updates the 9x9 puzzle matrix with the corresponding
#'   solution and places the correct \code{TRUE/FALSE} values in the
#'   corresponding places in the 9x9x9 logical array
#'
#' @export

upPuz <- function(puzList, solution) {
  i <- solution[1]
  j <- solution[2]
  k <- solution[3]

  # updates the puzzle
  puzList[[1]][i, j] <- k

  # updates the array
  puzList[[2]] <- upArray(puzList[[2]], i, j, k)
  return(puzList)
}


# Function that combines all functions above ------------------------------
#' Function which solves a sudoku puzzle
#'
#' Takes a 9x9 matrix that represents a sudoku puzzle and returns the puzzle
#' solved.
#'
#' @param puz 9x9 matrix representation of a sudoku puzzle. Cells of the sudoku
#'   puzzle which are not filled in should be represented by \code{NA} values.
#'
#' @return Takes as input a 9x9 representation of a sudoku puzzle and finds and
#'   returns its solution. This function utilizes the functions \code{initArray}
#'   as well as \code{logic1}, \code{logic2}, \code{upArray} recursively in
#'   order to find the solution for the puzzle
#'
#' @export

solver <- function(puz){
  puzList <- initArray(puz)
  iterator <- function(puzList){
    solution <- logic1(puzList)
    if (!is.null(solution)){
      print(solution)
      puzList <- upPuz(puzList, solution)
      iterator(puzList)
    } else {
      puzList2 <- logic2(puzList)
      if (identical(puzList, puzList2)){
        return(puzList)
      } else {
        iterator(puzList2)
      }
    }
  }
  puzList <- iterator(puzList)
  print("No more solutions")
  return(puzList)
}

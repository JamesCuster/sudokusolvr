# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



# on attach can be used to do things when the package is attached ---------

  # displays welcome message
  # can also make it run code if needed
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my package \nyou don't actually change libname and pckgname for this to work")
}

#' Print "Hello, World!"
hello <- function() {
  print("Hello, World!")
}

#' Print "Hello, Punny Human"
world.hello <- function(){
  print("Hello, Punny Human")
}


# package imports and suggestions -----------------------------------------

# devtools::use_package()
# to add package to imports

# devtools::use_package(, "Suggests")
# to add to suggested packages

# When using suggested packages need to do 1 of 2 things
 # 1
# # You need the suggested package for this function
# my_fun <- function(a, b) {
#   if (!requireNamespace("pkg", quietly = TRUE)) {
#     stop("Package \"pkg\" needed for this function to work. Please install it.",
#          call. = FALSE)
#   }
# }
#
 # 2
# # There's a fallback method if the package isn't available
# my_fun <- function(a, b) {
#   if (requireNamespace("pkg", quietly = TRUE)) {
#     pkg::f()
#   } else {
#     g()
#   }
# }


# examples of function documentation --------------------------------------

#' Product of two elements.
#'
#' \code{multi} returns the product of two arguments x and y.
#'
#' @param x single numeric
#' @param y single numeric
#'
#' @return the product of x and y.
#'
#' @examples
#'  multi(10, 10)
#'  multi(5, 5)
#'
#' @export
multi <- function(x, y) {
  x * y
}


# Can have function inherit parameters from another function --------------

#' Sum of two numbers
#'
#' \code{add} returns the sum of x, y, and z.
#'
#' @inheritParams multi
#' @param z single numeric
#'
#' @return the sum of x, y, and z.
#'
#' @examples
#'  sudokusolvr:::add(3, 4, 6)
#'
add <- function(x, y, z) {
  x + y + z
}


# Two ways to document multiple functions on the same page ----------------
 # using rdname to add to existing file
#' Basic arithmetic1
#'
#' @param x,y numeric vectors.
#'
add1 <- function(x, y) x + y

#' @rdname add1
#'
times1 <- function(x, y) x * y


  # Create dummy file and then add to that
#' Basic arithmetic2
#'
#' @param x,y numeric vectors.
#' @name arith
NULL

#' @rdname arith
#'
add2 <- function(x, y) x + y

#' @rdname arith
#'
times2 <- function(x, y) x * y


#' Row Product
#'
#' Compute the product of all elements in a row in a matrix.
#'
#' @param x any numeric matrix.
#' @param na.rm logical; if TRUE, then remove missing value before computation.
#' @return a vector representing the row products of the data in \code{x}.
#'
#' @export skew

row.prod <- function (x, na.rm = FALSE) {
  n <- nrow(x); y <- double(length = n)
  for (i in seq_len(n)) {
    y[i] <- prod(x[i,] , na.rm = na.rm)
  }
  y
}

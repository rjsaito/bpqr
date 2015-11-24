#' Row Product
#'
#' Compute the product of all elements in a row in a matrix.
#'

row.prod <- function (x, na.rm = FALSE) {
  n <- nrow(x); y <- double(length = n)
  for (i in seq_len(n)) {
    y[i] <- prod(x[i,] , na.rm = na.rm)
  }
  y
}

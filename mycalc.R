#' My Calc
#'
#' This function performs basic arithmetic operations including
#'    addition, subtration, division, and multiplication. The
#'    user specifies two numeric arguments and one operand argument.
#' @param a,b numeric vector or matrix
#' @param oper string describing the operand to use. Valid values
#'    for this parameter include "sum", "subtract", "divide", and "multiply".
#' @export
#' @examples
#' mycalc(1:5, c(5, 2, 3, 10, 2), "divide")
#' mycalc(matrix(1:9, nrow = 3, ncol = 3), 10, "multiply")
#'
#' \dontrun{
#' mycalc(1:5, c(5, 2, 3, 10, 2), "subtract")
#' }


mycalc <- function(a, b, oper) {
  if (typeof(a) == "character" || typeof(b) == "character") {
    stop("Objects of type 'character' are not supported in this function.", call. = FALSE)
  } else {
    if (oper == "sum") {
      result <- a + b
    } else if (oper == "subtract") {
      result <- a - b
    } else if (oper == "divide") {
      if (b==0){ stop("divide by zero error")}
      result <- a / b
    } else if (oper == "multiply") {
      result <- a * b
    } else {
      stop("Your operator is not recognized. Please check your spelling.", call. = FALSE)
    }
    return(result)
  }
}
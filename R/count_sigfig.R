#' @name count_sigfig
#' @title Count Significant Figures in a Measured Value
#' 
#' @description Calculated values obtained through multiplication or division of 
#'   measured or calculated values retain only as many significant figures as the 
#'   value in the calculation with the fewest significant figures. The 
#'   \code{measured} class, however, does not maintain a \code{sigfig} attribute.
#'   The \code{count_sigfig} function determines the appropriate significnt figures
#'   for a measurement based on the precision of the instrument recorded in the 
#'   \code{precision} attribute.
#'   
#' @param x A vector that inherits the \code{measured} class, but that does 
#'   \emph{not} inherit the \code{calculated} class.
#'   
#' @details Vectors inheriting the \code{calculated} class have a \code{sigfig} 
#'   attribute, which is a more reliable reference for significant figures of
#'   a calculated value. 
#'   
#'   For measured values, the instrument precision is used as the limit by which
#'   the significant figures are determined.  This is notably different than 
#'   counting significant figures by any set of rules of which digits are significant
#'   and which are not.  The \code{count_sigfig} approach is to translate the 
#'   values such that all of the significant figures are to the left of the decimal 
#'   point. The \code{log} base 10 of the translated value rounded up to the next 
#'   integer estimates provides the number of digits to the left.
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item If \code{x} does not inherit class \code{measured}, cast an error.
#'   \item Return the correct count of significant figures for the 
#'         measurement.
#' }
#'  
#' @author Benjamin Nutter
#' 
#' @source  
#' \url{https://cboard.cprogramming.com/cplusplus-programming/131043-determining-order-magnitude-user-inputed-number.html}
#' 
#' @export

count_sigfig <- function(x)
{
  checkmate::assert_class(x = x,
                          classes = "measured")
  
  precision <- attr(x, "precision")
  x <- as.numeric(x)
  
  ceiling(
    log(abs(x * 10 ^ (precision * -1)),
        base = 10)
  )
}

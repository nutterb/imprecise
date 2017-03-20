#' @name print.measured
#' @title Print Measured Vector
#' 
#' @description Print a Measured Vector
#' 
#' @param x A vector that inherits class \code{measured}
#' @param label \code{logical(1)}. When \code{TRUE}, the label is printed.
#' @param units \code{logical(1)}. When \code{TRUE}, the unit of measurement
#'   is printed.
#' @param ... Arguments to pass to other methods.
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item The value of \code{x} is rounded to the corresponding value 
#'     in the \code{precision} attribute.
#'   \item When \code{label = TRUE}, the \code{label} attribute is 
#'     printed above the output.
#'   \item When \code{units = TRUE}, the \code{units} attribute is 
#'     printed above the output.
#'   \item when \code{units = TRUE} and there are no non-white space 
#'     characters in the units attribute, quietly reset \code{units}
#'     to \code{FALSE}
#' }
#' 
#' @seealso \code{\link{print.calculated}}
#' 
#' @author Benjamin Nutter
#' 
#' @export

print.measured <- function(x, 
                           label = getOption("imprecise_label", FALSE),
                           units = getOption("imprecise_units", FALSE),
                           ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "measured",
                          add = coll)
  
  checkmate::assert_logical(x = label,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = units,
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (trimws(attr(x, "units")) == "")
  {
    units <- FALSE
  }
    
  
  lu <- sprintf("%s%s%s",
                if (label) attr(x, "label") else "",
                if (label & units) " " else "",
                if (units) sprintf("(%s)", attr(x, "units")) else "")
  
  if (label || units)
  {
    cat(lu, "\n")
  }
  
  print(x = round(as.numeric(x), 
                  digits = attr(x, "precision")))
}

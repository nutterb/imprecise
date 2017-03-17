#' @name print.calculated
#' @title Print Calculated Vector
#' 
#' @description Print a Calculated Vector
#' 
#' @param x A vector that inherits class \code{calculated}
#' @param label \code{logical(1)}. When \code{TRUE}, the label is printed.
#' @param units \code{logical(1)}. When \code{TRUE}, the unit of measurement
#'   is printed.
#' @param ... Arguments to pass to other methods.
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item The value of \code{x} is printed with the significant figures 
#'     in the \code{sigfig} attribute.
#'   \item When \code{label = TRUE}, the \code{label} attribute is 
#'     printed above the output.
#'   \item When \code{units = TRUE}, the \code{units} attribute is 
#'     printed above the output.
#'   \item when \code{units = TRUE} and there are no non-white space 
#'     characters in the units attribute, quietly reset \code{units}
#'     to \code{FALSE}
#' }
#' 
#' @seealso \code{\link{print.measured}}
#' 
#' @author Benjamin Nutter
#' 
#' @export

print.calculated <- function(x, 
                             label = getOption("imprecise_label", FALSE),
                             units = getOption("imprecise_units", FALSE),
                             ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "calculated",
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
  
  print(x = signif(as.numeric(x), 
                   digits = attr(x, "sigfig")))
}
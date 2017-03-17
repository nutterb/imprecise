#' @name print.label
#' @title Print a Vector with a Label Attribute
#' 
#' @description Vectors with a \code{label} subclass is, by default, 
#'   printed as if it did not have the \code{label} subclass.  The label 
#'   may be printed by using \code{options(imprecise_label = TRUE)}.
#'   
#' @param x An atomic vector to be printed.
#' @param label \code{logical(1)}. When \code{TRUE}, the label is printed 
#'   above the vector.
#' @param units \code{logical(1)}. When \code{TRUE}, the units are printed
#'   above the vector, appended to the label if it is printed.
#' @param ... Additional arguments to pass to other methods.  
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item prints the vector using the method that would be used if the 
#'     vector did not carry the \code{label} subclass.
#'   \item When \code{label = TRUE}, the \code{label} attribute is 
#'     printed above the output.
#'   \item When \code{units = TRUE}, the \code{units} attribute is 
#'     printed above the output.
#'   \item when \code{units = TRUE} and there are no non-white space 
#'     characters in the units attribute, quietly reset \code{units}
#'     to \code{FALSE}
#' }
#' 
#' @author Benjamin Nutter
#' 
#' @export

print.label <- function(x, label = getOption("imprecise_label", FALSE),
                        units = getOption("imprecise_units", FALSE), ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_logical(x = label,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = units,
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  lbl <- get_label(x)
  unt <- get_units(x)
  
  if (is.null(attr(x, "units")))
  {
    units <- FALSE
  }
  
  # Remove the label subclass
  class(x) <- class(x)[!class(x) %in% "label"]
  
  # Remove the label and units attributes
  attr(x, "label") <- attr(x,"units") <- NULL
  
  lu <- sprintf("%s%s%s",
                if (label) lbl else "",
                if (label & units) " " else "",
                if (units) sprintf("(%s)", unt) else "")
  
  if (label || units)
  {
    cat(lu, "\n")
  }
  
  print(x, ...)
}
#' @name abstract_precision
#' @title Determine the abstract precision of a calculated value.
#' 
#' @description Calculated values need to be associated with a precision in 
#'   case they are used in an addition operation.  The abstract precision 
#'   is inferred by expanding the determining the smallest unit of precision 
#'   represented by the significant figures in the value.
#'   
#' @param x A vector of class \code{calculated}
#' 
#' @details The number of significant figures associated with a value must 
#'   be known to infer the precision.  If the value is precise to a 
#'   place larger than a tenth (the ones or higher), the abstract precision
#'   is determined by \code{ceiling(log(x, 10)) - sigfig + 1}.
#'   
#'   If the value is precise to the tenth or smaller, the abstract precision
#'   is determined by \code{ceiling(log(x, 10)) - sigfig}.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Correctly determines the smallest unit of precision in the value.
#'   \item If \code{x} is not of class \code{calculated}, cast an error
#' } 
#'    
#' @author Benjamin Nutter
#' 
#' @seealso \code{\link{count_sigfig}}
#' 
#' @export

abstract_precision <- function(x)
{
  checkmate::assert_class(x = x,
                          classes = "calculated")
  
  signif_x <- signif(x, digits = attr(x, "sigfig"))
  
  as.numeric(ceiling(log(abs(x), 10)) - attr(x, "sigfig"))
}
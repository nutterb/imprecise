#' @name set_sigfig
#' @title Set and Get Significant Figure Attribute of Measured Objects
#' 
#' @description Set or retrieve the sigfig attribute of a measured object.
#' 
#' @param x An object of class \code{calculated}.
#' @param sigfig A vector of integerish values.
#' 
#' @section Functional Requirements:
#' \code{set_sigfig}\cr
#' \enumerate{
#'   \item If \code{x} is not a \code{calculated} object, cast a warning and 
#'     return the vector unchanged.
#'   \item Correctly resets the sigfig attribute.
#'   \item If \code{length(x)} is not a multiple of \code{length(sigfig)},
#'     cast a warning.
#'   \item If \code{length(sigfig)} is greater than \code{length(x)},
#'     cast an error.
#'   \item If and \code{sigfig} is less than 1, cast an error.
#' }
#' 
#' \code{get_sigfig}\cr
#' \enumerate{
#'   \item Retrieves the correct precision values for a \code{calculated} object.
#'   \item If \code{x} is not \code{calculated}, return \code{NULL} and 
#'     cast a warning.
#' }
#' 
#' @seealso \code{\link{set_precision}}, \code{\link{get_precision}}, 
#'   \code{\link{set_label}}, \code{\link{get_label}},
#'   \code{\link{set_units}}, \code{\link{get_units}}
#' 
#' @author Benjamin Nutter
#' 
#' @export

set_sigfig <- function(x, sigfig)
{
  if (!inherits(x, "calculated"))
  {
    warning("Significant Figures can only be set for `calculated` values")
    return(x)
  }
  
  checkmate::assert_integerish(x = sigfig,
                               lower = 1,
                               max.len = length(x))
  
  if (length(x) %% length(sigfig) != 0)
  {
    warning("length(x) is not a multiple of length(sigfig). ",
            "Recycling may not return expected results.")
  }
  
  attr(x, "sigfig") <- sigfig
  attr(x, "precision") <- abstract_precision(x)
  
  x
}

#' @rdname set_sigfig
#' @export

get_sigfig <- function(x)
{
  if (!inherits(x, "calculated"))
  {
    warning("Significant figures can only be retrieved for `calculated` class")
    return(NULL)
  }
  
  attr(x, "sigfig")
}
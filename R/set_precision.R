#' @name set_precision
#' @title Set and Get Precision Attribute of Measured Objects
#' 
#' @description Set or retrieve the precision attribute of a measured object.
#' 
#' @param x An object of class \code{measured}.
#' @param precision A vector of integerish values
#' 
#' @section Functional Requirements:
#' \code{set_precision}\cr
#' \enumerate{
#'   \item If \code{x} is not a \code{measured} object, cast a warning and 
#'     return the vector unchanged.
#'   \item Correctly resets the precision attribute.
#'   \item If \code{length(x)} is not a multiple of \code{length(precision)},
#'     cast a warning.
#'   \item If \code{length(precision)} is greater than \code{length(x)},
#'     cast an error.
#' }
#' 
#' \code{get_precision}\cr
#' \enumerate{
#'   \item Retrieves the correct precision values for \code{measured} 
#'       and \code{calculated} objects.
#'   \item If \code{x} is not \code{measured} or \code{calculated}, 
#'       return \code{NULL} and cast a warning.
#' }
#' 
#' @seealso \code{\link{set_sigfig}}, \code{\link{get_sigfig}}, 
#'   \code{\link{set_label}}, \code{\link{get_label}},
#'   \code{\link{set_units}}, \code{\link{get_units}}
#' 
#' @author Benjamin Nutter
#' 
#' @export

set_precision <- function(x, precision)
{
  if (!inherits(x, "measured"))
  {
    warning("Precision can only be set for `measured` values")
    return(x)
  }
  
  checkmate::assert_integerish(x = precision,
                               max.len = length(x))
  
  if (length(x) %% length(precision) != 0)
  {
    warning("length(x) is not a multiple of length(precision). ",
            "Recycling may not return expected results.")
  }

  attr(x, "precision") <- precision
  
  x
}

#' @rdname set_precision
#' @export

get_precision <- function(x)
{
  get_precision_internal(x, handle_constant = FALSE)
}

# Unexported --------------------------------------------------------
#' @name get_precision_internal
#' @title Internal Get Precision Function
#' 
#' @description At times, I need \code{get_precision} to return a vector of 
#'   \code{-Inf}, to indicate the precision of a constant (a standard 
#'   numeric or double vector).  Using a separate internal function allows
#'   me this flexibility without exposing the \code{handle_constant}
#'   argument to the user.  \code{get_precision} then references this 
#'   function to avoid duplication of code.
#'
#' @param x A numeric vector.  The behavior of the function will vary
#'   for vectors that are not \code{measured} or \code{calculated}
#'   depending on the value of \code{handle_constant}
#' @param handle_constant \code{logical(1)}. This argument is ignored 
#'   when \code{x} is either \code{measured} or \code{calculated}. For
#'   other numeric types, when \code{FALSE}, \code{NULL} is returned; 
#'   otherwise, a vector of \code{-Inf} the length of \code{x} is 
#'   returned. This vector suggests that the values are considered
#'   constants (infinite precision).

get_precision_internal <- function(x, handle_constant = FALSE)
{
  if (!inherits(x, "measured") & !inherits(x, "calculated"))
  {
    if (handle_constant)
    {
      checkmate::assert_logical(x = handle_constant,
                                len = 1)
      return(rep(-Inf, length(x)))
    }
    else
    {
      warning("precision can only be retrieved for `measured` and `calculated` classes")
      return(NULL)
    }
  }
  
  attr(x, "precision")
}
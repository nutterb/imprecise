#' @name as_measured
#' @title Convert to Measured Object
#' 
#' @description Measured values require knowledge of the limitations of the 
#'   measuring instrument.  Measured value objects maintain an additional 
#'   attribute that specifies the precision of the measurement for use 
#'   in printing and downstream calculations.
#'   
#' @param x For \code{as_measured} A vector of either class \code{numeric} 
#'   or \code{double}. Vectors of other classes may be given, but they will 
#'   be returned unchanged.  For \code{is_measured} an object of to be tested.
#' @param precision A vector of integerish values. This vector will be recycled 
#'   over the length of \code{x}.  The value specifies the precision of the 
#'   measurement.  See Details.
#' @param label \code{character(1)}. The human-friendly label for the vector.
#' @param units \code{character(1)}. Gives the unit of measurement.
#' 
#' @details Precision is specified using the exponent of a power of ten.  
#'   A measurement that is precise to the ones place is specified by 
#'   \code{0} (1 * 10 ^ 0).  Precision to the tens place is specified by 
#'   \code{1} (1 * 10 ^ 1).  Precision to the tenths place specified by 
#'   \code{-1} (1 * 10 ^ -1). 
#'   
#'   This specification pattern is effectively the reverse of the 
#'   \code{digits} argument of the \code{\link{round}} function.
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item  If \code{x} is \code{numeric} or \code{double}, return a vector
#'     with \code{precision}, \code{label}, and \code{units} attributes.
#'   \item If \code{precision} is not integerish, cast an error.
#'   \item \code{precision} may have length no greater than \code{x}.
#'   \item If \code{length(x)} is not a multiple of \code{length(precision)}, cast a 
#'     warning.
#'   \item If \code{label} is not a character vector with length no greater 
#'     than 1, cast an error.
#'   \item If \code{units} is not a character vector with length no greater 
#'     than 1, cast an error.
#'   \item If \code{x} is an integer, cast a warning that no action is 
#'     taken because integers have no measurement imprecision.
#'   \item If \code{x} is not \code{numeric}, \code{double}, or \code{integer},
#'     cast a warning that no action is taken.
#'   \item If \code{x} is not \code{numeric} or \code{double},
#'     the vector returned is identical to \code{x}
#' }
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' as_measured(rnorm(10), precision = -2)
#' 
#' @export

as_measured <- function(x, precision, 
                        label = character(0), 
                        units = character(0))
{
  checkmate::assert_atomic_vector(x = x)
  
  if (is.integer(x))
  {
    warning("Integer values have no measurement imprecision. No action taken")
    return(x)
  }
  else if (!is.numeric(x))
  {
    warning("Non-numeric values have no measurement imprecision. No action taken")
    return(x)
  }
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_integerish(x = precision,
                               max.len = length(x),
                               add = coll)
  
  checkmate::assert_character(x = label,
                              max.len = 1,
                              add = coll)
  
  checkmate::assert_character(x = units,
                              max.len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (length(x) %% length(precision) != 0)
  {
    warning(
      "`length(x)` is not a multiple of `length(precision)`. ",
      "Precision may not have been applied as intended."
    )
  }
  
  if (!length(label))
  {
    label <- ""
  }
  
  if (!length(units))
  {
    units <- ""
  }
  
  precision <- rep(precision, length.out = length(x))
  
  structure(x,
            precision = precision,
            label = label,
            units = units,
            class = c("measured", "label", class(x)))
}

#' @rdname as_measured
#' @export

as.measured <- as_measured

#' @rdname as_measured
#' @export

is_measured <- function(x)
{
  inherits(x, "measured")
}

#' @rdname as_measured
#' @export

is.measured <- is_measured

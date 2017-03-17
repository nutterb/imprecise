#' @name as_calculated
#' @title Conver to Calculated Object
#' 
#' @description Values that result from calculations involving measurements
#'   are subject to imprecision based on the imprecision of the values 
#'   used in the calculation.  The \code{calculated} class tracks the 
#'   the abstract precision and the significant figures associated with the 
#'   calculated values, allowing them to be used in subsequent calculations 
#'   or for printing.
#'   
#' @param x For \code{as_calculated} A vector of either class \code{numeric} 
#'   or \code{double}. Vectors of other classes may be given, but they will 
#'   be returned unchanged.  For \code{is_calculated} an object of to be tested.
#' @param sigfig A vector of integerish values. This vector will be recycled 
#'   over the length of \code{x}.  The value specifies the number of 
#'   significant figures for the calculated value.
#' @param label \code{character(1)}. The human-friendly label for the vector.
#' @param units \code{character(1)}. Gives the unit of measurement.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item  If \code{x} is \code{numeric} or \code{double}, return a vector
#'     with \code{sigfig}, \code{label}, and \code{units} attributes.
#'   \item If \code{sigfig} is not integerish, cast an error.
#'   \item \code{sigfig} may have length no greater than \code{x}.
#'   \item If \code{length(x)} is not a multiple of \code{length(sigfig)}, cast a 
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
#'   \item If any \code{sigfig} is less than 1, cast an error.
#' }
#' 
#' @author Benjamin Nutter
#' 
#' @export

as_calculated <- function(x, sigfig,
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
  
  checkmate::assert_integerish(x = sigfig,
                               lower = 1,
                               max.len = length(x),
                               add = coll)
  
  checkmate::assert_character(x = label,
                              max.len = 1,
                              add = coll)
  
  checkmate::assert_character(x = units,
                              max.len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (length(x) %% length(sigfig) != 0)
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
  
  sigfig <- rep(sigfig, length.out = length(x))
  
  x <- structure(x,
                 sigfig = sigfig,
                 precision = rep(NA, length(x)),
                 label = label,
                 units = units,
                 class = c("calculated", "label", class(x)))
  
  attr(x, "precision") <- abstract_precision(x)
  
  x
}

#' @rdname as_calculated
#' @export

as.calculated <- as_calculated

#' @rdname as_calculated
#' @export

is_calculated <- function(x)
{
  inherits(x, "calculated")
}

#' @rdname as_calculated
#' @export

is.calculated <- is_calculated
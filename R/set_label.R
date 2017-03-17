#' @name set_label
#' @title Set and Get Label and Units Attributes
#' 
#' @description Set and retrieve labels and units from vectors.  While
#'   the \code{measured} and \code{calculated} classes explicitly 
#'   define an attribute for the label and units, they may be added to 
#'   any atomic vector.  These behave similarly to 
#'   
#' @param x An atomic vector.
#' @param label \code{character(1)} for the \code{label} attribute.
#' @param units \code{character(1)} for the \code{units} attribute.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item If \code{x} is not an atomic vector, cast an error.
#'   \item For the \code{set} methods, assign a new attribute, 
#'     \code{label} or \code{units} and give it 
#'     the value of the corresponding argument.
#'   \item For \code{get_label}, if the \code{label} attribute is 
#'     \code{NULL} or doesn't exist, return the name of \code{x}
#'   \item For \code{get_units}, if the \code{units} attribute is 
#'     \code{NULL} or doesn't exist, return an empty string. 
#' }
#' 
#' @seealso \code{\link{set_precision}}, \code{\link{get_precision}},
#'   \code{\link{set_sigfig}}, \code{\link{get_sigfig}}
#'   
#' @author Benjamin Nutter
#' 
#' @export

set_label <- function(x, label)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_atomic_vector(x = x,
                                  add = coll)
  
  checkmate::assert_character(x = label,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  attr(x, "label") <- label
  
  if (!"label" %in% class(x))
  {
    class(x) <- c("label", class(x))
  }
  
  x
}

#' @rdname set_label
#' @export

get_label <- function(x)
{
  checkmate::assert_atomic_vector(x = x)
  
  lbl <- attr(x, "label")
  
  if (is.null(lbl))
  {
    return(deparse(substitute(x)))
  }
  
  lbl
}

#' @rdname set_label
#' @export

set_units <- function(x, units)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_atomic_vector(x = x,
                                  add = coll)
  
  checkmate::assert_character(x = units,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  attr(x, "units") <- units
  
  if (!"label" %in% class(x))
  {
    class(x) <- c("label", class(x))
  }
  
  x
}

#' @rdname set_label
#' @export

get_units <- function(x)
{
  checkmate::assert_atomic_vector(x = x)
  
  unit <- attr(x, "units")
  
  if (is.null(unit))
  {
    return("")
  }
  
  unit
}
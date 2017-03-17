#' @name format.measured
#' @title Format Measured and Calculated Objects
#' 
#' @description Print \code{measured} and \code{calculated} objects to 
#'   either the appropriate precision or significant figures. The primary
#'   motivation for these functions is to print measured and calculated 
#'   values appropriately in data frames. 
#'   
#' @param x An object of class \code{measured} or \code{calculated}
#' @param ... Additional arguments to pass to other methods.
#' 
#' @details These methods merely round the values to the appropriate place
#'   prior to passing the values to their default \code{format} methods. 
#' 
#' @author Benjamin Nutter
#' 
#' @seealso \code{\link{format}}
#' 
#' @export

format.measured <- function(x, ...)
{
  format.default(
    round(as.numeric(x), 
          digits = attr(x, "precision") * -1), 
    ...
  )
}

#' @rdname format.measured
#' @export

format.calculated <- function(x, ...)
{
  format.default(
    signif(as.numeric(x),
           digits = attr(x, "sigfig")),
    ...
  )
}

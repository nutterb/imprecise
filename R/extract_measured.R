#' @name extract_measured
#' @title Extraction Methods for Measured and Calculated Values
#' 
#' @description Extraction and replacement methods for measured and
#'   calculated objects.  
#'   
#' @param x A \code{measured} or \code{calculated} object.
#' @param i A vector of indices to extract or replace
#' @param value replacement values for \code{x}. If \code{values} has 
#'   \code{precision} or \code{sigfig} attributes, they will replace the 
#'   attributes at positions \code{i} in \code{x}. Otherwise, only the 
#'   attributes are left as they are.
#'   
#' @section Functional Requirements:
#' Extraction Method \cr
#' \enumerate{
#'   \item The values at positions \code{i} are extracted and retain 
#'     their attributes.
#' }
#' Replacement Method \cr
#' \enumerate{
#'   \item When \code{value} has a \code{precision} attribute, the 
#'       \code{precision} attribute of \code{x} at positions \code{i}
#'       are replaced by the \code{precision} attribute of \code{value}
#'   \item When \code{value} has no \code{precision} attribute, the 
#'       value of \code{x} is replaced with no change to the 
#'       \code{precision} attribute of \code{x}.
#'   \item When \code{value} has a \code{sigfig} attribute, the 
#'       \code{sigfig} attribute of \code{x} at positions \code{i}
#'       are replaced by the \code{sigfig} attribute of \code{value}
#'   \item When \code{value} has no \code{sigfig} attribute, the 
#'       value of \code{x} is replaced with no change to the 
#'       \code{sigfig} attribute of \code{x}.
#'   \item Replacement works with named vectors.
#' }
#'   
#' @author Benjamin Nutter
#' 
#' @export

`[.measured` <- function(x, i)
{
  structure(
    as.numeric(x)[i],
    precision = attr(x, "precision")[i],
    label = attr(x, "label"),
    units = attr(x, "units"),
    names = names(x),
    class = class(x)
  )
}

#' @rdname extract_measured 
#' @export

`[<-.measured` <- function(x, i, value)
{
  if (is.character(i)) i <- match(i, names(x))
  
  x_new <- `[<-`(as.numeric(x), i, value)
  
  if (!is.null(attr(value, "precision")))
  {
    attr(x, "precision")[i] <- attr(value, "precision")
  }
  
  structure(
    x_new,
    precision = attr(x, "precision"),
    label = attr(x, "label"),
    units = attr(x, "units"),
    names = names(x),
    class = class(x)
  )
}

#' @rdname extract_measured
#' @export

`[.calculated` <- function(x, i)
{
  structure(
    as.numeric(x)[i],
    precision = attr(x, "precision")[i],
    sigfig = attr(x, "sigfig")[i],
    label = attr(x, "label"),
    units = attr(x, "units"),
    names = names(x),
    class = class(x)
  )
}

#' @rdname extract_measured 
#' @export

`[<-.calculated` <- function(x, i, value)
{
  if (is.character(i)) i <- match(i, names(x))
  
  x_new <- `[<-`(as.numeric(x), i, value)
  
  if (!is.null(attr(value, "precision")))
  {
    attr(x, "precision")[i] <- attr(value, "precision")
  }
  
  if (!is.null(attr(value, "sigfig")))
  {
    attr(x, "sigfig")[i] <- attr(value, "sigfig")
  }
  
  structure(
    x_new,
    precision = attr(x, "precision"),
    sigfig = attr(x, "sigfig"),
    label = attr(x, "label"),
    units = attr(x, "units"),
    names = names(x),
    class = class(x)
  )
}

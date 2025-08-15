################################################################################
###
### Functions useful for handling data frames
###
################################################################################

##' Create SAS FIRST. and LAST. variables
##'
##' Mimics the automatic DATA step variables 'FIRST.' and
##' 'LAST.'. This is an order-sensitive operation; it does not do what
##' \code{\link[base]{duplicate}} does.
##'
##' The result can be bound to the input as a matrix or data frame if
##' \code{add} is \code{TRUE}. The type of the return value depends on
##' \code{result}. If \code{add} = \code{FALSE}, the return value is a
##' matrix of logical values if \code{result} is \code{"both"}, or a
##' vector otherwise. If \code{add} is \code{TRUE} the result is
##' always a data frame because this is the only way to combine the
##' logical 'FIRST.'/'LAST.' vectors with whatever mode \code{x} may
##' be.
##'
##' @param x Vector.
##' @param result Return \dQuote{FIRST.}, \dQuote{LAST.}, or both?
##' @param add \code{\link[base]{cbind}} the result to \code{x}?
##' @return A vector, matrix or data frame (see \dQuote{Details}).
##' @author Mark Wheldon
##'
##' @examples
##' x <- expand.grid(col = c("red", "green", "blue"), number = 1:4)
##' first_last(x$number)
##' @export
first_last <- function(x, result = c("both", "first", "last"), add = FALSE, ...) {
    result <- match.arg(result)
    z <- c(TRUE, x[-1] != x[-length(x)])
    if (identical(result, "first")) out <- z
    else if (identical(result, "last")) out <- c(z[-1], TRUE)
    else out <- cbind(first = z, last = c(z[-1], TRUE))
    if (add) return(data.frame(x, out))
    else return(out)
}


##' Create group numbers based on an ordered vector.
##'
##' This is useful for a time series of values where duplicates may
##' occur, but the recurrence of a duplicate should indicate the start
##' of a new group. Uses \code{first_last}.
##'
##' @param x Vector.
##' @param add \code{\link[base]{cbind}} the result to \code{x}?
##' @return A logical vector if \code{add = FALSE} otherwise a data frame.
##' @author Mark Wheldon
##' @export
number_groups <- function(x, add = FALSE) {
    out <- cumsum(first_last(x, result = "first", add = FALSE))
    if (add) return(data.frame(x, out))
    else return(out)
}


##' Find breaks in an ordered numeric vector
##'
##' Elements that differ from their previous element by more
##' \code{diff_tol} are flagged.
##'
##' @param x Vector.
##' @param diff_tol The difference that marks a break in the sequence.
##' @param add \code{\link[base]{cbind}} the result to \code{x}?
##' @return Vector (\code{add = FALSE}) or data frame (\code{add = TRUE}).
##' @author Mark Wheldon
##' @export
find_breaks <- function(x, diff_tol = 1, add = FALSE) {
    stopifnot(is.numeric(x))
    out <- as.logical(c(1, diff(x) != diff_tol))
    if (add) return(data.frame(x, out))
    else return(out)
}


##' Create group numbers based on sequence breaks
##'
##' Like \code{number_groups} but uses \code{find_breaks} to determine
##' group membership.
##'
##' @param x Vector.
##' @param add \code{\link[base]{cbind}} the result to \code{x}?
##' @param diff_tol The difference that marks a break in the sequence.
##' @return Vector (\code{add = FALSE}) or data frame (\code{add = TRUE}).
##' @author Mark Wheldon
##' @export
number_sequence_groups <- function(x, add = FALSE) {
    out <- cumsum(find_breaks(x, add = FALSE))
    if (add) return(data.frame(x, out))
    else return(out)
}


################################################################################
###
### Basic text manipulation functions
###
################################################################################

##' Makes a character string into a valid filename (for Windows, at least).
##'
##' Replaces forbidden characters with underscore (default).
##'
##' @param x Character sting to convert.
##' @param safe Character used to replace disallowed.
##' @param disallowed Regular expression of disallowed characters. Passed to
##'     \code{gsub(..., ignore.case = FALSE, perl = FALSE, fixed = FALSE,
##'     useBytes = FALSE)}.
##' @return 'x' converted to a valid filename.
##' @author Mark Wheldon.
##' @export
makeFileName <- function(x, safe = "_", disallowed = "/|\\*|<|\\\\|>|:|\\||\\?|\"") {
    gsub(disallowed, safe, x = x)
    }


##' Pretty number
##'
##' Formats decimal numbers 'prettily'
##'
##' @param z Number to be fomatted
##' @param d Number of digits to print.
##' @param bm \dQuote{\code{big.mark}}.
##' @param to_text Logical; convert numbers below 10 to text.
##' @param capitalize If \code{to_text} is \code{TRUE}, should it be capitalized?
##'
##' @return Text string containing formatted number.
##' @author Mark Wheldon
##' @export
pn <- function(z, d = 2, bm = ",", to_text = FALSE, capitalize = FALSE) {
    if(length(z) > 1) {
        sapply(z, "pn", d = d, bm = bm, to_text = to_text, capitalize = capitalize)
    } else {
        y <- formatC(z, digits = d, big.mark = bm)
        if (grepl(pattern = "e\\+", x = y)) {
            out <- pn(z, d = d + 1, bm = bm, to_text = to_text, capitalize = capitalize)
        } else if (grepl(pattern = "e\\-", x = y)) {
            e.part <-
                as.numeric(sapply(strsplit(y, "e\\-"), "[[", 2))
            out <- formatC(z, digits = e.part, format = "f", big.mark = bm)
        } else if (to_text && identical(as.double(z), as.double(round(z))) && z < 10 && z >= 0) {
            out <- c("zero", "one", "two", "three", "four", "five", "six",
                     "seven", "eight", "nine")[z + 1]
            if (capitalize) out <- chartr(old = substr(out, 1, 1),
                                          new = toupper(substr(out, 1, 1)),
                                          x = out)
        } else {
            out <- y
        }
        return(gsub("^\\s+|\\s+$", "", out)) # remove whitespace at each end
    }
}


##' Print a character vector as text
##'
##' The first n-1 elements are collapsed with separating commas, then
##' an "and" is inserted, followed by the last element.
##'
##' @param x Character vector.
##' @return Character string.
##' @author Mark Wheldon
##' @export
vector_to_text <- function(x) paste0(paste(head(x, -1), collapse = ", "), ", and ", tail(x, 1))

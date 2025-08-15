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


## pretty number
pn <- function(z, d = 2) {
    if(length(z) > 1) {
        sapply(z, "pn", d = d)
    } else {
        y <- formatC(z, digits = d)
        if(grepl(pattern = "e\\+", x = y)) pn(z, d = d + 1)
        else if(grepl(pattern = "e\\-", x = y)) {
            e.part <- as.numeric(sapply(strsplit(y, "e\\-"), "[[", 2))
            return(formatC(z, digits = e.part, format = "f"))
        }
        else return(y)
    }
}

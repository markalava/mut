################################################################################
###
### DATE CREATED: 2023-03-06
###
### AUTHOR: Mark Wheldon
###
### PROJECT: mut
###
### DESCRIPTION: Helper functions for RMD compilation.
###
###-----------------------------------------------------------------------------
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


##' Augment 2x2 tables with margins and proportions.
##'
##' Simple function that adds rows and columns to an existing 2x2
##' table with totals and proportions.
##'
##' Currently, only the proportion of the total is supported but this
##' could easily be extended to row and column totals.
##'
##' Tables will need to be formatted for printing, e.g., with
##' \code{\link[knitr]{kable}}.
##'
##' @details
##' \code{digits} can be a single number, a vector or a list of
##' numbers or vectors.  If a list, it must have two elements named
##' \code{\dQuote{count}} and \code{\dQuote{prop}} to specify how
##' counts and proportions are rounded.
##'
##' Putting proportions in parentheses with \code{prop_in_paren} will
##' cause the output to be a \code{matrix} with character valued
##' elements.
##'
##' @section Last Updated: 2022-12-14
##'
##' @param x A 2x2 \code{\link{table}}.
##' @param add_margins Logical; add table margins?
##' @param add_prop Logical; add proportions of total?
##' @param digits Number of digits to display; passed to
##'     \code{round}. See \dQuote{Details}.
##' @param sum_label Name of the \dQuote{Sum} rows and columns if
##'     \code{add_margin} is \code{TRUE}.
##' @param prop_in_paren Logical; put proportions in parentheses. See
##'     \dQuote{Details}.
##' @param prop_scale Should proportions be expressed as proportions
##'     or percentages?
##' @param prop_label Label for row names.
##' @return A \code{\link{table}}, unless \code{prop_in_paren} is
##'     \code{TRUE} in which case a \code{\link{matrix}}.
##' @author Mark Wheldon
##'
##' @seealso \code{\link[gmodels]{CrossTable}} in the \pkg{gmodels}
##'     package, \code{\link[knitr]{kable}} in the \pkg{knitr}
##'     package, and package \pkg{kableExtra}. Especially see
##'     \code{\link[summarytools]{ctable}} in package
##'     \pkg{summarytools}.
##' @export
contingency_table <- function(x, add_margins = TRUE, add_prop = TRUE,
                              digits = list(count = getOption("digits"),
                                            prop = getOption("digits")),
                              sum_label = "Total",
                              prop_in_paren = TRUE,
                              prop_scale = c("perc", "prop"),
                              prop_label = paste0(".", prop_scale)) {
    ## x must be a 2x2 table
    stopifnot(is.table(x))
    stopifnot(identical(dim(x), c(2L, 2L)))

    ## digits
    if (!is.list(digits)) {
        stopifnot(is.numeric(digits))
        digits <- list(count = digits, prop = digits)
    } else {
        stopifnot(all(c("count", "prop") %in% names(digits)))
    }

    ## Proportions and margins
    if (add_prop) {
        prop_scale <- match.arg(prop_scale)
        if (identical(prop_scale, "prop")) prop_mult <- 1
        else prop_mult <- 100
        x_prop <- x / sum(x) * prop_mult
        if (add_margins) x_prop <- addmargins(x_prop)
        if (identical(length(digits$prop), 1L)) {
            x_prop <- round(x_prop, digits = digits$prop)
         } else {
            stopifnot(identical(length(digits$prop), ncol(x_prop)))
            for (j in 1:ncol(x_prop)) {
                x_prop[, j] <- round(x_prop[, j], digits$prop[j])
            }
        }
    }
    if (add_margins) x <- addmargins(x)
    if (identical(length(digits$count), 1L)) {
        x <- round(x, digits = digits$count)
    } else {
        stopifnot(identical(length(digits$count), ncol(x)))
        for (j in 1:ncol(x)) {
            x[, j] <- round(x[, j], digits$prop[j])
        }
    }

    ## Build table
    if (add_prop) {
        out_nrow <- dim(x)[1] + dim(x_prop)[1]
        out_row_names <-
            paste(rep(rownames(x), each = 2), c("", prop_label), sep = "")
    } else {
        out_nrow <- dim(x)[1]
    }
    out <- matrix(NA, nrow = out_nrow, ncol = dim(x)[2])
    for (i in seq_len(nrow(x))) {
        k <- seq(from = 1, by = 2, length.out = nrow(x))
        out[c(k[i], k[i] + 1), ] <- rbind(x[i, ], x_prop[i, ])
    }
    out <- as.table(out)
    dimnames(out) <-
        lapply(list(out_row_names, colnames(x)),
               function(z) gsub("Sum", sum_label, z))
    names(dimnames(out)) <- names(dimnames(x))

    ## Format if needed
    if (prop_in_paren && add_prop) {
        out <- formatC(out)
        for (i in seq(from = 2, to = nrow(out), by = 2)) {
            for (j in 1:ncol(out)) {
                out[i, j] <- paste0("(", out[i, j], ")")
            }
        }
    }
    return(out)
}


#' Create a table from an \code{ftable} object using \code{kbl}
#'
#' Formats an \code{\link{ftable}} object and applies
#' \code{\link[kableExtra]{kbl}} with some sensible
#' options. Additional styling can be done on the result by piping into
#' \pkg{kableExtra} functions.
#'
#' If you wish to provide a customized text matrix, e.g., you have
#' applied \code{\link{format.ftable}} and made some alterations, set
#' \code{format} to \code{FALSE}.
#'
#' @param x An \code{ftable} object, or a text matrix or data frame.
#' @param digits Number of numeric digits to keep.
#' @param bold_row.vars Logical; should the column of \code{row.vars} be emboldened?
#' @return The result of appling \code{\link[kableExtra]{kbl}}.
#' @author Mark Wheldon
##' @export
kbl_ftable <- function(x, digits = 2, bold_row.vars = FALSE) {
    stopifnot(inherits(x, "ftable"))
    stopifnot(identical(length(attr(x, "row.vars")), 2L))
    stopifnot(identical(length(attr(x, "col.vars")), 1L))
    x <- format(x, digits = digits, quote = FALSE,
                method = "row.compact")
    col_headers <- x[1,]
    x <- as.data.frame(x[-1, -3])
    colnames(x) <- col_headers[-3]
    x <- kableExtra::kbl(x, position = "bpht")
    x <- kableExtra::add_header_above(x, setNames(c(2, 2), c(" ", col_headers[3])))
    if (bold_row.vars) x <- kableExtra::column_spec(x, 1:2, bold = TRUE)
    return(x)
}


##' Print median and uncertainty interval from FPEM results data frame
##'
##' Given a data frame of the form returned by
##' 'FPEMglobal.aux::get_csv_res', this function will
##' return a string of the form \dQuote{[median] (95 percent UI: [lower], [upper])}.
##'
##' @param x Data frame returned by 'FPEMglobal.aux::get_csv_res'.
##' @param name Location name.
##' @param year Year. If not a 'mid-year', will have 0.5 added to it.
##' @param indicator Name of indicator to report.
##' @param digits Number of decimal places to include.
##' @return String.
##' @author Mark Wheldon
##' @export
get_fpem_res_and_ui <- function(x, name, year, indicator, digits = 1, include_UI = FALSE) {
    x <- as.data.frame(x)
    stopifnot(name %in% x$name)
    stopifnot(year %in% x$year || year + 0.5 %in% x$year)
    stopifnot(identical(length(unique(x$stat)), 1L))
    stopifnot(indicator %in% colnames(x))

    if (identical(as.double(year), as.double(floor(year))))
        year <- year + 0.5

    idx <- x$name == name & x$year == year
    median <- round(x[idx & x$percentile == 0.5, indicator] * 100, digits)
    out <- paste0(formatC(x[idx & x$percentile == 0.5, indicator] * 100, digits, format = "f"),
                  " percent")
    if (include_UI)
        out <- paste0(out,
                      " (95% UI: [",
                      formatC(x[idx & x$percentile == 0.025, indicator] * 100, digits, format = "f"),
                      ", ",
                      formatC(x[idx & x$percentile == 0.975, indicator] * 100, digits, format = "f"),
                      "])")
    return(out)
}


##' Get plateau period for a country
##'
##' @param x \code{main_fp_plateau_only_results_table} from \code{make_all_results_list}.
##' @param name Country name.
##' @return Character string.
##' @author Mark Wheldon
##' @export
get_plateau_years <- function(x, name) {
    return(x[x$name == name, ]$year_range)
}


##' Print country name with plateau period in parentheses
##'
##' Prints the name of the country followed by the plateau years in parentheses.
##'
##' @param x \code{main_fp_plateau_only_results_table} from \code{make_all_results_list}.
##' @param name Country name.
##' @return Character string.
##' @author Mark Wheldon
##' @export
print_c_plateau_years <- function(x, name) {
    return(paste0(name, " (", get_plateau_years(x, name), ")"))
}



##' Use formal names of countries
##'
##' For use in tables.
formalize_country_names <- function(x, col_name = "name",
                                    from = "Cote d'Ivoire", to = "Côte d'Ivoire") {
    stopifnot(is.data.frame(x))
    stopifnot(identical(length(from), length(to)))
    for (i in seq_along(from)) {
        x[[col_name]][x[[col_name]] == from[i]] <- to[i]
    }
    return(x)
}

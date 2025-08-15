################################################################################
###
### Functions useful for tabulations
###
################################################################################


##' Blank out cells in a table column
##'
##' Blanks out cells in a column that are the same as cell above.  In
##' tabulations, some columns are block titles that apply to several rows
##' beneath them. This function replaces cells with blanks if they are
##' duplicates of cells above them.
##'
##' Argument \code{cols} is passed to \code{\link{[}}} as second
##' argument (i.e., \code{x[,j]}) so it can be either numeric or
##' character, as long as it selects a column of \code{x}.
##'
##' @param x Data frame, or object that can be coerced to one with
##'     \code{\link{as.data.frame}}.
##' @param cols Vector of column names or positions. Each element will be passed
##'     to \code{\link{[}} one-by-one.
##' @param reset_row.names Logical; should row names be set to \code{NULL} after
##'     blanking?
##' @return Data frame.
##' @author Mark C. Wheldon
##' @export
blankCells <- function(x, cols, reset_row.names = FALSE) {
    istib <- tibble::is_tibble(x)
    x <- as.data.frame(x)
    for (j in cols) {
        if (is.factor(x[, j])) x[, j] <- as.character(x[, j])
        z <- c("", head(x[, j], -1))
        x[x[, j] == z, j] <- ""
    }
    if (reset_row.names) row.names(x) <- NULL
    if (istib) x <- tibble::as_tibble(x)
    return(x)
}


##' Convert three columns into a single median (confidence intervals) column
##'
##' Given three columns containing the median and lower and upper
##' bounds of a confidence interval, combine them into a single column
##' formatted as \dQuote{\var{median} (\var{lower}, \var{upper})}.
##'
##' @param x Data frame, or object that can be coerced to one with
##'     \code{\link{as.data.frame}}.
##' @param cols List of triplets of column names or positions in the
##'     order \sQuote{median}, \sQuote{lower}, \sQuote{upper}. If the
##'     elements are named, the names will be used as names of the
##'     newly created columns.
##' @param digits
##' @return Data frame.
##' @author Mark C. Wheldon
makeCICols <- function(x, cols,
                       digits = getOption("digits"),
                       out_names = paste("CI", seq_along(cols), sep = "_")) {
    if (is.null(out_names)) {
        out_names <- paste("CI", seq_along(cols), sep = "_")
    } else {
        if (!identical(length(out_names), length(cols)))
            stop("'cols' and 'out_names' must be the same length.")
    }
    if (identical(length(digits), 1L)) digits <- rep(digits, length(cols))
    istib <- tibble::is_tibble(x)
    x <- as.data.frame(x)

    ## Create new columns; put them in the location of the first
    ## column in each triplet.
    for (j in seq_along(cols)) {
        J <- cols[[j]]
        x_J_1 <- x[, J[[1]]] # The median!
        x_J_2 <- x[, J[[2]]]
        x_J_3 <- x[, J[[3]]]
        is_na_1 <- is.na(x_J_1)
        is_na_2 <- is.na(x_J_2) | is.na(x_J_3)
        z <- rep(NA, nrow(x))
        z[!is_na_1 & !is_na_2] <-
            paste0(round(x_J_1[!is_na_1 & !is_na_2], digits[j]), " ",
                    "(", round(x_J_2[!is_na_1 & !is_na_2], digits[j]), ", ",
                   round(x_J_3[!is_na_1 & !is_na_2], digits[j]), ")")
        z[!is_na_1 & is_na_2] <- round(x_J_1[!is_na_1 & is_na_2], digits[j])
        x[, J[[1]]] <- z
        J_1 <- J[[1]]
        if (!is.null(names(cols))) {
            if (!is.numeric(J_1)) J_1 <- which(colnames(x) == J_1)
            colnames(x)[J_1] <- names(cols)[j]
        } else colnames(x)[J_1] <- out_names[j]
    }

    ## Delete the unneeded columns (need to do it from right to left).
    K <- rev(unlist(lapply(cols, "[", -1)))
    for (k in K) x[, k] <- NULL

    if (istib) x <- tibble::as_tibble(x)
    return(x)
}


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
##'
##' @section Last Updated: 2022-12-14
#'
#' @param x An \code{ftable} object, or a text matrix or data frame.
#' @param digits Number of numeric digits to keep.
#' @param bold_row.vars Logical; should the column of \code{row.vars} be emboldened?
#' @return The result of appling \code{\link[kableExtra]{kbl}}.
#' @author Mark Wheldon
kbl_ftable <- function(x, digits = 2, bold_row.vars = FALSE) {
    stopifnot(inherits(x, "ftable"))
    stopifnot(identical(length(attr(x, "row.vars")), 2L))
    stopifnot(identical(length(attr(x, "col.vars")), 1L))
    x <- format(x, digits = digits, quote = FALSE,
                method = "row.compact")
    col_headers <- x[1,]
    x <- as.data.frame(x[-1, -3])
    colnames(x) <- col_headers[-3]
    x <- kableExtra::kbl(x, position = "hbpt")
    x <- kableExtra::add_header_above(x, setNames(c(2, 2), c(" ", col_headers[3])))
    if (bold_row.vars) x <- kableExtra::column_spec(x, 1:2, bold = TRUE)
    return(x)
}


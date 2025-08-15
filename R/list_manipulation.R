################################################################################
###
### TITLE: List Manipulation Functions
###
### DATE CREATED: 2018-07-23
###
### AUTHOR: Mark Wheldon
###
### DESC:
###     Manipulate lists in R.
###
###-----------------------------------------------------------------------------
###
################################################################################

##' Convert Recursive List to Data Frame
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param x A recursive list such that \code{inherits(x,
##'     "data.frame")} is \code{TRUE}.
##' @param id_names Names to identify the original list elements rows
##'     came from. If missing, columns are created automatically.
##' @return A data frame.
##' @author Mark Wheldon.
rec_list_to_df <- function(x, id_names) {
        recursion_f <- function(x) {
            if(!inherits(x[[1]], "data.frame")) {
                x <- lapply(x, "recursion_f")
            }
            x <- Map(function(a, b) {
                data.frame(rec_list_to_df_id = a, b, stringsAsFactors = FALSE)
            },
            names(x), x)
            x <- Reduce(function(a, b) {
                rbind(a, b, stringsAsFactors = FALSE)
            }, x)
            return(x)
        }
        x <- recursion_f(x)
        if(!base::missing(id_names)) {
            colnames(x)[grepl("rec_list_to_df", colnames(x))] <-
                id_names
            }
        return(x)
    }

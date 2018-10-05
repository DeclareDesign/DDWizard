# Common utility functions.
#
# Markus Konrad <markus.konrad@wzb.eu>
# Oct. 2018
#


#' Append `v` to list `l`. Appending is slow, don't use that often!
#' @export
#'
list_append <- function(l, v) {
    l[[length(l)+1]] <- v
    l
}

#' Create an input element for a design argument with name `argname`,
#' default value `argdefault` and type `argtype`.
#' Returns NULL if argument type is not supported.
#' @export
#'
input_elem_for_design_arg <- function(argname, argdefault, argtype) {
    inp_id <- paste0('design_arg_', argname)
    
    if (argtype == 'numeric') {
        return(numericInput(inp_id, argname, value = argdefault))
    } else if (argtype == 'integer') {
        return(numericInput(inp_id, argname, value = argdefault, step = 1))
    }
    
    NULL
}
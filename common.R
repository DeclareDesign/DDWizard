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
#' default value `argdefault`, class `argclass` and type `argtype` (from `typeof()`)
#' Returns NULL if argument type is not supported.
#' @export
#'
input_elem_for_design_arg <- function(argname, argdefault, argclass, argtype) {
    inp_id <- paste0('design_arg_', argname)
    
    if (argclass == 'numeric') {
        return(numericInput(inp_id, argname, value = argdefault, step = 0.1))
    } else if (argclass == 'integer') {
        return(numericInput(inp_id, argname, value = argdefault, step = 1))
    } else if (argclass == 'call' && argtype == 'language') {
        return(textInput(inp_id, argname, value = deparse(argdefault)))
    }
    
    NULL
}

design_arg_value_from_input <- function(inp_value, argdefault, argclass, argtype) {
    if (argclass %in% c('numeric', 'integer')) {
        arg_value <- as.numeric(inp_value)
    } else if (argclass == 'call' && argtype == 'language') {
        #arg_value <- parse(text = inp_value)    # TODO: what here?
        return(NULL)
    } else {
        return(NULL)
    }
    
    return(ifelse(length(inp_value) > 0, arg_value, argdefault))
}

#' Clear the internal list of warnings as produced by calls to `warning()`.
#' Solution from https://stackoverflow.com/q/5725106
clear_warnings <- function() {
    assign("last.warning", NULL, envir = baseenv())
}
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
#' Returns NULL if argument type is not supported.
#' @export
#'
input_elem_for_design_arg <- function(argname, argdefault, argdefinition) {
    inp_id <- paste0('design_arg_', argname)
    
    argclass <- class(argdefault)
    argtype <- typeof(argdefault)
    
    if (argclass %in% c('call', 'name') && argtype %in% c('language', 'symbol')) {
        return(textInput(inp_id, argname, value = deparse(argdefault)))
    } else {
        argmin <- ifelse(is.finite(argdefinition$min), argdefinition$min, NA)
        argmax <- ifelse(is.finite(argdefinition$max), argdefinition$max, NA)
        
        if (argdefinition$class == 'numeric') {
            return(numericInput(inp_id, argname, value = argdefault, step = 0.1, min = argmin, max = argmax))
        } else if (argdefinition$class == 'integer') {
            return(numericInput(inp_id, argname, value = argdefault, step = 1, min = argmin, max = argmax))
        } else if (argdefinition$class == 'character') {
            return(textInput(inp_id, argname, value = argdefault))
        }
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

#' #' Clear the internal list of warnings as produced by calls to `warning()`.
#' #' Solution from https://stackoverflow.com/q/5725106
#' clear_warnings <- function() {
#'     assign("last.warning", NULL, envir = baseenv())
#' }

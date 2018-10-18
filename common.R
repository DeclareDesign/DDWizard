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
    
    inp_elem_constructor <- NULL
    inp_elem_args <- list(
        inp_id,
        argname,
        width = '50%'
    )
    
    argmin <- ifelse(is.finite(argdefinition$min), argdefinition$min, NA)
    argmax <- ifelse(is.finite(argdefinition$max), argdefinition$max, NA)
    
    if (argclass %in% c('call', 'name') && argtype %in% c('language', 'symbol')) {
        inp_elem_args$value = ''
    } else {
        inp_elem_args$value = argdefault
    }
    
    if (argdefinition$class == 'character') {
        inp_elem_constructor <- textInput
    } else if (argdefinition$class %in% c('numeric', 'integer')) {
        inp_elem_constructor <- numericInput
        inp_elem_args$min = argmin
        inp_elem_args$max = argmax
        
        if (argdefinition$class == 'numeric') {
            inp_elem_args$step = 0.1
        } else if (argdefinition$class == 'integer') {
            inp_elem_args$step = 1
        }
    }
    
    if (!is.null(inp_elem_constructor)) {
        return(do.call(inp_elem_constructor, inp_elem_args))
    }
    
    NULL
}

design_arg_value_from_input <- function(inp_value, argdefault, argdefinition, argclass, argtype) {
    if (argclass %in% c('numeric', 'integer')) {
        arg_value <- as.numeric(inp_value)
    } else if (argclass %in% c('call', 'name') && argtype %in% c('language', 'symbol')) {
        if (length(inp_value) > 0 && !is.na(inp_value) && !is.null(argdefinition)) {
            if (argdefinition$class %in% c('numeric', 'integer')) {
                arg_value <- as.numeric(inp_value)
            } else if (argdefinition$class == 'character') {
                arg_value <- as.character(inp_value)
            } else {
                return(NULL)
            }
        } else {
            return(NULL)
        }
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

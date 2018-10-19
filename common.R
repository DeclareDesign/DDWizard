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
input_elem_for_design_arg <- function(argname, argdefault, argdefinition, width = '70%') {
    inp_id <- paste0('design_arg_', argname)
    
    argclass <- class(argdefault)
    argtype <- typeof(argdefault)
    
    inp_elem_constructor <- NULL
    inp_elem_args <- list(
        inp_id,
        argname,
        width = width
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

create_design_parameter_ui <- function(type, react, design_instance_fn, input = NULL, defaults = NULL) {
    boxes <- list()
    
    if (is.null(react$design)) {
        boxes <- list_append(boxes, p('Load a design first'))
    } else {
        if (type == 'design') {
            boxes <- list_append(boxes, HTML(attr(react$design, 'description')))
            boxes <- list_append(boxes, textInput('design_arg_design_name', 'Design name', value = react$design_id))
        }
        
        args <- formals(react$design)
        
        if (is.null(react$design_argdefinitions)) design_instance_fn()    # create instance with default args in order to get arg. definitions
        arg_defs <- react$design_argdefinitions
        
        for (argname in names(args)) {
            if (argname %in% args_control_skip_design_args) next()
            argdefault <- args[[argname]]
            argdefinition <- as.list(arg_defs[arg_defs$names == argname,])
            
            inp_elem_name_fixed <- paste0('design_arg_', argname, '_fixed')
            
            if (type != 'design' && isTruthy(input[[inp_elem_name_fixed]])) {
                next()
            }
            
            if (type == 'design') {
                inp_elem <- input_elem_for_design_arg(argname, argdefault, argdefinition, width = '70%')
                inp_elem_complete <- tags$div(tags$div(style = 'float:right;padding-top:23px',
                                                       checkboxInput(inp_elem_name_fixed, label = 'fixed', width = '30%')),
                                              inp_elem)
                
            } else {   # type == 'inspect'
                if (!is.null(defaults) && argname %in% names(defaults)) {
                    argvalue <- as.character(defaults[[argname]])
                } else {
                    if (typeof(argdefault) %in% c('language', 'symbol')) {
                        argvalue <- ''
                    } else {
                        argvalue <- as.character(argdefault)
                    }
                }
                
                inp_elem_complete <- textInput(paste0('design_arg_', argname), argname, value = argvalue)
            }
            
            boxes <- list_append(boxes, inp_elem_complete)
        }
    }
    
    return(do.call(material_card, c('Compare design parameters', boxes)))
}
# UI related utility functions.
#
# Markus Konrad <markus.konrad@wzb.eu>
# Sisi Huang <sisi.huang@wzb.eu>
#
# Dec. 2018
#

library(ggplot2)


# Create an input element for a design argument with name `argname`, with a default value `argdefault`.
# Use min/max and class definition from `argdefinition` (comes from designer "definitions" attrib.).
# Set input element width to `width` and input ID to `<idprefix>_arg_<argname>`.
# Returns NULL if argument class is not supported.
input_elem_for_design_arg <- function(design, argname, argdefault, argdefinition, nspace = function(x) { x }, width = '70%', idprefix = 'design') {
    args <- formals(design) # need to evaluate design defaults in cases when input is language
    
    inp_id <- nspace(paste0(idprefix, '_arg_', argname))
    
    argclass <- class(argdefault)
    argtype <- typeof(argdefault)
    
    inp_elem_constructor <- NULL
    inp_elem_args <- list(
        inp_id,
        argname,
        width = width
    )
    
    #evaluate arguments in separate environment
    #this allows us to evaluate 'language' class default arguments and convert resulting vectors to string inputs
    eval_envir <- new.env()
    
    args_eval <- lapply(1:length(args), function(a){
        evaluated_arg <- invisible(eval(args[[a]], envir = eval_envir))
        invisible(assign(x = names(args)[a], value = evaluated_arg, envir = eval_envir))
        hold <- invisible(get(names(args)[a], envir = eval_envir))
        if(length(hold) > 1) hold <- paste0(hold, collapse = ", ")
        return(hold)
    })
    
    names(args_eval) <- names(args)
    
    argmin <- ifelse(is.finite(argdefinition$min), argdefinition$min, NA)
    argmax <- ifelse(is.finite(argdefinition$max), argdefinition$max, NA)
    
    if (argclass %in% c('call', 'name') && argtype %in% c('language', 'symbol')) {  # "language" constructs (R formula/code)
        inp_elem_args$value = args_eval[[argname]]
    } else if (argclass == 'NULL' && argtype == 'NULL') {
        inp_elem_args$value = ''
    } else {
        inp_elem_args$value = argdefault
    }
    
    if ('class' %in% names(argdefinition)) {
        if (argdefinition$class == 'character') {
            inp_elem_constructor <- textInput
        } else if (argdefinition$class %in% c('numeric', 'integer') && class(args_eval[[argname]]) %in% c('numeric', 'integer')) {
            inp_elem_constructor <- numericInput
            inp_elem_args$min = argmin
            inp_elem_args$max = argmax
            
            # set step value for input element depending on class
            if (argdefinition$class == 'numeric') {
                inp_elem_args$step = 0.1
            } else if (argdefinition$class == 'integer') {
                inp_elem_args$step = 1
            }
        } else if (argdefinition$class %in% c('numeric', 'integer') && class(args_eval[[argname]]) == 'character') {
            inp_elem_constructor <- textInput
        }
    }
    
    # create the input element and return it
    if (!is.null(inp_elem_constructor)) {
        return(do.call(inp_elem_constructor, inp_elem_args))
    }
    
    # return NULL if argument class is not supported
    NULL
}


# Create a design argument value from a raw input value `inp_value`. The design argument has a
# default value `argdefault` and an argument definition `argdefinition` (which comes from designer
# "definitions" attrib.). `argclass` and `argtype` are usually `class(argdefault)` and
# `typeof(argtype)`.
design_arg_value_from_input <- function(inp_value, argdefault, argdefinition, argclass, argtype) {
    if (argclass %in% c('numeric', 'integer')) {
        arg_value <- as.numeric(inp_value)
    } else if (argclass %in% c('call', 'name') && argtype %in% c('language', 'symbol')) { # "language" constructs (R formula/code)
        if (length(inp_value) > 0 && !is.na(inp_value) && !is.null(argdefinition)) {
            # if there is a input value for an R formula field, convert it to the requested class
            if (argdefinition$class %in% c('numeric', 'integer')) {
                if (is.character(inp_value)) {
                    arg_value <- as.numeric(trimws(strsplit(inp_value, ",")[[1]]))
                } else {
                    arg_value <- as.numeric(inp_value)
                }
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
    # ifelse cannot return vector
    if(length(inp_value) > 0){
        return(arg_value)
    }else{
        return(argdefault) 
    }
   
}


# Create UI element with input boxes for design parameters. If `type` is "design", then this is for the
# "Design" tab, otherwise it's the design parameters for comparison UI for the "Inspect" tab.
# `react` is a list of reactive values (designer object and arg. definitions are needed).
# `nspace` is the namespace prefix function
# `design_instance_fn` is the function to generate a design (`design_instance` from tab_design.R).
# `input` is the shiny input values list, which is used to determine whether an argument has been set to
# "fixed" for the "inspect" design UI elements.
# `defaults` contains the default values for the input elements.
# `create_fixed_checkboxes`: if type is "design" create checkboxes for each input to allow fixing an argument
create_design_parameter_ui <- function(type, react, nspace, input = NULL, defaults = NULL, create_fixed_checkboxes = TRUE) {
    boxes <- list()
    
    args_design <- get_designer_args(react$design)
    arg_defs <- react$design_argdefinitions
    
    for (argname in names(args_design)) {
        if (argname %in% args_control_skip_design_args)
            next()
        argdefault <- args_design[[argname]]
        argdefinition <- as.list(arg_defs[arg_defs$names == argname,])
        inp_elem_complete <- NULL
        
        # check whether an element is set to fixed and skip it for "inspect" tab
        inp_elem_name_fixed <- paste0('design_arg_', argname, '_fixed')
        if (type != 'design' && isTruthy(input[[inp_elem_name_fixed]])) {
            next()
        }
        
        if (type == 'design') {
            # for the "design" tab, create two input elements for each argument:
            # 1. the argument value input box
            # 2. the "fixed" checkbox next to it
            if (create_fixed_checkboxes) {
                inp_elem_width <- '70%'
            } else {
                inp_elem_width <- '100%'
            }
            
            inp_elem <- input_elem_for_design_arg(react$design, argname, argdefault, argdefinition,
                                                  width = inp_elem_width, nspace = nspace, idprefix = type)

            if (!is.null(inp_elem)) {
                if (create_fixed_checkboxes) {
                    inp_elem_complete <- tags$div(tags$div(style = 'float:right;padding-top:23px',
                                                           checkboxInput(nspace(inp_elem_name_fixed),
                                                                         label = 'fixed', width = '30%')),
                                                  inp_elem)
                } else {
                    inp_elem_complete <- inp_elem
                }
            } else {
                warning(paste('input element could not be created for argument', argname))
            }
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
            
            # in "inspect" tab, the input is always a text input in order to support input of sequences
            inp_elem_complete <- textInput(nspace(paste0('inspect_arg_', argname)), argname, value = argvalue)
        }
        
        if (!is.null(inp_elem_complete)) {
            boxes <- list_append(boxes, inp_elem_complete)
        }
    }
    
    boxes
}


# Default plot theme for Declare Design.
# Copied from wizard_shiny code.
dd_theme <- function() {
    theme_bw() +
        theme(
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            axis.text=element_text(),#size=12),
            axis.title=element_text(face="bold"),#size=14,
            strip.text.x = element_text(face="bold"),
            panel.border = element_blank(),
            panel.grid.major = element_line(color = '#eeeeee'),
            strip.background = element_blank(),
            legend.position = "bottom",
            text = element_text(family = "Palatino", size=16)
        )
}

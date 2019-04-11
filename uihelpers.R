# UI related utility functions.
#
# Markus Konrad <markus.konrad@wzb.eu>
# Sisi Huang <sisi.huang@wzb.eu>
#
# Dec. 2018
#

library(ggplot2)


#Inspector help text
inpector_help_text <- '<dl>
                        <dt>Top panel</dt>
                        <dd>Specify the number of simulations in your diagnosis.</dd>
                        <dt>Left panel</dt>
                        <dd>Specify which design arguments to vary with each design 
                        diagnosis. You can input numeric values separated by a comma 
                        (e.g., 10, 20, 30) or provide ranges of values with one step 
                        to create an arithmetic sequence of values 
                        (e.g., 10, 20, ..., 100 generates a sequence from 10 to 100 
                        where the difference in terms is 10).</dd>
                        <dt>Right panel</dt>
                        <dd>Choose which arguments should be mapped to which visual
                        property of the diagnosis plot and which diagnosand should 
                        be displayed.</dd>
                        <br><dd>Click <b>"Run diagnoses"</b> to run the diagnoses and
                        display the updated plot.<br><br></dd>'

# Create an input element for a design argument with name `argname`, with a default value `argdefault`.
# Use min/max and class definition from `argdefinition` (comes from designer "definitions" attrib.).
# Set input element width to `width` and input ID to `<idprefix>_arg_<argname>`.
# Returns NULL if argument class is not supported.
input_elem_for_design_arg <- function(design, argname, argvalue, argvalue_parsed, argdefault, argdefinition, nspace = function(x) { x }, width = '70%', idprefix = 'design') {
    # extract the tips from library
    tips <- get_tips(design)
    inp_id <- nspace(paste0(idprefix, '_arg_', argname))
    
    argclass <- class(argdefault)
    argtype <- typeof(argdefault)
    
    inp_elem_constructor <- NULL
    inp_elem_args <- list(
        inp_id,
        argname,
        width = width
    )
    
    # get evaluated designer arguments (because they might be "language" constructs)
    # this allows us to evaluate 'language' class default arguments and convert resulting vectors to string inputs
    args_eval <- evaluate_designer_args(get_designer_args(design), attr(design, 'definitions'))
    
    argmin <- ifelse(is.finite(argdefinition$min), argdefinition$min, NA)
    argmax <- ifelse(is.finite(argdefinition$max), argdefinition$max, NA)
    
    if (argdefinition$vector && !is.null(argvalue_parsed) && any(is.na(argvalue_parsed))) {
        inp_elem_args$value <- argvalue
    } else {
        if (argclass %in% c('call', 'name') && argtype %in% c('language', 'symbol')) {  # "language" constructs (R formula/code)
            inp_elem_args$value <- args_eval[[argname]]
        } else if (argclass == 'NULL' && argtype == 'NULL') {
            inp_elem_args$value <- ''
        } else {
            inp_elem_args$value <- argdefault
        }
    }
    
    if ('class' %in% names(argdefinition)) {
        if (argdefinition$class == 'character' || argdefinition$vector) {
            inp_elem_constructor <- textInput
        } else if (argdefinition$class %in% c('numeric', 'integer') && class(args_eval[[argname]]) %in% c('numeric', 'integer', 'NULL')) {
            inp_elem_constructor <- numericInput
            inp_elem_args$min = argmin
            inp_elem_args$max = argmax
            
            # set step value for input element depending on class
            if (argdefinition$class == 'numeric') {
                inp_elem_args$step = 0.1
            } else if (argdefinition$class == 'integer') {
                inp_elem_args$step = 1
            }
        }
    }
    
    # escape the single quotes in the tips for Javascript
    if(str_detect(tips[[argname]], '\'')) tips[[argname]] <- gsub('\'', "\"", tips[[argname]])
    
    # create the input element and return it
    if (is.function(inp_elem_constructor)) {
        ret <- do.call(inp_elem_constructor, inp_elem_args)
        if (is.character(tips[[argname]])) {
            ret <- list(ret, dd_tipify(inp_id, argname, tips[[argname]]))
        }
    } else { 
        # return NULL if argument class is not supported
        ret <- NULL
    }
    ret
}


# Create a design argument value from a raw input value `inp_value`. The design argument has a
# default value `argdefault` and an argument definition `argdefinition` (which comes from designer
# "definitions" attrib.). `argclass` and `argtype` are usually `class(argdefault)` and
# `typeof(argtype)`.
design_arg_value_from_input <- function(inp_value, argdefault, argdefinition, argclass, argtype) {
    if (argclass == 'NULL') {
        argclass <- argdefinition$class
    }
    
    if (argtype == 'NULL') {
        argtype <- argdefinition$class
    }
    
    if (argclass %in% c('numeric', 'integer') && !argdefinition$vector) {
        arg_value <- as.numeric(inp_value)
    } else if ((argclass %in% c('call', 'name') && argtype %in% c('language', 'symbol') || argdefinition$vector) && argdefinition$class != 'character') { # "language" constructs (R formula/code)
        if (length(inp_value) > 0 && !is.na(inp_value) && !is.null(argdefinition)) {
            # if there is a input value for an R formula field, convert it to the requested class
            if (argdefinition$class %in% c('numeric', 'integer')) {
                if (is.character(inp_value)) {
                    arg_value <- trimws(strsplit(inp_value, ",")[[1]])
                    # convert the fractions to the decimals
                    arg_value <- unname(sapply(arg_value, function(x) eval(parse(text=x))))
                } else {
                    arg_value <- as.numeric(inp_value)
                }
            } else {
                return(NULL)
            }
        } else {
            return(NULL)
        }
    } else if ((argclass == 'character' || argdefinition$class == 'character') && is.character(inp_value)) {
        arg_value <- trimws(strsplit(inp_value, ",")[[1]])
    } else {
        return(NULL)
    }

    if (length(arg_value) > 0 && !(!argdefinition$vector && sum(is.na(arg_value)) == length(arg_value) && is.null(argdefault))) {
        return(arg_value)
    } else {
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
create_design_parameter_ui <- function(type, react, nspace, input, defaults, create_fixed_checkboxes = TRUE) {
    boxes <- list()
    # extract the tips from library
    tips <- get_tips(react$design)
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
        
        inp_id <- nspace(paste0('inspect_arg_', argname))
        
        if (type == 'design') {
            # for the "design" tab, create two input elements for each argument:
            # 1. the argument value input box
            # 2. the "fixed" checkbox next to it
            inp_elem_width <- ifelse(create_fixed_checkboxes, '70%', '100%')
            
            inp_elem <- input_elem_for_design_arg(react$design, argname, input[[paste0('design_arg_', argname)]],
                                                  defaults[[argname]], argdefault, argdefinition,
                                                  width = inp_elem_width, nspace = nspace, idprefix = type)
            

            if (!is.null(inp_elem)) {
                if (create_fixed_checkboxes) {
                    
                    inp_elem_complete <-
                        tags$div(tags$div(style = 'float:right;padding-top:23px',
                                          checkboxInput(nspace(inp_elem_name_fixed),
                                                        label = 'fixed',
                                                        width = '30%')), inp_elem)

                } else {
                    inp_elem_complete <- inp_elem
                }
            } else {
                warning(paste('input element could not be created for argument', argname))
            }
        } else {   # type == 'inspect'
            # omit character or logical arguments as they only determine label names or options that are useless for inspection
            if (!(argdefinition$class %in% c('character', 'logical'))) {
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
                inp_id <- nspace(paste0('inspect_arg_', argname))

                                # add input instruction to vector argument tips in inspector
                if (arg_defs$vector[arg_defs$names == argname]) tips[[argname]] <- 
                        paste0(tips[[argname]], ". Vary by enclosing values within parameters, separated by a space.")
                
                # escape the single quotes in the tips for Javascript
                if(str_detect(tips[[argname]], '\'')) tips[[argname]] <- gsub('\'', "\"", tips[[argname]])
                
                if (argdefinition$vector) {
                    inp_elem_complete <- list(
                        textAreaInput(inp_id, argname, value = argvalue, width = '100%', rows = 2, resize = 'vertical'),
                        dd_tipify(inp_id, argname, tips[[argname]])
                    )
                } else {
                    inp_elem_complete <- list(
                        textInput(inp_id, argname, value = argvalue, width = '100%'),
                        dd_tipify(inp_id, argname, tips[[argname]])
                    )
                    
                }
            }
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


# Wrapper for tipifying function
dd_tipify <- function(id, title, content){
    bsPopover(id = id, title = title, content = content, placement = "top", trigger = "hover")
}
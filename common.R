# Common utility functions.
#
# Markus Konrad <markus.konrad@wzb.eu>
# Oct. 2018
#

library(stringr)
library(future)
library(rlang)
library(digest)


# Append `v` to list `l` and return the resulting list. Appending is slow, don't use that often!
list_append <- function(l, v) {
    l[[length(l)+1]] <- v
    l
}


# Parse a sequence string `s` in the form of "n, n+s, ..., m", e.g. "1, 2, ..., 5" or "0, 0.25, ..., 1", etc.
# Convert the result to a vector of class `cls`.
# Finds the step `s` and generates the sequence using `seq()`.
# If a sequence without ellipsis that indicate range is passed, then the sequence is used as is. So
# a string of "1, 3, 8, 2" will just be split and converted depending on `cls`.
# Is fault tolerant to commas, so "1, 2 ... 5" is also accepted.
parse_sequence_string <- function(s, cls = 'numeric') {
    if (cls %in% c('numeric', 'integer') && grepl('...', s, fixed = TRUE)) {   # int/num sequences with range like 1, 2, ..., 10
        start_end <- str_split(s, "\\.\\.\\.")[[1]]
        start <- str_trim(str_split(start_end[1], ',')[[1]])
        startnums <- as.numeric(start[start != ''])

        end <- str_trim(str_replace_all(start_end[[2]], ',', ''))
        endnum <- as.numeric(end)
        
        if (length(startnums) > 1) {
            step <- startnums[2] - startnums[1]
        } else {
            step <- ifelse(startnums[1] <= endnum, 1, -1)
        }
        
        return(seq(from = startnums[1], to = endnum, by = step))
    } else {  # character list or int/num scalar or int/num sequence like 1, 3, 8, 2
        elems <- str_trim(str_split(s, ',')[[1]])
        if (cls %in% c('numeric', 'integer')) {
            return(as.numeric(elems))
        } else {
            return(elems)
        }
    }
}


# Create an input element for a design argument with name `argname`, with a default value `argdefault`.
# Use min/max and class definition from `argdefinition` (comes from designer "definitions" attrib.).
# Set input element width to `width` and input ID to `<idprefix>_arg_<argname>`.
# Returns NULL if argument class is not supported.
input_elem_for_design_arg <- function(argname, argdefault, argdefinition, width = '70%', idprefix = 'design') {
    inp_id <- paste0(idprefix, '_arg_', argname)
    
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
    
    if (argclass %in% c('call', 'name') && argtype %in% c('language', 'symbol')) {  # "language" constructs (R formula/code)
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
        
        # set step value for input element depending on class
        if (argdefinition$class == 'numeric') {
            inp_elem_args$step = 0.1
        } else if (argdefinition$class == 'integer') {
            inp_elem_args$step = 1
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


# Create UI element with input boxes for design parameters. If `type` is "design", then this is for the
# "Design" tab, otherwise it's the design parameters for comparison UI for the "Inspect" tab.
# `react` is a list of reactive values (designer object and arg. definitions are needed).
# `design_instance_fn` is the function to generate a design (`design_instance` from app.R).
# `input` is the shiny input values list, which is used to determine whether an argument has been set to
# "fixed" for the "inspect" design UI elements.
# `defaults` contains the default values for the input elements.
create_design_parameter_ui <- function(type, react, design_instance_fn, input = NULL, defaults = NULL) {
    boxes <- list()
    
    if (is.null(react$design)) {
        boxes <- list_append(boxes, p('Load a design first'))
    } else {
        if (type == 'design') {
            # design description and name come first in the "design" tab
            boxes <- list_append(boxes, HTML(attr(react$design, 'description')))
            boxes <- list_append(boxes, textInput('design_arg_design_name', 'Design name', value = react$design_id))
        }
        
        args <- formals(react$design)
        
        if (is.null(react$design_argdefinitions)) design_instance_fn()    # create instance with default args in order
                                                                          # to get arg. definitions
        arg_defs <- react$design_argdefinitions
        
        for (argname in names(args)) {
            if (argname %in% args_control_skip_design_args) next()
            argdefault <- args[[argname]]
            argdefinition <- as.list(arg_defs[arg_defs$names == argname,])
            
            # check whether an element is set to fixed and skip it for "inspect" tab
            inp_elem_name_fixed <- paste0('design_arg_', argname, '_fixed')
            if (type != 'design' && isTruthy(input[[inp_elem_name_fixed]])) {
                next()
            }
            
            if (type == 'design') {
                # for the "design" tab, create two input elements for each argument:
                # 1. the argument value input box
                # 2. the "fixed" checkbox next to it
                inp_elem <- input_elem_for_design_arg(argname, argdefault, argdefinition, width = '70%', idprefix = type)
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
                
                # in "inspect" tab, the input is always a text input in order to support input of sequences
                inp_elem_complete <- textInput(paste0('inspect_arg_', argname), argname, value = argvalue)
            }
            
            boxes <- list_append(boxes, inp_elem_complete)
        }
    }
    
    return(do.call(material_card, c('Compare design parameters', boxes)))
}


# For a given designer `design`, its argument definitions `d_argdefs` and the shiny input values object `input`,
# parse the sequence string for each designer argument and generate a list of arguments used for inspection.
# These argument values will define the paremeter space for inspection.
get_args_for_inspection <- function(design, d_argdefs, input) {
    d_args <- formals(design)
    
    insp_args <- list()
    
    for (d_argname in names(d_args)) {
        d_argdef <- as.list(d_argdefs[d_argdefs$names == d_argname,])
        inp_name <- paste0('inspect_arg_', d_argname)
        
        inp_value <- input[[inp_name]]
        d_argclass <- d_argdef$class
        
        # if a value was entered, try to parse it as sequence string and add the result to the list of arguments to compare
        inp_elem_name_fixed <- paste0('design_arg_', d_argname, '_fixed')
        if (isTruthy(inp_value) && !isTruthy(input[[inp_elem_name_fixed]])) {
            insp_args[[d_argname]] <- parse_sequence_string(inp_value, d_argclass)
        }
    }
    
    insp_args
}


# Run diagnoses on designer `designer` and parameter space `args`. Run `sims` simulations and `bootstrap_sims` bootstraps.
# If `use_cache` is TRUE, check if diagnosis results already exist for this designer / parameter combinations and return
# the cached results or create new diagnosis results.
# The simulations are run in parallel if packages `future` and `future.apply` are installed.
run_diagnoses <- function(designer, args, sims, bootstrap_sims, use_cache = TRUE) {
    if (use_cache) {
        # cache fingerprint generated from designer object, simulation config. and parameter space
        fingerprint_args <- args
        fingerprint_args$sims <- sims
        fingerprint_args$bootstrap_sims <- bootstrap_sims
        fingerprint_args$design <- designer
        fingerprint <- digest(fingerprint_args)   # uses MD5
        
        cache_file <- paste0('.cache/diag_', fingerprint, '.RDS')
        
        if (file.exists(cache_file)) {   # read and return result object from cache
            return(readRDS(cache_file))
        }
    } else {
        print('caching disabled')
        cache_file <- NULL
    }
    
    # set up to run in parallel
    plan('multicore', workers = n_diagnosis_workers)
    
    # generate designs from designer with arguments `args`
    all_designs <- eval_bare(expr(expand_design(designer = designer, expand = TRUE, !!!args)))
    
    # run diagnoses
    diag_res <- diagnose_design(all_designs, sims = sims, bootstrap_sims = bootstrap_sims)
    
    # save to cache if requested
    if (!is.null(diag_res) && !is.null(cache_file)) {
        saveRDS(diag_res, cache_file)
    }
    
    diag_res
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

# Inspect tab related utility functions.
#
# March 2019
#

# get defaults for inputs in inspect tab: use value from design args `d_args` in design tab unless a sequence of values
# for arg comparison was defined in inspect tab (`input` is the input object for the inspect tab).
# `defs` is the argument definitions table for the current designer
get_inspect_input_defaults <- function(d_args, defs, input) {
    first_arg <- names(d_args)[1]
    if (first_arg == 'N' && is.null(d_args['N'])) first_arg <- names(d_args)[2]
    
    sapply(names(d_args), function(argname) {
        arg_inspect_input <- input[[paste0('inspect_arg_', argname)]]
        argdef <- as.list(defs[defs$names == argname,])
        
        parsed_arg_inspect_input <- tryCatch(parse_sequence_string(arg_inspect_input),
                                             warning = function(cond) { NA },
                                             error = function(cond) { NA })
        
        #if (is.null(arg_inspect_input) || (!any(is.na(parsed_arg_inspect_input)) && length(parsed_arg_inspect_input) < 2)) {
        if (is.null(arg_inspect_input)) {
            if (argname == first_arg) {
                # set a default value for "N" the first time
                # but there are some design without N argument
                if (first_arg == 'N') {
                    n_int <- as.integer(d_args[[first_arg]])
                    return(sprintf('%d, %d ... %d', n_int, n_int + 10, n_int + 100))
                } else {
                    min_int <- argdef$inspector_min
                    step_int <- argdef$inspector_step
                    max_int <- min_int + 4*step_int
                    return(sprintf('%d, %d ... %d', min_int, min_int + step_int, max_int))
                }
            } else {
                if(any(vapply(d_args[[argname]], nchar, FUN.VALUE=numeric(1)) > 10) && argdef[["class"]] == "numeric"){
                    d_args[[argname]] <- fractions(d_args[[argname]])
                }
                arg_char <- as.character(d_args[[argname]])
                if (argdef$vector) {  # vector of vectors input
                    return(sprintf('(%s)', paste(arg_char, collapse = ', ')))
                } else {
                    return(arg_char)
                }
            }
        } else {
            return(arg_inspect_input)
        }
    }, simplify = FALSE)
}


# For a given designer `design`, its argument definitions `d_argdefs`, the inspector tab input values object `inspect_input`,
# a character vector of fixed design arguments `fixed_args`, and the design tab input values object `design_input`,
# parse the sequence string for each designer argument and generate a list of arguments used for inspection.
# These argument values will define the parameter space for inspection.
get_args_for_inspection <- function(design, d_argdefs, inspect_input, fixed_args, design_input) {
    d_args <- get_designer_args(design)
    
    insp_args <- list()
    
    for (d_argname in names(d_args)) {
        inp_name_design <- paste0('design_arg_', d_argname)
        inp_name_inspect <- paste0('inspect_arg_', d_argname)
       
        # for a fixed argument or if no input is given in the inspect tab (character arguments),
        # use the design tab input value
        if (d_argname %in% fixed_args) {
            inp_value <- design_input[[inp_name_design]]
        } else {                           # else use the value from the inspect tab
            inp_value <- inspect_input[[inp_name_inspect]]
        }
        
        d_argdef <- as.list(d_argdefs[d_argdefs$names == d_argname,])
        d_argclass <- d_argdef$class
        # if a value was entered, try to parse it as sequence string and add the result to the list of arguments to compare
        inp_elem_name_fixed <- paste0('design_arg_', d_argname, '_fixed')
        if (isTruthy(inp_value) && !isTruthy(inspect_input[[inp_elem_name_fixed]])) {
            insp_args[[d_argname]] <- tryCatch({
                if (d_argdef$vector) {
                    # convert fractions to decimals
                    inp_value <-  unname(sapply(trimws(strsplit(gsub("[()]","", inp_value), ",")[[1]]), function(x) eval(parse(text=x))))
                    inp_value <- sprintf('(%s)', paste(inp_value, collapse = ', '))
                    parse_sequence_of_sequences_string(inp_value, d_argclass)
                } else {
                    parse_sequence_string(inp_value, d_argclass)
                }
            }, warning = function(cond) {
                NA
            }, error = function(cond) {
                NA
            })
        }
    }
    
    insp_args
}


# get diagnosands call closure and vector of available diagnosands for `designer`
# returns a list with:
# $diagnosands_call -- a closure that generates a diagnosands function depending on parameter "alpha"
# $available_diagnosands -- a character vector of available diagnosand labels
get_diagnosands_info <- function(designer) {
    diag_call <- attr(designer, 'diagnosands')
    
    res <- list()
    
    if (is.null(diag_call)) {
        res$diagnosands_call <- function(diag_param_alpha) {  # here we can pass alpha
            function(data) {
                DeclareDesign:::default_diagnosands(data, alpha = diag_param_alpha)
            }
        }
        
        res$available_diagnosands <- DeclareDesign:::default_diagnosands(NULL)$diagnosand_label
    } else {
        res$diagnosands_call <- function(diag_param_alpha) {  # here we ignore alpha
            attr(diag_call, 'call')
        }
        
        quick_diagnosis <- suppressWarnings(diagnose_design(designer, sims = 2, bootstrap_sims = 0)$diagnosands_df)
        res$available_diagnosands <- setdiff(names(quick_diagnosis), c("design_label", "estimand_label", "estimator_label",
                                                                       "term", "n_sims"))
    }
    
    res
}

# clean and capitalize string
str_cap <- function(str, hard_code = c("rmse" = "RMSE",
                                       "type_s_rate" = "Type S rate",
                                       "mean_se" = "Mean SE",
                                       "sd_estimate" = "SD estimate")){ #can hardcode specific capitalizations
    if(str %in% names(hard_code)) 
        hard_code[[str]]
    else {
        str_ret <- rm_usc(str)
        paste0(toupper(substr(str_ret, 1, 1)), 
               substr(str_ret, 2, nchar(str_ret)))
    }
        
}

# round function of diagnosands data table
round_df <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    df[,nums] <- round(df[,nums], digits = digits)
    return(df)
}

# generate plot code
generate_plot_code <- function(plotdf, design_name, diag_param, x_param, color_param, facets_param, plot_ci) {
    code <- readLines('inspect_plot_template.txt')
    plot_color <- isTruthy(color_param) && color_param != '(none)'
    plot_facets <- isTruthy(facets_param) && facets_param != '(none)'
    
    if (plot_color) {
        plotdf[[color_param]] <- factor(plotdf[[color_param]])
    }
    
    if (plot_facets) {
        plotdf[[facets_param]] <- factor(plotdf[[facets_param]])
    }
    
    aes_args <- list(
        'x' = x_param,   
        'y' = diag_param,
        'ymin' = paste0(diag_param, '_min'),
        'ymax' = paste0(diag_param, '_max')
    )
    
    # if the "color" parameter is set, add it to the aeshetics definition
    if (plot_color) {
        aes_args$color <- color_param
        aes_args$fill <- color_param
        aes_args$group <- color_param
    } else {
        aes_args$group <- 1
    }
    
    vars <- list()
    vars$CREATION_DATE <- Sys.Date()
    vars$CREATE_DATA <- paste(capture.output({
        datapasta::df_paste(plotdf,     # nicely format data frame. ugly alternative: dput
                            output_context = datapasta::console_context())
    }), collapse = '\n')
    vars$DIAG_PARAM <- diag_param
    vars$X_PARAM <- x_param
    vars$DESIGN_NAME <- design_name
    vars$PLOT_AES <- paste(paste(names(aes_args), '=', as.character(aes_args)), collapse = ', ')
    
    if (plot_ci) {
        vars$PLOT_RIBBON <- "\n    geom_ribbon(alpha = 0.25, color = 'white') +"
    } else {
        vars$PLOT_RIBBON <- ''
    }
    
    if (plot_facets) {
        vars$PLOT_FACETS <- sprintf("+\n    facet_wrap(~%s, ncol = 2, labeller = label_both)", facets_param)
    } else {
        vars$PLOT_FACETS <- ''
    }

    for (varname in names(vars)) {
        code <- gsub(paste0('%', varname, '%'), vars[[varname]], code, fixed = TRUE)
    }
    
    code
}
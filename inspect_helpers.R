# Inspect tab related utility functions.
#
# Clara Bicalho <clara.bicalho@wzb.eu>
# Sisi Huang <sisi.huang@wzb.eu>
# Markus Konrad <markus.konrad@wzb.eu>
#
# March 2019
#

# get defaults for inputs in inspect tab: use value from design args `d_args` in design tab unless the argument
# is in the set of previously changed arguments `insp_changed_args`, a sequence of values for arg comparison was
# defined in inspect tab (`input` is the input object for the inspect tab) or the input is invalid and needs to
# be fixed by the user.
# `defs` is the argument definitions table for the current designer
get_inspect_input_defaults <- function(d_args, defs, input, insp_changed_args) {
    first_arg <- names(d_args)[1]
    if (first_arg == 'N' && is.null(d_args['N'])) first_arg <- names(d_args)[2]
    
    sapply(names(d_args), function(argname) {
        arg_inspect_input <- input[[paste0('inspect_arg_', argname)]]
        arg_design_val <- d_args[[argname]]
        argdef <- as.list(defs[defs$names == argname,])
        
        parsed_arg_inspect_input <- tryCatch(parse_sequence_string(arg_inspect_input),
                                             warning = function(cond) { NA },
                                             error = function(cond) { NA })
        
        if (is.null(arg_inspect_input)) {   # initial state: no inputs in the "inspect" tab on the left side
            # set a default value for "N" the first time
            # but there are some design without N argument
            if (argname == first_arg) {
                if (first_arg == 'N') {
                    n_int <- as.integer(d_args[[first_arg]])
                    return(sprintf('%d, %d ... %d', n_int, n_int + 10, n_int + 100))
                } else {
                    min_int <- argdef$inspector_min
                    step_int <- argdef$inspector_step
                    max_int <- min_int + 4*step_int
                    return(sprintf('%d, %d ... %d', min_int, min_int + step_int, max_int))
                }
            } else {  # set a default for all other arguments
                return(designer_arg_value_to_fraction(arg_design_val, argdef$class, argdef$vector, to_char = TRUE))
            }
        } else {   # "inspect" tab on the left side has inputs
            # if it is in the set of previously changed arguments or if it is varying (user has entered a sequence),
            # or if the user has entered an invalid value, return this value as set by the user
            
            seq_input <- tryCatch(parse_sequence_string(arg_inspect_input),
                                  warning = function(cond) { NA },
                                  error = function(cond) { NA })

            seqofseq_input <- tryCatch(parse_sequence_of_sequences_string(arg_inspect_input),
                                       warning = function(cond) { NA },
                                       error = function(cond) { NA })
            
            if (argname %in% insp_changed_args || (argdef$vector && !is.null(seqofseq_input) && length(seqofseq_input) > 1)
                || (!argdef$vector && any(is.na(seq_input))))
            {
                return(arg_inspect_input)
            } else {  # else return the argument value from the design tab. this overwrites values set by the user in this tab
                return(designer_arg_value_to_fraction(arg_design_val, argdef$class, argdef$vector, to_char = TRUE))
            }
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
                # the format of inp_value depends on whether it is as fixed_arg, if fixed, then without brackets around the value 
                if (d_argname %in% fixed_args) { 
                    if (d_argdef$vector) {
                        trimws(strsplit(inp_value, ",")[[1]])
                    } else {
                        parse_sequence_string(inp_value, d_argclass)
                    }
                } else if (d_argdef$vector) {
                    # split the possible "vector or vectors" into a list of character vectors
                    split_strings <- parse_sequence_of_sequences_string(inp_value, cls = 'character')
                    
                    if (d_argclass != 'character') {  # convert strings to numbers
                        # eval() is evil, so make sure to include only characters that can make up integer, real or rational number:
                        # all digits, dots, slashes and minus
                        split_strings <- lapply(split_strings, function(s) {
                            gsub('[^\\d\\.\\/\\-]', '', s, perl = TRUE)
                        })
                        
                        lapply(split_strings, function(s) {  # outer lapply: list of vectors like ("1.3", "1/5", "-2") -> s
                            unname(sapply(s, function(x) {   # inner sapply: parse each element in s to produce a numeric (this also handles fractions, otherwise we could use "as.numeric")
                                eval(parse(text=x))
                            }))
                        })
                    } else {  # return the strings as they are
                        split_strings
                    }
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
        res$available_diagnosands <- grep("se(", res$available_diagnosands, fixed = TRUE, invert = TRUE, value = TRUE)
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

# Function to enclose string within parameters
inpar <- function(vector){
    sapply(vector, function(v) 
        ifelse(is.na(v) || v == "", "", paste0("[", v , "]")))
}

# Function that weaves two matrices (first row of first matrix)
weave <- function(mat1, mat2, inpar_mat2 = TRUE, rnames = NULL, excl_0 = TRUE, within_col = TRUE){
    if(!identical(dim(mat1), dim(mat2))) stop("Input matrices should be the same length")
    if(is.vector(mat1)){
        if(within_col){
            mat1 <- matrix(mat1, ncol = 1)
            mat2 <- matrix(mat2, ncol = 1)  
        }else{
            mat1 <- matrix(mat1, nrow = 1)
            mat2 <- matrix(mat2, nrow = 1)
        }
    }
    if(!is.matrix(mat1)) mat1 <- as.matrix(mat1)
    if(!is.matrix(mat2)) mat2 <- as.matrix(mat2)
    
    matout <- matrix(NA, nrow(mat1)*2, ncol(mat1))
    for(i in 1:nrow(mat1)){
        matout[(2*i-1),] <- mat1[i,]
        if(inpar_mat2) 
            matout[(2*i),] <- inpar(mat2[i,])
        else 
            matout[(2*i),] <- mat2[i,]
    }
    
    if(!is.null(rnames)){
        rnames <- rep(rnames, each = 2)
        rmn <- 1:length(rnames)%%2 == 0
        rnames[rmn] <- ""
    }
    
    if(!is.null(colnames(mat1))) colnames(matout) <- colnames(mat1)
    matout <- cbind(rnames, matout) 
    matout <- gsub("NaN", "", matout, fixed = TRUE)
    if(excl_0) matout <- gsub("(0)", "", matout, fixed = TRUE)
    
    return(matout)
}

# Make diagnositic table long (used in download long option of diagnosis tab)

make_diagnosis_long <- function(tab, diagnosand_labels, within_col = FALSE){
    mains <- diagnosand_labels
    ses  <- paste0('se(', mains, ')')
    
    tab_args <- tab[, !names(tab) %in% c(mains, ses)]
    tab_means <- tab[,names(tab) %in% mains]
    tab_ses   <- tab[,names(tab) %in% ses]
    
    to_export <- cbind(weave(tab_args, matrix("", nrow(tab_args), ncol(tab_args))),
                       weave(round(tab_means, 3), round(tab_ses, 3), within_col = within_col))
    colnames(to_export) <- c(names(tab_args), mains)
    return(to_export)
}

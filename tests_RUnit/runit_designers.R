# Test suite for all designers from DesignLibrary in order to check if they
# meet the requirements to work with DDWizard.
# This also tests some functions from `inspect_helpers.R`

library(RUnit)
library(DesignLibrary)


source('../common.R')
source('../inspect_helpers.R')


get_available_designers <- function() {
    all_designers <- grep("designer$", ls(as.environment("package:DesignLibrary")), value = TRUE)
    all_designers[!grepl('^simple_', all_designers)]
}

get_designer <- function(designer_name) {
    getFromNamespace(designer_name, 'DesignLibrary')
}


test.designers_have_valid_definitions <- function() {
    designer_names <- get_available_designers()
    for (d_name in designer_names) {
        d <- get_designer(d_name)
        checkTrue('definitions' %in% names(attributes(d)),
                  paste(d_name, 'definitions exist'))
        
        defs <- attr(d, 'definitions')
        args <- formals(d)
        argnames <- names(args)
        
        checkEquals(lapply(defs, class), list(names = 'character',
                                              tips = 'character',
                                              class = 'character',
                                              vector = 'logical',
                                              min = 'numeric',
                                              max = 'numeric',
                                              inspector_min = 'numeric',
                                              inspector_step = 'numeric'),
                    paste(d_name, 'definitions data frame has expected columns and data types'))
        
        checkEquals(defs$names, argnames,
                    paste(d_name, 'definitions arg. names match function arg. names'))
        checkTrue(sum(is.na(defs$tips)) == 0 && sum(nchar(defs$tips) == 0) == 0,
                  paste(d_name, 'definitions tips are non-NA and filled out'))
        checkTrue(all(defs$class %in% c('integer', 'numeric', 'character', 'logical')),
                  paste(d_name, 'definitions class are correct'))
        checkTrue(sum(is.na(defs[defs$class %in% c('integer', 'numeric'), c('min', 'max', 'inspector_min', 'inspector_step')])) == 0,
                  paste(d_name, 'min, max, inspector_min and inspector_step are all non-NA for numeric/integer args in definitions'))
    }
}


test.designers_instantiation_with_defaults <- function() {
    designer_names <- get_available_designers()
    for (d_name in designer_names) {
        d <- get_designer(d_name)
        d_instance <- d()
        
        checkEquals(class(d_instance), c('design', 'dd'),
                    paste(d_name, 'design can be created and has correct class'))
        
        d_output <- capture.output({
            d_instance
        })
        
        checkTrue(class(d_output) == 'character' && sum(nchar(d_output)) > 0,
                  paste(d_name, 'design generates output'))
        checkEquals(paste0(d_output[1:3], collapse = ''), 'Design Summary',
                    paste(d_name, 'design generates Design Summary'))
        checkTrue('code' %in% names(attributes(d_instance)),
                  paste(d_name, 'design has code attribute'))
        
        d_code <- attr(d_instance, 'code')
        checkTrue(class(d_code) == 'character' && sum(nchar(d_code)) > 0,
                  paste(d_name, 'design generates code'))
    }
}


test.designers_inspection_with_defaults <- function() {
    omit_designers <- c('multi_arm_designer', 'two_by_two_designer')  # both still contain errors
    
    designer_names <- get_available_designers()
    for (d_name in designer_names) {
        if (d_name %in% omit_designers) {
            warning(paste('omitting designer', d_name))
            next()
        }
        
        d <- get_designer(d_name)
        d_args <- get_designer_args(d)
        d_args <- d_args[setdiff(names(d_args), 'args_to_fix')]  # remove args_to_fix
        defs <- attr(d, 'definitions')
        
        # remove all but numerical arguments (because only for those we provide inputs anyway in the inspector tab)
        d_args <- d_args[defs[defs$class %in% c('numeric', 'integer'), 'names']]
        
        d_args_eval <- evaluate_designer_args(d_args)
        checkEquals(names(d_args), names(d_args_eval),
                    paste(d_name, 'design argument defaults can be evaluated'))
        
        d_args_eval <- d_args_eval[!sapply(d_args, is.null)]
        
        insp_input <- get_inspect_input_defaults(d_args_eval, defs, list())
        names(insp_input) <- paste0('inspect_arg_', names(insp_input))
        design_input <- d_args_eval
        names(design_input) <- paste0('design_arg_', names(design_input))
        
        insp_args <- get_args_for_inspection(d, defs, insp_input, character(), design_input)
        checkEquals(names(insp_args), names(d_args_eval),
                    paste(d_name, 'got complete design arguments for inspections'))
        
        vec_args <- defs[defs$names %in% names(insp_args) & defs$vector, 'names']
        checkTrue(all(sapply(insp_args[vec_args], class) == 'list'),
                  paste(d_name, 'all vector arguments are of class "list"'))
        
        varying_arg <- names(insp_args)[sapply(insp_args, length) > 1]
        checkTrue(length(varying_arg) == 1,
                  paste(d_name, 'inspection arguments contain exactly one varying argument'))
        
        diag_info <- get_diagnosands_info(d)
        
        suppressWarnings({  # suppress low number of sims warnings
            diag_res <- run_diagnoses(d, insp_args, 2, 2, diagnosands_call = diag_info$diagnosands_call(0.05),
                                      use_cache = FALSE, advance_progressbar = 0, n_diagnosis_workers = 1)
        })
        
        checkTrue(diag_res$from_cache == FALSE,
                  paste(d_name, 'diagnoses results are not from cache'))
        checkTrue(class(diag_res$results) == 'diagnosis',
                  paste(d_name, 'diagnoses results are of correct class'))

        diag_df <- diag_res$results$diagnosands_df
        varying_arg_res <- diag_df[, varying_arg]
        if (class(varying_arg_res) == 'factor') {
            varying_arg_res <- sort(as.numeric(levels(varying_arg_res)))
        }
        checkEquals(insp_args[[varying_arg]], unique(varying_arg_res),
                    paste(d_name, 'diagnoses results have results for varying arg'))
        
        checkTrue(all(diag_info$available_diagnosands %in% names(diag_df)),
                  paste(d_name, 'diagnoses results contain all diagnosands'))
    }
}
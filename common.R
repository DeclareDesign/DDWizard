# Common utility functions.
#
# Markus Konrad <markus.konrad@wzb.eu>
# Sisi Huang <sisi.huang@wzb.eu>
#
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

# Merge list `l1` with `l2` and return combined list. If the same key exists in both lists, the
# value in `l2` will overwrite the value in `l1`.
list_merge <- function(l1, l2) {
    l_out <- list()
    keys1 <- names(l1)
    keys2 <- names(l2)
    
    for (k in keys1) { l_out[[k]] <- l1[[k]] }
    for (k in keys2) { l_out[[k]] <- l2[[k]] }
    
    stopifnot(setequal(union(keys1, keys2), names(l_out)))
    
    l_out
}

# Round numeric values in a data frame to `digits`.
# Copied from "wizard_shiny" repository.
round_df <- function(df, digits){
    i <- vapply(df, is.numeric, TRUE)
    df[i] <- lapply(df[i], round, digits)
    df
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


# Turn a string `s` into a valid R object name.
make_valid_r_object_name <- function(s) {
    # Identifiers consist of a sequence of letters, digits, the period (‘.’) and the underscore.
    # They must not start with a digit or an underscore, or with a period followed by a digit.
    # — R Language Definition
    s <- str_replace_all(s, '[^A-Za-z0-9\\._]', '_')
    s <- str_replace(s, '^[\\d_]+', '')
    str_replace(s, '^\\.\\d+', '')
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


# get cache file name unique to cache type `cachetype` (simulation or diagnosis results),
# parameter space `args`, number of (bootstrap) simulations `sims`, designer name `designer`
get_diagnosis_cache_filename <- function(cachetype, args, sims, designer) {
    fingerprint_args <- args
    fingerprint_args$sims <- sims
    fingerprint_args$design <- designer
    fingerprint_args$cache_version <- 1       # increment whenever the simulated data in cache is not compatible anymore (i.e. DD upgrade)
    fingerprint <- digest(fingerprint_args)   # uses MD5
    
    sprintf('.cache/%s_%s.RDS', cachetype, fingerprint)
}


# Run diagnoses on designer `designer` and parameter space `args`. Run `sims` simulations and `bootstrap_sims` bootstraps.
# If `use_cache` is TRUE, check if simulated data already exists for this designer / parameter combinations and use cached
# data or create newly simulated data for running diagnoses.
# The simulations are run in parallel if packages `future` and `future.apply` are installed.
run_diagnoses <- function(designer, args, sims, bootstrap_sims, diag_param_alpha = 0.05, use_cache = TRUE, advance_progressbar = 0) {
    # generate designs from designer with arguments `args`
    all_designs <- eval_bare(expr(expand_design(designer = designer, expand = TRUE, !!!args)))
    if (advance_progressbar) incProgress(advance_progressbar)
    
    # get simulated data either from cache or generate it
    
    simdata <- NULL
    if (use_cache) {
        # cache fingerprint generated from designer object, simulation config. and parameter space
        cache_file <- get_diagnosis_cache_filename('simdata', args, sims, designer)
        
        if (file.exists(cache_file)) {   # read and return result object from cache
            if (advance_progressbar) incProgress(advance_progressbar)
            simdata <- readRDS(cache_file)
            print('loaded simulation data from cache')
        }
    } else {
        cache_file <- NULL
    }
    
    # set up to run in parallel
    plan('multicore', workers = n_diagnosis_workers)
    
    if (is.null(simdata)) {  # generate simulations
        # simulate data
        simdata <- simulate_designs(all_designs, sims = sims)
        if (advance_progressbar) incProgress(advance_progressbar)
        
        # save simulations to cache if requested
        if (!is.null(simdata) && !is.null(cache_file)) {
            saveRDS(simdata, cache_file)
        }
    }
    
    diag_res <- NULL
    if (use_cache) {
        stopifnot(!is.null(cache_file))
        
        # make the cache fingerprint dependent on simulated data's fingerprint and on parameters for diagnosands
        args <- list(
            'diag_param_alpha' = diag_param_alpha,
            'from_simdata' = cache_file
        )
        
        diag_cache_file <- get_diagnosis_cache_filename('diagresult', args, bootstrap_sims, designer)
        
        if (file.exists(diag_cache_file)) {   # read and return result object from cache
            if (advance_progressbar) incProgress(advance_progressbar)
            diag_res <- readRDS(diag_cache_file)
            print('loaded diagnosis results from cache')
        }
    } else {
        diag_cache_file <- NULL
    }
    
    if (is.null(diag_res)) {  # run diagnoses using the simulated data
        def_diag_fns <- function(data) { DeclareDesign:::default_diagnosands(data, alpha = diag_param_alpha) }  # pass alpha parameter for power calc.
        diag_res <- diagnose_designs(simdata, diagnosands = def_diag_fns, bootstrap_sims = bootstrap_sims)
        if (advance_progressbar) incProgress(advance_progressbar)
        
        # save diagnosis results to cache if requested
        if (!is.null(diag_res) && !is.null(diag_cache_file)) {
            saveRDS(diag_res, diag_cache_file)
        }
    }

    diag_res
}

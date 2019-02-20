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

# Parse a string `s` that denotes a sequence of sequences of class `cls` such as:
#
# '(1, 2, 3), (4, 5, 6), (7,8, 9)'
# or
# '(1, 2, 3),
# (4, 5, 6),
# (7,8, 9)'
# or
# (1, 2) (3) (4, 5 ,6)
#
# Note the inner sequences must be denoted as "(a, b, ... z)", i.e. there must be a comma to
# split the values. However, to split the sequences themselves no comma is needed.
# 
# If `require_rectangular` is TRUE, all sequences in `s` must be of the same length, i.e. they
# must form a regular, rectangular matrix (like the first two examples). The output will then
# be a matrix of size NxM, where N is the number of sequences and M is the number of items in
# each sequence. If `s` does not form a regular matrix, NULL will be returned.
# If `require_rectangular` is TRUE, the sequences in `s` can be of any length > 0. The output will
# then be a list of length N, where N is the number of sequences. Each list item is then a numeric
# vector of variable length.
#
# If the input cannot be parsed, NULL will be returned.
parse_sequence_of_sequences_string <- function(s, cls = 'numeric', require_rectangular = FALSE) {
    m <- gregexpr('\\(([^\\(\\)]*)\\)', s)
    vecs <- regmatches(s, m)
    
    if (length(vecs[[1]]) == 0) {
        return(NULL)
    }
    
    parsed <- sapply(vecs[[1]], function(v) {
        v <- gsub('[\\(\\)]', '', v)
        if (cls == 'character') {
            v <- gsub('["\']', '', v)
        }
        parse_sequence_string(v, cls = cls)
    }, USE.NAMES = FALSE, simplify = require_rectangular)
    
    if (require_rectangular) {
        if (class(parsed) != 'list') {
            return(t(parsed))
        } else {
            return(NULL)
        }
    } else {
        return(parsed)
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


# Return a list of valid designer parameters
get_designer_args <- function(designer) {
    formals(designer)
}


# For a given designer `design`, its argument definitions `d_argdefs`, the inspector tab input values object `inspect_input`,
# a character vector of fixed design arguments `fixed_args`, and the design tab input values object `design_input`,
# parse the sequence string for each designer argument and generate a list of arguments used for inspection.
# These argument values will define the paremeter space for inspection.
get_args_for_inspection <- function(design, d_argdefs, inspect_input, fixed_args, design_input, vecinput_args) {
    d_args <- get_designer_args(design)
    
    insp_args <- list()
    
    for (d_argname in names(d_args)) {
        if (d_argname %in% fixed_args) {   # for a fixed argument, use the design tab input value
            inp_name <- paste0('design_arg_', d_argname)
            inp_value <- design_input[[inp_name]]
        } else {                           # else use the value from the inspect tab
            inp_name <- paste0('inspect_arg_', d_argname)
            inp_value <- inspect_input[[inp_name]]
        }
        
        d_argdef <- as.list(d_argdefs[d_argdefs$names == d_argname,])
        d_argclass <- d_argdef$class
        
        # if a value was entered, try to parse it as sequence string and add the result to the list of arguments to compare
        inp_elem_name_fixed <- paste0('design_arg_', d_argname, '_fixed')
        if (isTruthy(inp_value) && !isTruthy(inspect_input[[inp_elem_name_fixed]])) {
            if (d_argname %in% vecinput_args) {
                insp_args[[d_argname]] <- parse_sequence_of_sequences_string(inp_value, d_argclass)
            } else {
                insp_args[[d_argname]] <- parse_sequence_string(inp_value, d_argclass)
            }
        }
    }
    
    insp_args
}


# get cache file name unique to cache type `cachetype` (designs, simulation or diagnosis results),
# parameter space `args`, number of (bootstrap) simulations `sims`, designer name `designer`
get_diagnosis_cache_filename <- function(cachetype, args, sims, bs_sims, designer) {
    fingerprint_args <- args
    fingerprint_args$sims <- sims
    fingerprint_args$bs_sims <- bs_sims
    fingerprint_args$design <- designer
    fingerprint_args$cache_version <- 1       # increment whenever the simulated data in cache is not compatible anymore (i.e. DD upgrade)
    fingerprint <- digest(fingerprint_args)   # uses MD5
    
    # print('CACHE FINGERPRINT ARGS/')
    # print(fingerprint_args)
    # print('/CACHE FINGERPRINT ARGS')
    
    sprintf('.cache/%s_%s.RDS', cachetype, fingerprint)
}


# Run diagnoses on designer `designer` and parameter space `args`. Run `sims` simulations and `bootstrap_sims` bootstraps.
# `diagnosands_call` is a closure to calculate the diagnosands
# If `use_cache` is TRUE, check if simulated data already exists for this designer / parameter combinations and use cached
# data or create newly simulated data for running diagnoses.
# The simulations are run in parallel if packages `future` and `future.apply` are installed.
run_diagnoses <- function(designer, args, sims, bootstrap_sims, diagnosands_call, use_cache = TRUE, advance_progressbar = 0) {
    # set up to run in parallel
    plan('multicore', workers = n_diagnosis_workers)
    
    all_designs <- NULL
    if (use_cache) {
        # cache fingerprint generated from designer object, simulation config. and parameter space
        designs_cache_file <- get_diagnosis_cache_filename('designs', args, NULL, NULL, designer)
        
        if (file.exists(designs_cache_file)) {   # read and return result object from cache
            if (advance_progressbar) incProgress(advance_progressbar)
            all_designs <- readRDS(designs_cache_file)
            print('loaded generated designs from cache')
        }
    } else {
        designs_cache_file <- NULL
    }

    if (is.null(all_designs)) {  # generate designs
        # generate designs from designer with arguments `args`
        all_designs <- eval_bare(expr(expand_design(designer = designer, expand = TRUE, !!!args)))
        if (advance_progressbar) incProgress(advance_progressbar)
        
        # save designs to cache if requested
        if (!is.null(all_designs) && !is.null(designs_cache_file)) {
            saveRDS(all_designs, designs_cache_file)
        }
    }
    
    from_cache <- FALSE   # records if some data was loaded from cache
    simdata <- NULL
    if (use_cache) {
        # cache fingerprint generated from designer object, simulation config. and parameter space
        cache_file <- get_diagnosis_cache_filename('simdata', args, sims, NULL, designer)
        
        if (file.exists(cache_file)) {   # read and return result object from cache
            if (advance_progressbar) incProgress(advance_progressbar)
            simdata <- readRDS(cache_file)
            print('loaded simulation data from cache')
            from_cache <- TRUE
        }
    } else {
        cache_file <- NULL
    }

    
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
        diag_call_src <- deparse(diagnosands_call)
        diag_call_env <- environment(diagnosands_call)
        if (is.null(diag_call_env)) {
            diag_call_objnames <- NULL
            diag_call_objvals <- NULL
        } else {
            diag_call_objnames <- ls(diag_call_env)
            diag_call_objvals <- get(diag_call_objnames, diag_call_env)
        }
        
        args <- list(
            'diag_call_src' = diag_call_src,
            'diag_call_objnames' = diag_call_objnames,
            'diag_call_objvals' = diag_call_objvals,
            'from_simdata' = cache_file
        )
        
        diag_cache_file <- get_diagnosis_cache_filename('diagresult', args, sims, bootstrap_sims, designer)
        
        if (file.exists(diag_cache_file)) {   # read and return result object from cache
            if (advance_progressbar) incProgress(advance_progressbar)
            diag_res <- readRDS(diag_cache_file)
            print('loaded diagnosis results from cache')
            from_cache <- TRUE
        }
    } else {
        diag_cache_file <- NULL
    }

    if (is.null(diag_res)) {  # run diagnoses using the simulated data
        diag_res <- diagnose_designs(simdata, diagnosands = diagnosands_call, bootstrap_sims = bootstrap_sims)
        if (advance_progressbar) incProgress(advance_progressbar)
        
        # save diagnosis results to cache if requested
        if (!is.null(diag_res) && !is.null(diag_cache_file)) {
            saveRDS(diag_res, diag_cache_file)
        }
    }

    list(results = diag_res, from_cache = from_cache)
}

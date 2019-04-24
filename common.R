# Common utility functions.
#
# Markus Konrad <markus.konrad@wzb.eu>
# Sisi Huang <sisi.huang@wzb.eu>
#
# Oct. 2018
#

library(shiny)
library(stringr)
library(future)
library(rlang)
library(digest)
library(reshape)
library(tidyr)
library(DeclareDesign)


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

# Get "tips" from `definitions` attribute of designer
get_tips <- function(designer){
    def <- attr(designer, "definitions")
    split(def$tips, def$names)
}
    
# Check if lists `a` and `b` have equal elements in a "shallow" way, i.e. *not* traversing recursively
# through nested lists.
lists_equal_shallow <- function(a, b, na.rm = FALSE) {
    if (na.rm) {
        a <- a[!is.na(a)]
        b <- b[!is.na(b)]
    }
    
    if (!setequal(names(a), names(b))) {
        return(FALSE)
    }
    
    all(sapply(names(a), function (e) {
        a_elem <- a[[e]]
        b_elem <- b[[e]]
        
        if (na.rm) {  # doing this here already because we need to remove NAs before checking length()
            a_elem <- a_elem[!is.na(a_elem)]
            b_elem <- b_elem[!is.na(b_elem)]
        }
        
        is.numeric(a_elem) && is.numeric(b_elem) && length(a_elem) == length(b_elem) && all(a_elem == b_elem)
    }))
}


# Round numeric values in a data frame to `digits`.
# Copied from "wizard_shiny" repository.
round_df <- function(df, digits){
    i <- vapply(df, is.numeric, TRUE)
    df[i] <- lapply(df[i], round, digits)
    df
}


# reshape the diagnosis table
reshape_data <- function(data){
    # coefficients
    coef_var <- DeclareDesign:::default_diagnosands(NULL)$diagnosand_label
    # standrad errors of coefficients
    se_var <- paste0("se(", coef_var, ")")
    # remove all the NA 
    if(any(is.na(data))) data <- data[complete.cases(data[c(coef_var, se_var)]),]
    # melt data to the long shape
    newdata <- melt(data, id = colnames(data)[!colnames(data) %in% c(coef_var, se_var)])
    # round the data as 2- digital 
    newdata$value <- format(round(newdata$value, 2), nsmall = 2)
    # remove the space, if the value is negative, other positive values would produce empty space
    newdata$value <- gsub(" ", "", newdata$value)
    # add bracket on the values of se
    newdata$value[grepl("^se", newdata$variable)] <- paste0("(",newdata$value[grepl("^se", newdata$variable)],")")
    # remove "se()" in the variable name 
    newdata$variable <- gsub("^se\\(|\\)", "", newdata$variable)
    # spread single columns into mutiple columns  
    reshape_data <- as.data.frame(newdata %>% group_by(variable) %>% mutate(i = row_number()) %>% spread(variable, value) %>% select(-i))
    return(reshape_data)
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
        elems <- elems[nchar(elems) > 0]
        if (cls %in% c('numeric', 'integer')) {
            if (length(elems) == 1 && elems == '') {
                return(numeric())
            } else {
                return(as.numeric(elems))
            }
        } else {
            if (length(elems) == 1 && elems == '') {
                return(character())
            } else {
                return(elems)
            }
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
    if (str_trim(s) == '') return(list())
    
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

# evaluate argument defaults of designers in separate environment (because they might be "language" constructs)
# return evaluated argument defaults
# `args` is a list of arguments with defaults as returned from `formals(<designer>)`
evaluate_designer_args <- function(args) {
    eval_envir <- new.env()
    
    args_eval <- lapply(1:length(args), function(a){
        evaluated_arg <- invisible(eval(args[[a]], envir = eval_envir))
        invisible(assign(x = names(args)[a], value = evaluated_arg, envir = eval_envir))
        hold <- invisible(get(names(args)[a], envir = eval_envir))
        if(length(hold) > 1) hold <- paste0(hold, collapse = ", ")
        return(hold)
    })
    
    names(args_eval) <- names(args)
    args_eval
}

# get cache file name unique to cache type `cachetype` (designs, simulation or diagnosis results),
# parameter space `args`, number of (bootstrap) simulations `sims`, designer object `designer`
get_diagnosis_cache_filename <- function(cachetype, args, sims, bs_sims, designer) {
    fingerprint_args <- args
    fingerprint_args$sims <- sims
    fingerprint_args$bs_sims <- bs_sims
    fingerprint_args$designer_src <- deparse(designer)
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
run_diagnoses <- function(designer, args, sims, bootstrap_sims, diagnosands_call, use_cache = TRUE,
                          advance_progressbar = 0, n_diagnosis_workers = 1) {
    if (n_diagnosis_workers > 1) {
        # set up to run in parallel
        plan('multicore', workers = n_diagnosis_workers)
    }
    
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

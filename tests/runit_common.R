library(RUnit)

source('../common.R')

test.list_merge <- function() {
    checkEquals(list_merge(list(a = 1), list(b = 2)),
                list(a = 1, b = 2))
    
    checkEquals(list_merge(list(a = 1), list(a = 3, b = 2)),
                list(a = 3, b = 2))
    
    checkEquals(list_merge(list(a = 1), list()),
                list(a = 1))
    
    checkEquals(list_merge(list(), list(a = 1)),
                list(a = 1))
    
    checkEquals(list_merge(list(), list()),
                list())
}


test.lists_equal_shallow <- function() {
    checkTrue(lists_equal_shallow(list(a = 1), list(a = 1)))
    checkTrue(!lists_equal_shallow(list(a = 1), list(b = 1)))
    checkTrue(!lists_equal_shallow(list(a = 1, b = NA), list(a = 1, b = NA), na.rm = FALSE))
    checkTrue(lists_equal_shallow(list(a = 1, b = NA), list(a = 1, b = NA), na.rm = TRUE))
}


test.parse_sequence_string <- function() {
    checkEquals(parse_sequence_string(''), numeric())
    checkEquals(parse_sequence_string('1'), 1)
    checkEquals(parse_sequence_string('1,2'), c(1, 2))
    checkEquals(parse_sequence_string('1, 2'), c(1, 2))
    checkEquals(parse_sequence_string('  1,  2 '), c(1, 2))
    checkEquals(parse_sequence_string('1.2'), 1.2)
    checkEquals(parse_sequence_string('1.2, 3, 4'), c(1.2, 3, 4))
    
    checkEquals(parse_sequence_string('', cls = 'character'), character())
    checkEquals(parse_sequence_string('a', cls = 'character'), 'a')
    checkEquals(parse_sequence_string('a, b', cls = 'character'), c('a', 'b'))
    checkEquals(parse_sequence_string('  a, b,   c ', cls = 'character'), c('a', 'b', 'c'))
    
    # sequences
    checkEquals(parse_sequence_string('1, 2 ... 10'), 1:10)
    checkEquals(parse_sequence_string('1, 2, ... 10'), 1:10)
    checkEquals(parse_sequence_string('1, 2, ..., 10'), 1:10)
    checkEquals(parse_sequence_string('1, 2, ... , 10'), 1:10)
    checkEquals(parse_sequence_string('1, 2 ... 2'), 1:2)
    checkEquals(parse_sequence_string('1, 2 ... 3'), 1:3)
    
    checkEquals(parse_sequence_string('100, 110 ... 200'), seq(100, 200, by = 10))
    checkEquals(parse_sequence_string('100, 99 ... 0'), 100:0)
    checkException(parse_sequence_string('100, 99 ... 200'))
    
    checkEquals(parse_sequence_string('-1, 0 ... 3'), -1:3)
    checkEquals(parse_sequence_string('-1, -2 ... -10'), -1:-10)
    checkEquals(parse_sequence_string('-10, -15 ... -30'), seq(-10, -30, by = -5))
}


test.parse_sequence_of_sequences_string <- function() {
    checkEquals(parse_sequence_of_sequences_string(''), list())
    checkEquals(parse_sequence_of_sequences_string('(1, 2, 3)'), list(1:3))
    checkEquals(parse_sequence_of_sequences_string('(1, 2, 3), (4, 5, 6), (7,8, 9)'), list(1:3, 4:6, 7:9))
    checkEquals(parse_sequence_of_sequences_string('(1, 2, 3) (4, 5, 6)     (7,8, 9)'), list(1:3, 4:6, 7:9))
    checkEquals(parse_sequence_of_sequences_string('(1, 2, 3)\n(4, 5, 6)\n(7,8, 9)'), list(1:3, 4:6, 7:9))
    checkEquals(parse_sequence_of_sequences_string('(10, 20 ... 50), (0, 5 ... 20)'), list(seq(10, 50, by = 10), seq(0, 20, by = 5)))
    
    checkEquals(parse_sequence_of_sequences_string('(1, 2, 3)\n(4, 5, 6)\n(7,8, 9)', require_rectangular = TRUE),
                matrix(1:9, ncol = 3, byrow = TRUE))
}


test.get_args_for_inspection_two_arm <- function() {
    d <- DesignLibrary::two_arm_designer
    d_args <- get_designer_args(d)
    d_argdefs <- attr(d, 'definitions')
    
    argnames <- names(d_args)
    
    d_args <- lapply(names(d_args), function(argname) {
        argval <- d_args[[argname]]
        if (!(class(argval) %in% c('integer', 'numeric', 'character'))) {
            ''
        } else {
            argval
        }
    })
    names(d_args) <- argnames
    
    # simple check: one varying parameter N
    
    insp_input <- d_args
    insp_input$N <- '10, 20 ... 100'
    insp_input$ate <- 2
    names(insp_input) <- paste0('inspect_arg_', argnames)
    
    design_input <- d_args
    names(design_input) <- paste0('design_arg_', argnames)
    
    checkEquals(get_args_for_inspection(d, d_argdefs, insp_input, character(), design_input),
                list(N = seq(10, 100, by = 10),
                     assignment_prob = d_args$assignment_prob,
                     control_mean = d_args$control_mean,
                     control_sd = d_args$control_sd,
                     ate = 2,
                     rho = d_args$rho))
    
    # check NULL input from inspector
    
    insp_input <- d_args
    insp_input$N <- '10, 20 ... 100'
    insp_input$control_mean <- NULL        # these values are not set in the inspector ...
    insp_input$ate <- NULL
    names(insp_input) <- paste0('inspect_arg_', setdiff(argnames, c('control_mean', 'ate')))
    
    checkEquals(get_args_for_inspection(d, d_argdefs, insp_input, character(), design_input),
                list(N = seq(10, 100, by = 10),
                     assignment_prob = d_args$assignment_prob,
                     control_mean = d_args$control_mean,  # ... they equal the values ...
                     control_sd = d_args$control_sd,      
                     ate = d_args$ate,                    # ... from the design input.
                     rho = d_args$rho))
    
    # check effect of fixed_args
    
    insp_input <- d_args
    insp_input$N <- '10, 20 ... 100'
    insp_input$control_mean <- 0.1   # set control_mean and ATE in inspector, but since these are passed as fixed ...
    insp_input$ate <- 2
    names(insp_input) <- paste0('inspect_arg_', argnames)
    
    checkEquals(get_args_for_inspection(d, d_argdefs, insp_input, c('control_mean', 'ate'), design_input),
                list(N = seq(10, 100, by = 10),
                     assignment_prob = d_args$assignment_prob,
                     control_mean = d_args$control_mean,  # ... they equal the values ...
                     control_sd = d_args$control_sd,      
                     ate = d_args$ate,                    # ... from the design input.
                     rho = d_args$rho))
}

test.get_args_for_inspection_binary_iv <- function() {
    d <- DesignLibrary::binary_iv_designer
    d_args <- get_designer_args(d)
    
    # manually setting the defaults here
    d_args$type_probs <- c(1/3, 1/3, 1/3, 0)
    d_args$a <- c(1, 0, 0, 0)
    d_args$b <- rep(0, 4)
    d_args$d <- rep(0, 4)
    
    d_argdefs <- attr(d, 'definitions')
    argnames <- names(d_args)
    
    # check sequence of sequences inputs from inspector
    
    insp_input <- d_args
    insp_input$assignment_probs <- '(.5, .5, .5, .5) (.1, .2, .3, .4) (.5, .6, .7, .8)'       # seq. of sequences input
    # defaults for seq. of seq. inputs as they come from the inspector
    insp_input$type_probs <- paste0('(', paste(insp_input$type_probs, collapse = ','), ')')
    insp_input$a <- paste0('(', paste(insp_input$a, collapse = ','), ')')
    insp_input$b <- paste0('(', paste(insp_input$b, collapse = ','), ')')
    insp_input$d <- paste0('(', paste(insp_input$d, collapse = ','), ')')
    names(insp_input) <- paste0('inspect_arg_', argnames)
    
    design_input <- d_args
    names(design_input) <- paste0('design_arg_', argnames)
    
    checkEquals(get_args_for_inspection(d, d_argdefs, insp_input, character(), design_input),
                list(N = d_args$N,
                     type_probs = list(d_args$type_probs),   # seq. of seq. parameter is always a list of sequences
                     assignment_probs = list(rep(0.5, 4), seq(0.1, 0.4, by = 0.1), seq(0.5, 0.8, by = 0.1)),
                     a_Y = d_args$a_Y,
                     b_Y = d_args$b_Y,
                     d_Y = d_args$d_Y,
                     outcome_sd = d_args$outcome_sd,
                     a = list(d_args$a),
                     b = list(d_args$b),
                     d = list(d_args$d)
    ))
}


test.get_diagnosis_cache_filename <- function() {
    pttrn <- '^\\.cache/test_[0-9a-f]{32}\\.RDS$'
    
    cachefile <- get_diagnosis_cache_filename('test', list(), 1, 1, DesignLibrary::two_arm_designer)
    cachefile <- c(cachefile, get_diagnosis_cache_filename('test', list(), 1, 1, DesignLibrary::two_arm_designer))
    cachefile <- c(cachefile, get_diagnosis_cache_filename('test', list(a = 123), 1, 1, DesignLibrary::two_arm_designer))
    cachefile <- c(cachefile, get_diagnosis_cache_filename('test', list(a = 123), 1, 2, DesignLibrary::two_arm_designer))
    cachefile <- c(cachefile, get_diagnosis_cache_filename('test', list(a = 123), 2, 1, DesignLibrary::two_arm_designer))
    cachefile <- c(cachefile, get_diagnosis_cache_filename('test', list(a = 123), 2, 1, DesignLibrary::binary_iv_designer))
    
    checkTrue(all(grepl(pttrn, cachefile)))
    checkTrue(cachefile[1] == cachefile[2])
    checkTrue(cachefile[2] != cachefile[3])
    checkTrue(cachefile[3] != cachefile[4])
    checkTrue(cachefile[4] != cachefile[5])
    checkTrue(cachefile[5] != cachefile[6])
}


test.run_diagnoses_two_arm <- function() {
    # Note: you should remove all *.RDS files in tests/.cache before running
    # this test (run_tests in Makefile does this automatically)
    
    d <- DesignLibrary::two_arm_designer
    insp_args <- list(N = c(10, 20, 30),       # run diagnoses for those three designs
                      assignment_prob = 0.5,
                      control_mean = 0,
                      control_sd = 1,
                      ate = 1,
                      rho = 1)
    
    diag_call <- function(diag_param_alpha) {  # here we can pass alpha
        function(data) {
            DeclareDesign:::default_diagnosands(data, alpha = diag_param_alpha)
        }
    }
    
    suppressWarnings({
        res <- run_diagnoses(d, insp_args, 2, 2, diagnosands_call = diag_call(0.05), use_cache = FALSE,
                             advance_progressbar = 0, n_diagnosis_workers = 1)
    })
    checkTrue(res$from_cache == FALSE)
    checkTrue(class(res$results) == 'diagnosis')
    checkTrue(nrow(res$results$diagnosands_df) == length(insp_args$N))
    checkTrue(all(res$results$diagnosands_df$N == insp_args$N))
    
    # check caching
    suppressWarnings({
        res1 <- run_diagnoses(d, insp_args, 30, 30, diagnosands_call = diag_call(0.05), use_cache = TRUE,
                              advance_progressbar = 0, n_diagnosis_workers = 1)
        res2 <- run_diagnoses(d, insp_args, 30, 30, diagnosands_call = diag_call(0.05), use_cache = TRUE,
                              advance_progressbar = 0, n_diagnosis_workers = 1)
        res3 <- run_diagnoses(d, insp_args, 30, 30, diagnosands_call = diag_call(0.01), use_cache = TRUE,
                              advance_progressbar = 0, n_diagnosis_workers = 1)
    })
    
    checkTrue(res2$from_cache)  # res2 comes fully from cache (designs, simulations, diagnosands)
    checkTrue(all(res1$results$simulations_df == res2$results$simulations_df))
    checkTrue(all(res1$results$diagnosands_df == res2$results$diagnosands_df))
    
    checkTrue(res3$from_cache)  # res3 is partly from cache: diagnosis is not from cache due to different diagnosands_call
    checkTrue(all(res2$results$simulations_df == res3$results$simulations_df))  # simulations are the same
    checkTrue(any(res2$results$diagnosands_df != res3$results$diagnosands_df))  # diagnosands are not the same
}
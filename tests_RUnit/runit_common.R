# Test suite for functions from common.R

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
    checkEquals(parse_sequence_string('1,'), 1)
    checkEquals(parse_sequence_string('1,  '), 1)
    checkEquals(parse_sequence_string('1,2'), c(1, 2))
    checkEquals(parse_sequence_string('1, 2'), c(1, 2))
    checkEquals(parse_sequence_string('1,2,'), c(1, 2))
    checkEquals(parse_sequence_string('1,2 ,'), c(1, 2))
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
    checkException(parse_sequence_string('100, 99 ... 200'), silent = TRUE)
    
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
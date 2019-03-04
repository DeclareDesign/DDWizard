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


# Test suite for functions from inspect_helpers.R

library(RUnit)

source('../conf.R')
source('../common.R')
source('../inspect_helpers.R')


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
    
    checkEquals(get_args_for_inspection(d, 'two_arm_designer', d_argdefs, insp_input, character(), design_input),
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
    
    # control_mean and ate do not appear in the results anymore since they're NULL
    checkEquals(get_args_for_inspection(d, 'two_arm_designer', d_argdefs, insp_input, character(), design_input),
                list(N = seq(10, 100, by = 10),
                     assignment_prob = d_args$assignment_prob,
                     control_sd = d_args$control_sd,
                     rho = d_args$rho))
    
    # check effect of fixed_args
    
    insp_input <- d_args
    insp_input$N <- '10, 20 ... 100'
    insp_input$control_mean <- 0.1   # set control_mean and ATE in inspector, but since these are passed as fixed ...
    insp_input$ate <- 2
    names(insp_input) <- paste0('inspect_arg_', argnames)
    
    checkEquals(get_args_for_inspection(d, 'two_arm_designer', d_argdefs, insp_input, c('control_mean', 'ate'), design_input),
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
    
    checkEquals(get_args_for_inspection(d, 'binary_iv_designer', d_argdefs, insp_input, character(), design_input),
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

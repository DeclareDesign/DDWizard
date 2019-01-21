# UI and server module for "Sandbox" tab.
#
# Clara Bicalho <clarabmcorreia@gmail.com>
# Markus Konrad <markus.konrad@wzb.eu>
#
# Jan. 2019
#


### UI ###

sandboxTabUI <- function(id, label = 'Sandbox') {
    nspace <- NS(id)
    
    material_tab_content(
        tab_id = id,
        material_row(
            material_column(  # left: definition of design steps
                width = 4,
                material_card("Steps",
                              textInput(nspace("design_name"), "Design name", value = "my_design"),
                              uiOutput(nspace("design_steps")),
                              actionButton(nspace("add_step"), "+")
                )
            ),
            material_column(  # right: design output
                width = 8,
                material_card("Configure step",
                              uiOutput(nspace("configure_step"))),
                material_card("Code",
                              verbatimTextOutput(nspace("design_code"))),
                material_card("Download",
                              downloadButton(nspace('download_r_script'), label = 'R code', disabled = 'disabled'),
                              downloadButton(nspace('download_rds_obj'), label = 'Design as RDS file', disabled = 'disabled')
                )
            )
        )
    )
}

### Server ###

step_conf <- list(
    'population' = list(
        'ui' = function(nspace, input, inp_prefix, react) {
            pop_conf_input_elems <- generate_population_all_vars_inputs(nspace, input, inp_prefix, react)
            
            conditionalPanel(
                condition = paste0('input.', inp_prefix, 'type == "population"'),
                material_row(
                    material_column(numericInput(nspace(paste0(inp_prefix, 'num_obs')), 'Number of observations',
                                                 min = 0, value = 10, step = 1)),
                    material_column(numericInput(nspace(paste0(inp_prefix, 'num_vars')), 'Number of variables',
                                                 min = 1, value = default_value(input, paste0(inp_prefix, 'num_vars'), 1), step = 1))
                ),
                pop_conf_input_elems,
                ns = nspace
            )
        },
        'code' = function(input, inp_prefix) {
            N <- input[[paste0(inp_prefix, 'num_obs')]]
            n_vars <- input[[paste0(inp_prefix, 'num_vars')]]
            
            pop_args <- list()
            for (i in 1:n_vars) {
                inp_prefix_arg <- paste0(inp_prefix, 'var', i, '_')
                varname <- input[[paste0(inp_prefix_arg, 'varname')]]
                vartype <- input[[paste0(inp_prefix_arg, 'vartype')]]
                
                if (!is.null(varname) && !is.null(vartype)) {
                    varcode_fn <- population_var_definitions[[vartype]]$code
                    
                    pop_args[[varname]] <- varcode_fn(input, inp_prefix_arg)
                }
            }
            
            expr(declare_population(N = !!N, !!!pop_args))
        }
    ),
    'sampling' = list(
        'ui' = function(nspace, input, inp_prefix, react) {
            conditionalPanel(
                condition = paste0('input.', inp_prefix, 'type == "sampling"'),
                p('TODO: Sampling configuration'),
                ns = nspace
            )
        },
        'code' = function(input) {
            
        }
    )
)


# many code parts copied from fabricatr_shiny and modified
# orig. author: Aaron Rudkin
# TODO: put in separate source file

population_inputs <- list(
    'prob' = function(nspace, id, default) {
        list(numericInput(nspace(id),
                          label = "Probability",
                          min = 0, max = 1,
                          value = default,
                          step = 0.01))
    },
    'trials' = function(nspace, id, default) {
        list(numericInput(nspace(id),
                          label = "Number of trials",
                          value = default,
                          min = 1))
    },
    'mean_count' = function(nspace, id, default) {
        list(numericInput(nspace(id),
                          label = "Mean count",
                          value = default,
                          min = 0))
    },
    'ordered_likert_breaks' = function(nspace, id, default) {
        list(
            textInput(nspace(id),
                      label = "Break points for ordered categories",
                      value = default[[1]]),
            textInput(nspace(paste0(id, '_labels')),
                      label = "Unquoted break labels",
                      value = default[[2]])
        )
    },
    'latent_var' = function(nspace, id, default) {
        input_norm_mean <- numericInput(
            nspace(paste0(id, '_norm_mean')),
            label = "Mean",
            value = default$norm_mean
        )
            
        input_norm_sd <- numericInput(
            nspace(paste0(id, '_norm_sd')),
            label = "SD",
            value = default$norm_sd
        )

        input_unif_min <- numericInput(
            nspace(paste0(id, '_unif_min')),
            label = "Min",
            value = default$unif_min
        )
            
        input_unif_max <- numericInput(
            nspace(paste0(id, '_unif_max')),
            label = "Max",
            value = default$unif_max
        )
        
        list(
            selectInput(nspace(id),
                        label = "Latent Variable",
                        choices = list(
                            "Normal" = "rnorm",
                            "Uniform" = "runif"
                        ),
                        selected = default$type
            ),
            conditionalPanel(condition = paste0('input.', id, ' == "rnorm"'),
                             input_norm_mean, input_norm_sd,
                             ns = nspace),
            conditionalPanel(condition = paste0('input.', id, ' == "runif"'),
                             input_unif_min, input_unif_max,
                             ns = nspace)
        )
    }
)

population_input_defaults <- list(
    'prob' = 0.5,
    'trials' = 1,
    'mean_count' = 1,
    'ordered_likert_breaks' = list(
        '-1.5, -0.5, 0.5, 1.5',
        'Strongly Disagree, Disagree, Neutral, Agree, Strongly Agree'
    ),
    'latent_var' = list(
        'type' = 'rnorm',
        'norm_mean' = 0,
        'norm_sd' = 1,
        'unif_min' = -2.5,
        'unif_max' = 2.5
    )
)

population_var_definitions <- list(
    'binary' = list(
        'requires' = c('prob'),
        'code' = function(input, inp_prefix) {
            prob <- default_value(input, paste0(inp_prefix, 'prob'), population_input_defaults$prob)
            expr(draw_binary(N = N, prob = !!prob))
        }
    ),
    'binomial' = list(
        'requires' = c('trials', 'prob'),
        'code' = function(input, inp_prefix) {
            prob <- default_value(input, paste0(inp_prefix, 'prob'), population_input_defaults$prob)
            trials <- default_value(input, paste0(inp_prefix, 'trials'), population_input_defaults$trials)
            expr(draw_binomial(N = N, prob = !!prob, trials = !!trials))
        }
    ),
    'count' = list(
        'requires' = c('mean_count'),
        'code' = function(input, inp_prefix) {
            mean_count <- default_value(input, paste0(inp_prefix, 'mean_count'), population_input_defaults$mean_count)
            expr(draw_count(N = N, mean = !!mean_count))
        }
    ),
    'ordered' = list(
        'requires' = c('ordered_likert_breaks', 'latent_var'),
        'code' = function(input, inp_prefix) {
            breaks <- default_value(input, paste0(inp_prefix, 'ordered_likert_breaks'),
                                    population_input_defaults$ordered_likert_breaks[[1]])
            breaks <- c(-Inf, as.numeric(str_trim(unlist(strsplit(breaks, "[,;]")))), Inf)
            break_labels <- default_value(input, paste0(inp_prefix, 'ordered_likert_breaks_labels'),
                                           population_input_defaults$ordered_likert_breaks[[2]])
            break_labels <- str_trim(unlist(strsplit(break_labels, "[,;]")))
            
            latent_type <- default_value(input, paste0(inp_prefix, 'latent_var'),
                                         population_input_defaults$latent_var$type)
            
            if (latent_type == 'rnorm') {
                latent_var_mean <- default_value(input, paste0(inp_prefix, 'latent_var_norm_mean'),
                                                 population_input_defaults$latent_var$norm_mean)
                latent_var_sd <- default_value(input, paste0(inp_prefix, 'latent_var_norm_sd'),
                                               population_input_defaults$latent_var$norm_sd)
                
                latent_var_expr <- expr(rnorm(n = N,
                                        mean = !!latent_var_mean,
                                        sd = !!latent_var_sd))
            } else {  # runif
                latent_var_unif_min <- default_value(input, paste0(inp_prefix, 'latent_var_unif_min'),
                                                     population_input_defaults$latent_var$unif_min)
                latent_var_unif_max <- default_value(input, paste0(inp_prefix, 'latent_var_unif_max'),
                                                     population_input_defaults$latent_var$unif_max)
                
                latent_var_expr <- expr(runif(n = N,
                                              min = !!latent_var_unif_min,
                                              max = !!latent_var_unif_max))
            }
            
            expr(draw_ordered(N = N, x = !!latent_var_expr, breaks = !!breaks, break_labels = !!break_labels))
        }
    )
)

generate_population_single_var_inputs <- function(nspace, input, inp_prefix, i) {
    inp_id_name <- paste0(inp_prefix, 'varname')
    inp_id_type <- paste0(inp_prefix, 'vartype')
    
    population_var_types <- names(population_var_definitions)
    
    var_type <- default_value(input, inp_id_type, population_var_types[1])
    
    generate_args <- list(
        textInput(nspace(inp_id_name), 'Name',
                  default_value(input, inp_id_name, paste0('X', i))),
        selectInput(nspace(inp_id_type), 'Type', population_var_types, var_type)
    )
    
    for (inp_type in population_var_definitions[[var_type]]$requires) {
        pop_input_elems_fn <- population_inputs[[inp_type]]
        arg_input_elems <- pop_input_elems_fn(nspace, paste0(inp_prefix, inp_type),
                                              population_input_defaults[[inp_type]])
        for (e in arg_input_elems) {
            generate_args <- list_append(generate_args, e)
        }
    }
    
    do.call(wellPanel, generate_args)
}

generate_population_all_vars_inputs <- function(nspace, input, inp_prefix, react) {
    n_vars <- input[[paste0(inp_prefix, 'num_vars')]]
    if (!is.null(n_vars)) {
        input_boxes_vars <- lapply(1:n_vars, function(i) {
            generate_population_single_var_inputs(nspace, input, paste0(inp_prefix, 'var', i, '_'), i)
        })
        
        is_left <- as.logical(1:length(input_boxes_vars) %% 2)
        boxes_left <- input_boxes_vars[is_left]
        boxes_right <- input_boxes_vars[!is_left]
        
        material_row(
            do.call(material_column, boxes_left),
            do.call(material_column, boxes_right)
        )
    }
}


sandboxTab <- function(input, output, session) {
    nspace <- NS('tab_sandbox')
    
    react <- reactiveValues(
        n_steps = 1,
        conf_step = 1
        # step_inputs = list(default_step_input)
    )
    
    output$selected_step <- reactive({ react$conf_step })
    
    ### input observers ###
    
    # input observer for adding a new step
    observeEvent(input$add_step, {
        react$n_steps <- react$n_steps + 1
        react$conf_step <- react$n_steps
    })
    
    ### UI outputs ###
    
    output$design_steps <- renderUI({
        boxes <- list()
        
        first_step_name <- NULL
        
        for(i in 1:react$n_steps) {
            inp_prefix <- sprintf('step%d_', i)
            
            inp_id_btn <- paste0(inp_prefix, 'select')
            
            cur_step_type <- input[[paste0(inp_prefix, 'type')]]
            
            if (is.null(cur_step_type)) {
                step_name <- sprintf('Step %d', i)
            } else {
                step_name <- sprintf('Step %d (%s)', i, cur_step_type)
            }
            
            select_step_btn <- actionButton(nspace(inp_id_btn), step_name, width = '100%')
            
            if (!(inp_id_btn %in% names(reactiveValuesToList(input)))) {   # add dynamic event observer
                observeEvent(input[[inp_id_btn]], {
                    react$conf_step <- i
                    print(paste('configure step', i))
                })
            }
            
            boxes <- list_append(boxes, select_step_btn)
        }

        boxes
    })
    
    output$configure_step <- renderUI({
        available_types <- names(step_conf)
        
        boxes <- list()
        
        cur_step <- react$conf_step
        cur_step_type <- input[[sprintf('step%d_type', cur_step)]]
        
        if (is.null(cur_step_type)) {
            step_name <- sprintf('Step %d', cur_step)
        } else {
            step_name <- sprintf('Step %d (%s)', cur_step, cur_step_type)
        }
        boxes <- list_append(boxes, h6(step_name))
        
        for(i in 1:react$n_steps) {
            inp_prefix <- sprintf('step%d_', i)
            
            inp_id_type <- paste0(inp_prefix, 'type')
            cur_step_type <- input[[inp_id_type]]
    
            if (is.null(cur_step_type)) {
                cur_step_type <- available_types[1]
            }
            
            inp_type <- selectInput(nspace(inp_id_type), 'Type', available_types, cur_step_type)
            
            step_showhide_panel <- div(    # couldn't get conditionalPanel to work together with actionButton, so I'm using this...
                style = paste0('display:', ifelse(cur_step == i, 'block', 'none')),
                inp_type,
                step_conf[[cur_step_type]]$ui(nspace, input, inp_prefix, react)
            )
            
            boxes <- list_append(boxes, step_showhide_panel)
        }
        
        boxes
    })

    output$design_code <- renderText({
        code_steps <- character()
        code_steps_objectnames <- character()

        for(i in 1:react$n_steps) {
            inp_prefix <- sprintf('step%d_', i)

            inp_id_type <- paste0(inp_prefix, 'type')
            step_type <- input[[inp_id_type]]

            if (!is.null(step_type)) {
                step_type_code_fn <- step_conf[[step_type]]$code
                step_type_code <- step_type_code_fn(input, inp_prefix)
                
                if (!is.null(step_type_code)) {
                    body <- deparse(step_type_code)
                    
                    step_name <- generate_unique_name(step_type, code_steps_objectnames)
                    code_steps <- c(code_steps, paste0(c(paste0(step_name, " <- "), rep("", length(body)-1)), body))
                    code_steps_objectnames <- c(code_steps_objectnames, step_name)
                    #print(code_steps[[i]])
                    #print('---')
                }
            }
        }

        code_steps <- c(code_steps, paste0(paste0(input$design_name, " <- "), paste0(code_steps_objectnames, collapse = " + ")))
        paste0(code_steps, collapse = "\n")
    })
}

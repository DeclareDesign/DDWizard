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

STEP_TYPES <- list(
    population = list(
        "N obs, W and X cov" = function(N = 10) eval_bare(expr(declare_population(N = !!N, W = runif(!!N), X = runif(!!N)))),
        "N obs, Y and Z cov" = function(N = 10) eval_bare(expr(declare_population(N = !!N, Y = runif(!!N), Z = runif(!!N))))
    ),
    sampling = list(
        "Sample n" = function(n = NULL) eval_bare(expr(declare_sampling(n = !!n)))
    )
)

default_step_type = "population"
default_step_template = "N obs, W and X cov"

 
# default_step_input <- list(
#     type = default_step_type,
#     template = default_step_template
# )

sandboxTab <- function(input, output, session) {
    nspace <- NS('tab_sandbox')
    
    react <- reactiveValues(
        n_steps = 1
        # step_inputs = list(default_step_input)
    )
    
    ### input observers ###
    
    # input observer for adding a new step
    observeEvent(input$add_step, {
        # react$step_inputs <- list_append(react$step_inputs, default_step_input)
        react$n_steps <- react$n_steps + 1
        #open_step <- sprintf('Step %d (%s)', react$n_steps, default_step_type)
        #updateCollapse(session, nspace('steps_collapsable'), open = open_step)
    })
    
    ### UI outputs ###
    
    output$design_steps <- renderUI({
        available_types <- names(STEP_TYPES)
        
        boxes <- list()
        
        first_step_name <- NULL
        
        for(i in 1:react$n_steps) {
            inp_prefix <- sprintf('step%d_', i)
            
            inp_id_type <- paste0(inp_prefix, 'type')
            cur_step_type <- input[[inp_id_type]]
            
            if (is.null(cur_step_type)) {
                cur_step_type <- default_step_type
            }
            
            step_name <- sprintf('Step %d (%s)', i, cur_step_type)
            step_output_elems <- list(step_name)
            if (i == 1) {
                first_step_name <- step_name
            }
            
            inp_type <- selectInput(nspace(inp_id_type), 'Type', available_types, cur_step_type)
            step_output_elems <- list_append(step_output_elems, inp_type)
            
            step_templates <- STEP_TYPES[[cur_step_type]]
            inp_id_template <- paste0(inp_prefix, 'template')
            cur_step_template <- input[[inp_id_template]]
            
            if (is.null(cur_step_template)) {
                cur_step_template <- default_step_template
            }
            
            inp_template <- selectInput(nspace(inp_id_template), 'Parameter template',
                                        names(step_templates), cur_step_template)
            step_output_elems <- list_append(step_output_elems, inp_template)
            
            stepfn_args <- formals(step_templates[[cur_step_template]])
            
            for (arg_name in names(stepfn_args)) {
                arg_default <- stepfn_args[[arg_name]]
                
                inp_id_stepfn_arg <- paste0(inp_prefix, 'arg_', arg_name)
                inp_stepfn_arg <- textInput(nspace(inp_id_stepfn_arg), arg_name, arg_default)
                step_output_elems <- list_append(step_output_elems, inp_stepfn_arg)
            }
            
            boxes <- list_append(boxes, do.call(bsCollapsePanel, step_output_elems))
        }
        
        boxes$id <- nspace('steps_collapsable')
        boxes$multiple <- TRUE
        boxes$open <- first_step_name
        
        do.call(bsCollapse, boxes)
    })
    
    output$design_code <- renderText({
        code_steps <- list()
        code_steps_objectnames <- c()
        
        for(i in 1:react$n_steps) {
            #print(i)
            inp_prefix <- sprintf('step%d_', i)
            
            inp_id_type <- paste0(inp_prefix, 'type')
            step_type <- input[[inp_id_type]]
            
            if (is.null(step_type)) {
                step_type <- default_step_type
            }
            
            inp_id_template <- paste0(inp_prefix, 'template')
            step_template <- input[[inp_id_template]]
            
            if (is.null(step_template)) {
                step_template <- default_step_template
            }
            
            step_fn <- STEP_TYPES[[step_type]][[step_template]]
            step_fn_args <- names(formals(step_fn))
            step_fn_args <- setdiff(step_fn_args, "data")
            
            step_inputs <- sapply(step_fn_args, function(arg_name) expr(!!as.numeric(input[[paste0(inp_prefix, 'arg_', arg_name)]])))
            #print(step_inputs)
            
            if (length(step_fn_args) != 0) {
                body <- capture.output(do.call(step_fn, exprs(!!!(step_inputs))))
                
            } else {
                body <- capture.output(rlang::expr(!!step_fn))
            }
            
            step_name <- generate_unique_name(step_type, code_steps_objectnames)
            code_steps[[i]] <- paste0(c(paste0(step_name, " <- "), rep("", length(body)-1)), body)
            code_steps_objectnames <- c(code_steps_objectnames, step_name)
            #print(code_steps[[i]])
            #print('---')
        }
        
        code_steps[[react$n_steps+1]] <- paste0(paste0(input$design_name, " <- "), paste0(code_steps_objectnames, collapse = " + "))
        paste0(unlist(code_steps), collapse = "\n")
    })
}

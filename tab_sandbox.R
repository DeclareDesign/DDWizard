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
                              uiOutput(nspace("design_steps")),
                              actionButton(nspace("add_step"), "+")
                )
            ),
            material_column(  # right: design output
                width = 8,
                material_card("Code"),
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
    react <- reactiveValues(
        n_steps = 1
        # step_inputs = list(default_step_input)
    )
    
    ### input observers ###
    
    # input observer for adding a new step
    observeEvent(input$add_step, {
        # react$step_inputs <- list_append(react$step_inputs, default_step_input)
        react$n_steps <- react$n_steps + 1
    })
    
    ### UI outputs ###
    
    output$design_steps <- renderUI({
        nspace <- NS('tab_sandbox')
        available_types <- names(STEP_TYPES)
        
        boxes <- list()
        
        for(i in 1:react$n_steps) {
            inp_prefix <- sprintf('step%d_', i)
            
            inp_id_type <- paste0(inp_prefix, 'type')
            cur_step_type <- input[[inp_id_type]]
            
            if (is.null(cur_step_type)) {
                cur_step_type <- default_step_type
            }
            
            inp_type <- selectInput(nspace(inp_id_type), 'Type', available_types, cur_step_type)
            boxes <- list_append(boxes, inp_type)
            
            step_templates <- STEP_TYPES[[cur_step_type]]
            inp_id_template <- paste0(inp_prefix, 'template')
            cur_step_template <- input[[inp_id_template]]
            
            if (is.null(cur_step_template)) {
                cur_step_template <- default_step_template
            }
            
            inp_template <- selectInput(nspace(inp_id_template), 'Parameter template',
                                        names(step_templates), cur_step_template)
            boxes <- list_append(boxes, inp_template)
            
            stepfn_args <- formals(step_templates[[cur_step_template]])
            
            for (arg_name in names(stepfn_args)) {
                arg_default <- stepfn_args[[arg_name]]
                
                inp_id_stepfn_arg <- paste0(inp_prefix, 'arg_', arg_name)
                inp_stepfn_arg <- textInput(nspace(inp_id_stepfn_arg), arg_name, arg_default)
                boxes <- list_append(boxes, inp_stepfn_arg)
            }
        }
        
        boxes
    })
}

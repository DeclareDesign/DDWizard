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

default_step_type = "population"

step_conf_panels <- list(
    'population' = function(nspace, input, inp_prefix) {
        conditionalPanel(
            condition = paste0('input.', inp_prefix, 'type == "population"'),
            p('Population conf.'),
            ns = nspace
        )
    },
    'sampling' = function(nspace, input, inp_prefix) {
        conditionalPanel(
            condition = paste0('input.', inp_prefix, 'type == "sampling"'),
            p('Sampling conf.'),
            ns = nspace
        )
    }
)

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
        available_types <- names(step_conf_panels)
        
        boxes <- list()
        
        cur_step <- react$conf_step
        cur_step_type <- input[[sprintf('step%d_type', cur_step)]]
        
        if (is.null(cur_step_type)) {
            step_name <- sprintf('Step %d', cur_step)
        } else {
            step_name <- sprintf('Step %d (%s)', cur_step, cur_step_type)
        }
        boxes <- list_append(boxes, p(step_name))
        
        for(i in 1:react$n_steps) {
            inp_prefix <- sprintf('step%d_', i)
            
            inp_id_type <- paste0(inp_prefix, 'type')
            cur_step_type <- input[[inp_id_type]]
    
            if (is.null(cur_step_type)) {
                cur_step_type <- default_step_type
            }
            
            inp_type <- selectInput(nspace(inp_id_type), 'Type', available_types, cur_step_type)
            
            step_showhide_panel <- div(    # couldn't get conditionalPanel to work together with actionButton, so I'm using this...
                style = paste0('display:', ifelse(cur_step == i, 'block', 'none')),
                inp_type,
                step_conf_panels[[cur_step_type]](nspace, input, inp_prefix)
            )
            
            boxes <- list_append(boxes, step_showhide_panel)
        }
        
        boxes
    })
    
    output$design_code <- renderText({
        'TODO'
    })
    
    # output$design_code <- renderText({
    #     code_steps <- list()
    #     code_steps_objectnames <- c()
    #     
    #     for(i in 1:react$n_steps) {
    #         #print(i)
    #         inp_prefix <- sprintf('step%d_', i)
    #         
    #         inp_id_type <- paste0(inp_prefix, 'type')
    #         step_type <- input[[inp_id_type]]
    #         
    #         if (is.null(step_type)) {
    #             step_type <- default_step_type
    #         }
    #         
    #         inp_id_template <- paste0(inp_prefix, 'template')
    #         step_template <- input[[inp_id_template]]
    #         
    #         if (is.null(step_template)) {
    #             step_template <- default_step_template
    #         }
    #         
    #         step_fn <- STEP_TYPES[[step_type]][[step_template]]
    #         step_fn_args <- names(formals(step_fn))
    #         step_fn_args <- setdiff(step_fn_args, "data")
    #         
    #         step_inputs <- sapply(step_fn_args, function(arg_name) expr(!!as.numeric(input[[paste0(inp_prefix, 'arg_', arg_name)]])))
    #         #print(step_inputs)
    #         
    #         if (length(step_fn_args) != 0) {
    #             body <- capture.output(do.call(step_fn, exprs(!!!(step_inputs))))
    #             
    #         } else {
    #             body <- capture.output(rlang::expr(!!step_fn))
    #         }
    #         
    #         step_name <- generate_unique_name(step_type, code_steps_objectnames)
    #         code_steps[[i]] <- paste0(c(paste0(step_name, " <- "), rep("", length(body)-1)), body)
    #         code_steps_objectnames <- c(code_steps_objectnames, step_name)
    #         #print(code_steps[[i]])
    #         #print('---')
    #     }
    #     
    #     code_steps[[react$n_steps+1]] <- paste0(paste0(input$design_name, " <- "), paste0(code_steps_objectnames, collapse = " + "))
    #     paste0(unlist(code_steps), collapse = "\n")
    # })
}

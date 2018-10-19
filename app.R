library(DesignLibrary)
library(shiny)
library(shinymaterial)
library(shinythemes)
library(shinyBS)
library(shinyjs)


source('conf.R')
source('common.R')


# Define UI for application that draws a histogram
ui <- material_page(
    title = app_title,
    nav_bar_color = nav_bar_color,
    shiny::tags$title(app_title),
    bootstrapLib(),
    withMathJax(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    shinyjs::useShinyjs(),
    material_tabs(
        tabs = c(
            "Design" = "tab_design",
            "Inspect" = "tab_inspect"
        )
    ),
    material_tab_content(
        tab_id = 'tab_design',
        material_row(
            material_column(
                width = 3,
                material_card("Input",
                              div(style="text-align: center;",
                                  actionButton("import_from_design_lib", label = "Import")
                              )
                ),
                uiOutput("design_parameters")    # display *all* arguments of an imported design
            ),
            material_column(
                width = 9,
                material_card("Download",
                              downloadButton('download_r_script', label = 'R code', disabled = 'disabled'),
                              downloadButton('download_rds_obj', label = 'Design as RDS file', disabled = 'disabled')),
                bsCollapse(id='sections_container',
                           bsCollapsePanel('Messages', verbatimTextOutput("section_messages")),
#                           bsCollapsePanel('Warnings', verbatimTextOutput("section_warnings")),
                           bsCollapsePanel('Summary', verbatimTextOutput("section_summary")),
                           bsCollapsePanel('Code output', verbatimTextOutput("section_design_code"))
                )
            )
        )
    ),
    material_tab_content(
        tab_id = 'tab_inspect',
        material_row(
            material_column(
                width = 3,
                uiOutput("compare_design_parameters")    # display not-fixed parameters of a design / allow to define sequences for comparison in plots
            ),
            material_column(
                width = 6,
                material_card("Diagnostic plots",
                              plotOutput('plot_output')
                )
            ),
            material_column(
                width = 3,
                material_card("Plot configuration",
                              uiOutput("plot_conf")
                )
            )
        )
    )
)


server <- function(input, output) {
    options(warn = 1)    # always directly print warnings
    load_design <- 'simple_two_arm_designer'     # TODO: so far, design cannot be chosen from lib
    
    ### reactive values  ###
    
    react <- reactiveValues(
        design = NULL,                  # parametric design / designer object (a closure)
        design_id = NULL,               # identifier for current design instance *after* being instantiated
        design_argdefinitions = NULL,   # argument definitions for current design instance
        captured_stdout = NULL,
        captured_msgs = NULL
#        captured_warnings = NULL
    )
    
    ### reactive expressions ###
    
    # arguments/parameters for react$design and their values taken from the inputs
    design_args <- reactive({
        output_args <- list()
        
        if (!is.null(react$design)) {
            args <- formals(react$design)
            arg_defs <- react$design_argdefinitions   # is NULL on first run
            
            fixed_args <- c('design_name')
            
            for (argname in names(args)) {
                if (argname %in% args_control_skip_design_args) next()
                
                argdefault <- args[[argname]]
                argdefinition <- as.list(arg_defs[arg_defs$names == argname,])
                inp_value <- input[[paste0('design_arg_', argname)]]
                #print(paste(argname, inp_value, class(argdefault), typeof(argdefault)))
                argvalue <- design_arg_value_from_input(inp_value, argdefault, argdefinition, class(argdefault), typeof(argdefault))
                
                if (!is.null(argvalue)) {
                    output_args[[argname]] <- argvalue
                }
                
                arg_is_fixed_value <- input[[paste0('design_arg_', argname, '_fixed')]]
                if (isTruthy(arg_is_fixed_value)) {
                    fixed_args <- c(fixed_args, argname)
                }
            }
            
            #output_args$design_name <- c(input$design_arg_design_name)  # super strange, doesn't work
            output_args$design_name <- load_design
            output_args$fixed <- fixed_args
            
            print('design args changed:')
            print(output_args)
        }
        
        output_args
    })
    
    # specific design instance generated from above react$design with specific parameter values `design_args()`
    design_instance <- reactive({
        d_inst <- NULL
        
        if (!is.null(react$design)) {
            d_args <- design_args()
            
            if (length(d_args) == 0) {
                print('using default arg values')
            }
            
            react$captured_msgs <- capture.output({
                react$captured_stdout <- capture.output({
                    e <- environment()
                    d_inst <- do.call(react$design, d_args, envir = parent.env(e))
                    print(d_inst)   # to create summary output
                }, type = 'output')
            }, type = 'message')
            
            react$design_id <- load_design
            react$design_argdefinitions <- attr(d_inst, 'definitions')
            
            print('design instance changed')
        }
        
        d_inst
    })
    
    ### input observers ###
    
    observeEvent(input$import_from_design_lib, {
        react$design <- getFromNamespace(load_design, 'DesignLibrary')
        react$design_id <- NULL    # set after being instantiated
        shinyjs::enable('download_r_script')
        shinyjs::enable('download_rds_obj')
        print('parametric design loaded')
    })
    
    
    ### DESIGN TAB: output elements ###
    
    output$design_parameters <- renderUI({
        create_design_parameter_ui('design', react, design_instance)
    })
    
    output$section_design_code <- renderText({
        if(!is.null(design_instance()) && !is.null(attr(design_instance(), 'code'))) {
            code_text <- paste(attr(design_instance(), 'code'), collapse = "\n")
        } else {
            code_text <- ''
        }
        
        code_text
    })
    
    output$section_summary <- renderText({
        if(!is.null(design_instance()) && !is.null(react$captured_stdout)) {   # call design_instance() will also create design
            txt <- paste(react$captured_stdout, collapse = "\n")
        } else {
            txt <- 'No summary.'
        }
        
        txt
    })
    
    output$section_messages <- renderText({
        if(!is.null(design_instance()) && !is.null(react$captured_msgs)) {   # call design_instance() will also create design
            txt <- paste(react$captured_msgs, collapse = "\n")
        } else {
            txt <- 'No messages.'
        }
        
        txt
    })

    output$download_r_script <- downloadHandler(
        filename = function() {
            design_name <- input$design_arg_design_name
            
            if (!isTruthy(design_name)) {
                design_name <- paste0("design-", Sys.Date())
            }
            
            paste0(design_name, '.R')
        },
        content = function(file) {
            if(!is.null(design_instance()) && !is.null(attr(design_instance(), 'code'))) {
                writeLines(attr(design_instance(), 'code'), file)
            }
        }
    )
    
    output$download_rds_obj <- downloadHandler(
        filename = function() {
            design_name <- input$design_arg_design_name
            
            if (!isTruthy(design_name)) {
                design_name <- paste0("design-", Sys.Date())
            }
            
            paste0(design_name, '.RDS')
        },
        content = function(file) {
            d <- design_instance()
            if(!is.null(d)) {
                saveRDS(d, file = file)
            }
        }
    )
    
    ### INSPECT TAB: output elements ###
    
    output$compare_design_parameters <- renderUI({
        tags$div(create_design_parameter_ui('inspect', react, design_instance,
                                            input = input,
                                            defaults = design_args()))
    })
    
    output$plot_output <- renderPlot({
        if (is.null(react$design) || is.null(react$design_argdefinitions)) {
            return(NULL)
        }
        
        d_args <- formals(react$design)
        d_argdefs <- react$design_argdefinitions
        
        insp_args <- list()
        
        for (d_argname in names(d_args)) {
            d_argdef <- as.list(d_argdefs[d_argdefs$names == d_argname,])
            inp_name <- paste0('inspect_arg_', d_argname)
            inp_value <- input[[inp_name]]
            d_argclass <- d_argdef$class
            
            if (isTruthy(inp_value)) {
                insp_args[[d_argname]] <- parse_sequence_string(inp_value, d_argclass)
            }
        }
        
        print('will run diagnoses with arguments:')
        print(insp_args)
        
        diag_results <- run_diagnoses(react$design, insp_args,
                                      sims = default_diag_sims,
                                      bootstrap_sims = defaul_diag_bootstrap_sims)
        
        print(diag_results)
    })
    
    output$plot_conf <- renderUI({
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

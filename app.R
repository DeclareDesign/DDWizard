library(DesignLibrary)
library(shiny)
library(shinymaterial)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(ggplot2)


source('conf.R')
source('common.R')


#######################################
# Frontend: User interface definition #
#######################################

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
                uiOutput("compare_design_parameters")    # display not-fixed parameters of a design / allow to define sequences
                                                         # for comparison in plots
            ),
            material_column(
                width = 6,
                material_card("Diagnostic plots",
                              actionButton('update_plot', 'Update plot'),
                              plotOutput('plot_output')
                )
            ),
            material_column(
                width = 3,
                uiOutput("plot_conf")
            )
        )
    )
)


###########################################################
# Backend: Input handling and output generation on server #
###########################################################


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
    
    get_diagnoses_for_plot <- eventReactive(input$update_plot, {
        if (is.null(react$design) || is.null(react$design_argdefinitions)) {
            return(NULL)
        }
        
        insp_args <- get_args_for_inspection(react$design, react$design_argdefinitions, input)
        
        print('will run diagnoses with arguments:')
        print(insp_args)
        
        diag_results <- run_diagnoses(react$design, insp_args,
                                      sims = default_diag_sims,
                                      bootstrap_sims = defaul_diag_bootstrap_sims)
        
        diag_results
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
        req(input$plot_conf_x_param)
        
        plotdf <- get_diagnoses_for_plot()$diagnosands_df
        
        
        plotdf$diagnosand_min <- plotdf[[input$plot_conf_diag_param]] - plotdf[[paste0("se(", input$plot_conf_diag_param, ")")]]
        plotdf$diagnosand_max <- plotdf[[input$plot_conf_diag_param]] + plotdf[[paste0("se(", input$plot_conf_diag_param, ")")]]

        aes_args <- list(
            'x' = input$plot_conf_x_param,
            'y' = input$plot_conf_diag_param,
            'ymin' = 'diagnosand_min',
            'ymax' = 'diagnosand_max'
        )
        
        if (isTruthy(input$plot_conf_color_param)) {
            plotdf$color_param <- as.factor(plotdf[[input$plot_conf_color_param]])
            aes_args$color <- 'color_param'
        }
        
        if (isTruthy(input$plot_conf_facets_param)) {
            plotdf$facets_param <- as.factor(plotdf[[input$plot_conf_facets_param]])
        }
        
        print(plotdf)
        
        aes_definition <- do.call(aes_string, aes_args)
        p <- ggplot(plotdf, aes_definition) +
            geom_line() +
            geom_pointrange() +
            scale_y_continuous(name = input$plot_conf_diag_param) +
            dd_theme() +
            labs(x = input$plot_conf_x_param, color = input$plot_conf_color_param)
        
        if (isTruthy(input$plot_conf_facets_param)) {
            p <- p + facet_wrap(input$plot_conf_facets_param, ncol = 2, labeller = label_both)
        }
        
        p
    })
    
    output$plot_conf <- renderUI({
        if (is.null(react$design) || is.null(react$design_argdefinitions)) {
            boxes <- list(p('Load a design first'))
        } else {
            inp_prefix <- 'plot_conf_'
            boxes <- list()
            
            d <- design_instance()
            d_estimates <- get_estimates(d)
            minimal_diag <- diagnose_design(d, sims = 1, bootstrap_sims = 1)
            
            # 1. estimator
            inp_estimator_id <- paste0(inp_prefix, "estimator")    
            inp_estimator <- selectInput(inp_estimator_id, "Estimator Label",
                                         choices = unique(d_estimates$estimator_label),
                                         selected = input[[inp_estimator_id]])
            boxes <- list_append(boxes, inp_estimator)
            
            # 2. coefficient
            if ("term" %in% names(d_estimates)) {
                coefficients <- d_estimates$term[d_estimates$estimator_label == input[[inp_estimator_id]]]
            } else {
                coefficients <- ""
            }
            
            inp_coeff_id <- paste0(inp_prefix, "coefficient")
            inp_coeff <- selectInput(inp_coeff_id, "Coefficient",
                                     choices = coefficients,
                                     selected = input[[inp_coeff_id]])
            boxes <- list_append(boxes, inp_coeff)
            
            # 3. diagnosand (y-axis)
            inp_diag_param_id <- paste0(inp_prefix, "diag_param")
            inp_diag_param <- selectInput(inp_diag_param_id, "Diagnosand (y-axis)",
                                          choices = minimal_diag$diagnosand_names,
                                          selected = input[[inp_diag_param_id]])
            boxes <- list_append(boxes, inp_diag_param)
            
            # 4. main inspection parameter (x-axis)
            insp_args <- get_args_for_inspection(react$design, react$design_argdefinitions, input)
            insp_args_lengths <- sapply(insp_args, length)
            variable_args <- names(insp_args_lengths[insp_args_lengths > 1])
            
            inp_x_param_id <- paste0(inp_prefix, "x_param")
            inp_x_param <- selectInput(inp_x_param_id, "Primary parameter (x-axis)",
                                       choices = variable_args,
                                       selected = input[[inp_x_param_id]])
            boxes <- list_append(boxes, inp_x_param)
            
            # 5. secondary inspection parameter (color)
            variable_args_optional <- c('', variable_args)
            inp_color_param_id <- paste0(inp_prefix, "color_param")
            inp_color_param <- selectInput(inp_color_param_id, "Secondary parameter (color)",
                                           choices = variable_args_optional,
                                           selected = input[[inp_color_param_id]])
            boxes <- list_append(boxes, inp_color_param)
            
            # 6. tertiary inspection parameter (small multiples)
            inp_facets_param_id <- paste0(inp_prefix, "facets_param")
            inp_facets_param <- selectInput(inp_facets_param_id, "Tertiary parameter (small multiples)",
                                            choices = variable_args_optional,
                                            selected = input[[inp_facets_param_id]])
            boxes <- list_append(boxes, inp_facets_param)
        }
        
        do.call(material_card, c(title="Plot configuration", boxes))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

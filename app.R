# Shiny application with UI and server-side code
#
# Markus Konrad <markus.konrad@wzb.eu>
# Oct. 2018
#

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
    # title
    title = app_title,
    nav_bar_color = nav_bar_color,
    shiny::tags$title(app_title),
    
    # additional JS / CSS libraries
    bootstrapLib(),
    withMathJax(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    shinyjs::useShinyjs(),
    
    # tabs
    material_tabs(
        tabs = c(
            "Design" = "tab_design",
            "Inspect" = "tab_inspect"
        )
    ),
    
    # "Design" tab
    material_tab_content(
        tab_id = 'tab_design',
        material_row(
            material_column(  # left: input and design parameters
                width = 3,
                material_card("Input",
                              div(style="text-align: center;",
                                  actionButton("import_from_design_lib", label = "Import")
                              )
                ),
                uiOutput("design_parameters")    # display *all* arguments of an imported design
            ),
            material_column(  # center: design output
                width = 9,
                material_card("Download",
                              downloadButton('download_r_script', label = 'R code', disabled = 'disabled'),
                              downloadButton('download_rds_obj', label = 'Design as RDS file', disabled = 'disabled')),
                bsCollapse(id='sections_container',
                           bsCollapsePanel('Messages', verbatimTextOutput("section_messages")),
                           bsCollapsePanel('Summary', verbatimTextOutput("section_summary")),
                           bsCollapsePanel('Code output', verbatimTextOutput("section_design_code")),
                           bsCollapsePanel('Simulated data',
                                           p("The following table shows a single draw of the data."),
                                           actionButton("simdata_redraw", label = "Redraw data", disabled = "disabled"),
                                           downloadButton("simdata_download", label = "Download data", disabled = "disabled"),
                                           dataTableOutput("section_simdata_table"))
                )
            )
        )
    ),

    # "Inspect" tab
    material_tab_content(
        tab_id = 'tab_inspect',
        material_row(
            material_column(   # left: design parameters for comparison
                width = 3,
                uiOutput("compare_design_parameters")    # display not-fixed parameters of a design / allow to define sequences
                                                         # for comparison in plots
            ),
            material_column(   # center: inspection output
                width = 6,
                bsCollapse(id='inspect_sections_simconf_container',
                           bsCollapsePanel('Configure simulations',
                                           checkboxInput('simconf_force_rerun', label = 'Always re-run simulations (disable cache)'))),  # add more simulation options here (issue #2)
                material_card("Diagnostic plots",
                              actionButton('update_plot', 'Update plot'),
                              plotOutput('plot_output'),
                              downloadButton("download_plot", label = "Download the plot", disabled = "disabled" )
                ),
                bsCollapse(id='inspect_sections_container',
                           bsCollapsePanel('Diagnosands',
                                           textOutput("section_diagnosands_message"),
                                           dataTableOutput("section_diagnosands_table"),
                                           downloadButton("section_diagnosands_download_subset",
                                                          label = "Download above table", disabled = "disabled"),
                                           downloadButton("section_diagnosands_download_full",
                                                          label = "Download full diagnosands table", disabled = "disabled"))
                )
            ),
            material_column(   # right: plot configuration
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
    load_design <- 'two_arm_designer'     # TODO: so far, design cannot be chosen from lib
    
    ### reactive values  ###
    
    react <- reactiveValues(
        design = NULL,                  # parametric designer object (a closure)
        design_id = NULL,               # identifier for current design instance *after* being instantiated
        design_argdefinitions = NULL,   # argument definitions for current design instance
        simdata = NULL,                 # a single draw of the data to be shown in the "simulated data" panel
        captured_stdout = NULL,         # captured output of print(design_instance). used in design summary
        captured_msgs = NULL,           # captured warnings and other messages during design creation
        diagnosands = NULL              # diagnosands for current plot in "inspect" tab
    )
    
    ### reactive expressions ###
    
    # arguments/parameters for react$design and their values taken from the inputs
    design_args <- reactive({
        output_args <- list()
        
        if (!is.null(react$design)) {   # return empty list if no designer given
            args <- formals(react$design)
            arg_defs <- react$design_argdefinitions   # is NULL on first run, otherwise data frame of argument definitions (class, min/max)
            
            fixed_args <- c('design_name')   # vector of fixed arguments. design_name is always fixed
            
            for (argname in names(args)) {
                if (argname %in% args_control_skip_design_args) next()
                
                argdefault <- args[[argname]]
                argdefinition <- as.list(arg_defs[arg_defs$names == argname,])
                inp_value <- input[[paste0('design_arg_', argname)]]
                
                # convert an input value to a argument value of correct class
                argvalue <- design_arg_value_from_input(inp_value, argdefault, argdefinition, class(argdefault), typeof(argdefault))
                
                if (!is.null(argvalue)) {  # add the value to the list of designer arguments
                    output_args[[argname]] <- argvalue
                }
                
                # determine whether argument was set to "fixed"
                arg_is_fixed_value <- input[[paste0('design_arg_', argname, '_fixed')]]
                if (isTruthy(arg_is_fixed_value)) {
                    fixed_args <- c(fixed_args, argname)
                }
            }
            
            #output_args$design_name <- c(input$design_arg_design_name)  # super strange, doesn't work
            
            # additional designer arguments: design name and vector of fixed arguments
            output_args$design_name <- load_design
            output_args$fixed <- fixed_args
            
            print('design args changed:')
            print(output_args)
        }
        
        output_args
    })
    
    # specific design instance generated from above react$design with specific parameter values `design_args()`
    design_instance <- reactive({
        d_inst <- NULL  # design instance
        
        if (!is.null(react$design)) {  # return NULL if no designer is given
            d_args <- design_args()  # designer arguments
            
            if (length(d_args) == 0) {
                print('using default arg values')
            }
            
            # create a design instance from the designer using the current arguments `d_args`
            # nested capture.output expressions:
            react$captured_msgs <- capture.output({  # capture any messages/warnings generated during design creation
                react$captured_stdout <- capture.output({  # capture output of `print(d_inst)`
                    #e <- environment()  # note: seems to work without
                    d_inst <- do.call(react$design, d_args) #, envir = parent.env(e))    # also, the documentation says parent.env() is evil
                    print(d_inst)   # to create summary output
                }, type = 'output')
            }, type = 'message')
            
            react$design_id <- load_design
            react$design_argdefinitions <- attr(react$design, 'definitions')  # get the designer's argument definitions
            
            print('design instance changed')
        }
        
        d_inst
    })
    
    # reactive function to run diagnoses in the "inspect" tab and return the results once "Update plot" is clicked
    get_diagnoses_for_plot <- eventReactive(input$update_plot, {
        req(react$design, react$design_argdefinitions)
        
        # get all arguments from the left side pane in the "Inspect" tab
        insp_args <- get_args_for_inspection(react$design, react$design_argdefinitions, input)
        
        # only if at least one argument is a sequence (i.e. its length is > 1) for comparison,
        # run the diagnoses and return a result
        if (max(sapply(insp_args, length)) > 1) {
            print('will run diagnoses with arguments:')
            print(insp_args)
            
            # run diagnoses and get results
            isolate({
                if (length(input$plot_conf_diag_param_param) == 0) {
                    diag_param_alpha <- 0.05
                } else {
                    diag_param_alpha <- input$plot_conf_diag_param_param
                }
                
                diag_results <- run_diagnoses(react$design, insp_args,
                                              sims = default_diag_sims,
                                              bootstrap_sims = default_diag_bootstrap_sims,
                                              diag_param_alpha = diag_param_alpha,
                                              use_cache = !input$simconf_force_rerun,
                                              advance_progressbar = 1/6)
            })
            
            return(diag_results)
        } else {
            return(NULL)
        }
    })
    
    # get subset data frame of diagnosands for display and download
    get_diagnosands_for_display <- reactive({
        req(react$diagnosands)
        
        # set columns to show
        cols <- c(input$plot_conf_x_param)
        
        if (isTruthy(input$plot_conf_color_param) && input$plot_conf_color_param != '(none)') {
            cols <- c(cols, input$plot_conf_color_param)
        }
        if (isTruthy(input$plot_conf_facets_param) && input$plot_conf_facets_param != '(none)') {
            cols <- c(cols, input$plot_conf_facets_param)
        }
        
        cols <- c(cols, 'estimator_label', 'term', input$plot_conf_diag_param, paste0('se(', input$plot_conf_diag_param, ')'))
        
        # return data frame subset
        react$diagnosands[cols]
    })
    
    
    ### input observers ###
    
    # input observer for click on design import
    observeEvent(input$import_from_design_lib, {
        # loads a pre-defined designer from the library
        react$design <- getFromNamespace(load_design, 'DesignLibrary')
        react$design_id <- NULL    # set after being instantiated
        shinyjs::enable('download_r_script')
        shinyjs::enable('download_rds_obj')
        shinyjs::enable('simdata_redraw')
        shinyjs::enable('simdata_download')
        print('parametric design loaded')
    })
    
    # input observer for click on "redraw data" button in "simulated data" section
    observeEvent(input$simdata_redraw, {
        d <- req(design_instance())
        simdata <- draw_data(d)
        simdata <- round_df(simdata, 4)
        react$simdata <- simdata
    })
    
    ### ----- DESIGN TAB: output elements ------ 
    
    # left side: designer parameters
    output$design_parameters <- renderUI({
        create_design_parameter_ui('design', react, design_instance)
    })
    
    # center: design code
    output$section_design_code <- renderText({
        if(!is.null(design_instance()) && !is.null(attr(design_instance(), 'code'))) {
            # use the "code" attribute of a design instance and convert it to a single string
            code_text <- paste(attr(design_instance(), 'code'), collapse = "\n")
        } else {
            code_text <- ''
        }
        
        code_text
    })
    
    # center: design summary
    output$section_summary <- renderText({
        if(!is.null(design_instance()) && !is.null(react$captured_stdout)) {   # call design_instance() will also create design
            # show caputured print() output
            txt <- paste(react$captured_stdout, collapse = "\n")
        } else {
            txt <- 'No summary.'
        }
        
        txt
    })
    
    # center: design messages
    output$section_messages <- renderText({
        if(!is.null(design_instance()) && !is.null(react$captured_msgs)) {   # call design_instance() will also create design
            # show caputured messages/warnings
            txt <- paste(react$captured_msgs, collapse = "\n")
        } else {
            txt <- 'No messages.'
        }
        
        txt
    })
    
    # center: download generated R code
    output$download_r_script <- downloadHandler(
        filename = function() {  # note that this seems to work only in a "real" browser, not in RStudio's browser
            design_name <- input$design_arg_design_name
            
            if (!isTruthy(design_name)) {
                design_name <- paste0("design-", Sys.Date())
            }
            
            paste0(design_name, '.R')
        },
        content = function(file) {
            if(!is.null(design_instance()) && !is.null(attr(design_instance(), 'code'))) {
                # use the "code" attribute of a design instance and write it to `file`
                writeLines(attr(design_instance(), 'code'), file)
            }
        }
    )
    
    # center: download design as RDS file
    output$download_rds_obj <- downloadHandler(
        filename = function() {  # note that this seems to work only in a "real" browser, not in RStudio's browser
            design_name <- input$design_arg_design_name
            
            if (!isTruthy(design_name)) {
                design_name <- paste0("design-", Sys.Date())
            }
            
            paste0(design_name, '.RDS')
        },
        content = function(file) {
            d <- design_instance()
            if(!is.null(d)) {   # save design instance
                saveRDS(d, file = file)
            }
        }
    )
    
    # center: simulated data table
    output$section_simdata_table <- renderDataTable({
        react$simdata
    }, options = list(searching = FALSE,
                      ordering = FALSE,
                      paging = TRUE,
                      pageLength = 10,
                      info = FALSE,
                      lengthChange = FALSE,
                      scrollX = TRUE))
    
    # center: download simulated data
    output$simdata_download <- downloadHandler(
        filename = function() {  # note that this seems to work only in a "real" browser, not in RStudio's browser
            design_name <- input$design_arg_design_name
            
            if (!isTruthy(design_name)) {
                design_name <- paste0("design-", Sys.Date())
            }
            
            paste0(design_name, '_simulated_data.csv')
        },
        content = function(file) {
            req(react$simdata)
            write.csv(react$simdata, file = file, row.names = FALSE)
        }
    )
    
    ### --- INSPECT TAB: output elements ---
    
    # left: design parameters to inspect
    output$compare_design_parameters <- renderUI({
        d_args <- design_args()
        isolate({
            # set defaults: use value from design args in design tab unless a sequence of values for arg comparison
            # was defined in inspect tab
            defaults <- sapply(names(d_args), function(argname) {
                ifelse(is.null(input[[paste0('inspect_arg_', argname)]]) || length(parse_sequence_string(input[[paste0('inspect_arg_', argname)]])) < 2,
                       d_args[[argname]],
                       input[[paste0('inspect_arg_', argname)]])
            }, simplify = FALSE)
        })
        
        tags$div(create_design_parameter_ui('inspect', react, design_instance,
                                            input = input,
                                            defaults = defaults))
    })
    
    # make the plot reactive
    plotinput <- reactive({
        n_steps = 6
        withProgress(message = 'Simulating data and generating plot...', value = 0, {
            incProgress(1/n_steps)
            diag_results <- get_diagnoses_for_plot()
            req(diag_results)
            
            plotdf <- diag_results$diagnosands_df
            plotdf <- plotdf[plotdf$estimator_label == input$plot_conf_estimator & plotdf$term == input$plot_conf_coefficient,]
            react$diagnosands <- plotdf
            
            # diagnosand values +/- SE
            # don't isolate this, because we can change the diagnosand on the fly (no reevaluation necessary)
            plotdf$diagnosand_min <- plotdf[[input$plot_conf_diag_param]] - plotdf[[paste0("se(", input$plot_conf_diag_param, ")")]]
            plotdf$diagnosand_max <- plotdf[[input$plot_conf_diag_param]] + plotdf[[paste0("se(", input$plot_conf_diag_param, ")")]]
            
            # base aesthetics for line plot
            isolate({  # isolate all other parameters used to configure the plot so that the "Update plot" button has to be clicked
                aes_args <- list(
                    'x' = input$plot_conf_x_param,   
                    'y' = input$plot_conf_diag_param,
                    'ymin' = 'diagnosand_min',
                    'ymax' = 'diagnosand_max'
                    
                )
                
                # if the "color" parameter is set, add it to the aeshetics definition
                if (isTruthy(input$plot_conf_color_param) && input$plot_conf_color_param != '(none)') {
                    plotdf$color_param <- factor(plotdf[[input$plot_conf_color_param]])
                    aes_args$color <- input$plot_conf_color_param
                    #aes_args$color <- 'color_param'
                    aes_args$group <- 'color_param'
                    plot_conf_color_param <- NULL
                } else {
                    plot_conf_color_param <- input$plot_conf_color_param
                }
                
                # if the "facets" parameter is set, add it to the aeshetics definition
                if (isTruthy(input$plot_conf_facets_param) && input$plot_conf_facets_param != '(none)') {
                    plotdf$facets_param <- as.factor(plotdf[[input$plot_conf_facets_param]])
                }
                
                #print(plotdf)
                
                # create aesthetics definition
                aes_definition <- do.call(aes_string, aes_args)
                
                incProgress(1/n_steps)
                
                # create base line plot
                p <- ggplot(plotdf, aes_definition) +
                    geom_line() +
                    geom_pointrange() +
                    scale_y_continuous(name = input$plot_conf_diag_param) +
                    dd_theme() +
                    labs(x = input$plot_conf_x_param
                         #, color = plot_conf_color_param
                         )
                
                # add facets if necessary
                if (isTruthy(input$plot_conf_facets_param) && input$plot_conf_facets_param != '(none)') {
                    p <- p + facet_wrap(input$plot_conf_facets_param, ncol = 2, labeller = label_both)
                }
                
                incProgress(1/n_steps)
                
                shinyjs::enable('section_diagnosands_download_subset')
                shinyjs::enable('section_diagnosands_download_full')
            
                print(p)
            })
        })
    })
   
    
    # -------------- center: plot output --------------
    
    output$plot_output <- renderPlot({
        print(plotinput())
        shinyjs::enable('download_plot')

    })
    
    # -------- download the plot --------
    output$download_plot <- downloadHandler(
        filename = function() {
            design_name <- input$design_arg_design_name
            
            if (!isTruthy(design_name)) {
                design_name <- paste0("design-", Sys.Date())
            }
            
            paste0(design_name, '_diagnostic_plot.png')
        },
        content = function(file) {
            png(file, width = 1200, height = 900)
            print(plotinput())
            dev.off()
        }
    )
    
    # center below plot: diagnosands table message
    output$section_diagnosands_message <- renderText({
        if (is.null(react$diagnosands)) {
            return('Not data yet. Set comparison parameters and generate a plot first.')
        }
    })
    
    # center below plot: diagnosands table
    output$section_diagnosands_table <- renderDataTable({
        get_diagnosands_for_display()
    }, options = list(searching = FALSE,
                      ordering = FALSE,
                      paging = TRUE,
                      pageLength = 10,
                      info = FALSE,
                      lengthChange = FALSE,
                      scrollX = TRUE)
    )
    
    # center below plot: download buttons
    output$section_diagnosands_download_subset <- downloadHandler(
        filename = function() {  # note that this seems to work only in a "real" browser, not in RStudio's browser
            design_name <- input$design_arg_design_name
            
            if (!isTruthy(design_name)) {
                design_name <- paste0("design-", Sys.Date())
            }
            
            paste0(design_name, '_diagnosands.csv')
        },
        content = function(file) {
            write.csv(get_diagnosands_for_display(), file = file, row.names = FALSE)
        }
    )
    
    output$section_diagnosands_download_full <- downloadHandler(
        filename = function() {  # note that this seems to work only in a "real" browser, not in RStudio's browser
            design_name <- input$design_arg_design_name
            
            if (!isTruthy(design_name)) {
                design_name <- paste0("design-", Sys.Date())
            }
            
            paste0(design_name, '_diagnosands_full.csv')
        },
        content = function(file) {
            write.csv(react$diagnosands, file = file, row.names = FALSE)
        }
    )   
    
    # right: inspection plot configuration
    output$plot_conf <- renderUI({
        if (is.null(react$design) || is.null(react$design_argdefinitions)) {
            boxes <- list(p('Load a design first'))
        } else {
            # create list of input elements, all with a common prefix
            inp_prefix <- 'plot_conf_'
            boxes <- list()
            
            # create the design instance and get its estimates
            d <- design_instance()
            d_estimates <- draw_estimates(d)
            
            # we need to find out the set of available diagnosands
            available_diagnosands <- DeclareDesign:::default_diagnosands(NULL)$diagnosand_label
            
            # old approach: run a minimal diagnosis
            # minimal_diag <- diagnose_design(d, sims = 1, bootstrap_sims = 1)
            # available_diagnosands <- minimal_diag$diagnosand_names
            
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
                                          choices = available_diagnosands,
                                          selected = input[[inp_diag_param_id]])
            boxes <- list_append(boxes, inp_diag_param)
            
            # 3b. optional: diagnosand parameter
            if (length(input[[inp_diag_param_id]]) > 0 && input[[inp_diag_param_id]] == 'power') {
                inp_diag_param_param_id <- paste0(inp_prefix, "diag_param_param")
                if (length(input[[inp_diag_param_param_id]]) > 0) {
                    inp_diag_param_param_default <- input[[inp_diag_param_param_id]]
                } else {
                    inp_diag_param_param_default <- 0.05
                }
                inp_diag_param_param <- numericInput(inp_diag_param_param_id, "Alpha for power",
                                                     min = 0, max = 1, step = 0.01,
                                                     value = inp_diag_param_param_default)
                boxes <- list_append(boxes, inp_diag_param_param)
            }
            
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
            variable_args_optional <- c('(none)', variable_args)
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

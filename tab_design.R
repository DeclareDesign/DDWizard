# UI and server module for "Design" tab.
#
# designTab function returns a list of objects that allow to access its reactive values and some functions (see end of file).
#
# Markus Konrad <markus.konrad@wzb.eu>
# Sisi Huang <sisi.huang@wzb.eu>
#
# Dec. 2018
#

### UI ###

designTabUI <- function(id, label = 'Design') {
    nspace <- NS(id)
    
    material_tab_content(
        tab_id = id,
        material_row(
            material_column(  # left: input and design parameters
                width = 3,
                material_card("Input",
                              div(style="text-align: center;",
                                  # add a selectbox to choose the design from DesignLibrary
                                  selectInput(nspace("choose_design_lib_id"), label = "Choose design name", choices = c("Factorial" = "factorial_designer",
                                                                                                                        "Multi Arm" = "multi_arm_designer",
                                                                                                                        "Binary IV" = "binary_iv_designer", 
                                                                                                                        "Block Cluster Two Arm" = "block_cluster_two_arm_designer",
                                                                                                                        "Cluster Sampling" = "cluster_sampling_designer", 
                                                                                                                        "Mediation Analysis" = "mediation_analysis_designer",
                                                                                                                        "Pretest Posttest" = "pretest_posttest_designer",
                                                                                                                        "Process Tracing" = "process_tracing_designer",
                                                                                                                        "Randomized Response" = "randomized_response_designer",
                                                                                                                        "Regression Discontinuity" = "regression_discontinuity_designer",
                                                                                                                        "Spillover" = "spillover_designer",
                                                                                                                        "Two Arm Attrition" = "two_arm_attrition_designer",
                                                                                                                        "Two Arm" = "two_arm_designer",
                                                                                                                        "Two By Two" = "two_by_two_designer"), 
                                              selected = "two_arm_designer", multiple = F),
                                  actionButton(nspace("import_from_design_lib"), label = "Import")
                              )
                ),
                uiOutput(nspace("design_parameters"))    # display *all* arguments of an imported design
            ),
            material_column(  # center: design output
                width = 9,
                material_card("Download",
                              downloadButton(nspace('download_r_script'), label = 'R code', disabled = 'disabled'),
                              downloadButton(nspace('download_rds_obj'), label = 'Design as RDS file', disabled = 'disabled')),
                bsCollapse(id=nspace('sections_container'),
                           bsCollapsePanel('Messages', verbatimTextOutput(nspace("section_messages"))),
                           bsCollapsePanel('Summary', verbatimTextOutput(nspace("section_summary"))),
                           bsCollapsePanel('Code output', verbatimTextOutput(nspace("section_design_code"))),
                           bsCollapsePanel('Simulated data',
                                           p("The following table shows a single draw of the data."),
                                           actionButton(nspace("simdata_redraw"), label = "Redraw data", disabled = "disabled"),
                                           downloadButton(nspace("simdata_download"), label = "Download data", disabled = "disabled"),
                                           dataTableOutput(nspace("section_simdata_table")))
                )
            )
        )
    )
}

### Server ###

designTab <- function(input, output, session) {
    options(warn = 1)    # always directly print warnings
    
    #load_design <- 'two_arm_designer'     # TODO: so far, design cannot be chosen from lib
    load_design <- reactive(input$choose_design_lib_id)
    print("!!!!!!!")
    print(isolate(load_design()))
    print("!!!!!")
   
    
    ### reactive values  ###
    
    react <- reactiveValues(
        design = NULL,                  # parametric designer object (a closure)
        design_id = NULL,               # identifier for current design instance *after* being instantiated
        design_argdefinitions = NULL,   # argument definitions for current design instance
        simdata = NULL,                 # a single draw of the data to be shown in the "simulated data" panel
        captured_stdout = NULL,         # captured output of print(design_instance). used in design summary
        captured_msgs = NULL            # captured warnings and other messages during design creation
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
            output_args$design_name <- load_design()
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
            
            react$design_id <- load_design()
            react$design_argdefinitions <- attr(react$design, 'definitions')  # get the designer's argument definitions
            
            print('design instance changed')
        }
        
        d_inst
    })
    
    ### input observers ###
    
    # input observer for click on design import
    observeEvent(input$import_from_design_lib, {
        # loads a pre-defined designer from the library
        print("------")
        print(isolate(load_design()))
        print("-------")
        react$design <- getFromNamespace(load_design(), 'DesignLibrary')
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
    
    ### output elements ###
    
    # left side: designer parameters
    output$design_parameters <- renderUI({
        create_design_parameter_ui('design', react, NS('tab_design'), design_instance)
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
    
    # return reactive values and some functions to be accessed from other modules
    return(list(
        react = react,
        design_args = design_args,
        design_instance = design_instance,
        input = input
    ))
}
    


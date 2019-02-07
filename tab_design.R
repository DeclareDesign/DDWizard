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
                material_card("Load design",
                              div(style="text-align: center;",
                                  # add a selectbox to choose the design from DesignLibrary
                                  uiOutput(nspace("import_design_lib_id")),
                                  actionButton(nspace("import_from_design_lib"), label = "Import")
                              )
                ),
                # show designer parameters if a design was loaded
                hidden(div(id = nspace('design_params_panel_wrapper'),
                    material_card("Set design parameters",
                        htmlOutput(nspace('design_description')),
                        textInput(nspace('design_arg_design_name'), 'Design name'),
                        conditionalPanel(paste0("output['", nspace('design_supports_fixed_arg'), "'] != ''"),
                            div(style="text-align: right;", uiOutput(nspace('fix_toggle_btn')))
                        ),
                        uiOutput(nspace("design_parameters"))    # display *all* arguments of an imported design
                    )
                ))
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
    
    ### reactive values  ###
    
    react <- reactiveValues(
        design = NULL,                  # parametric designer object (a closure)
        design_id = NULL,               # identifier for current design instance *after* being instantiated
        design_argdefinitions = NULL,   # argument definitions for current design instance
        design_name_once_changed = FALSE,  # records whether design name was changed after import
        fix_toggle = 'fix',             # toggle for fixing/unfixing all design parameters. must be either "fix" or "unfix"
        simdata = NULL,                 # a single draw of the data to be shown in the "simulated data" panel
        captured_stdout = NULL,         # captured output of print(design_instance). used in design summary
        captured_msgs = NULL            # captured warnings and other messages during design creation
    )
    
    ### reactive expressions ###
    
    # arguments/parameters for react$design and their values taken from the inputs
    design_args <- reactive({
        output_args <- list()
        
        if (!is.null(react$design)) {   # return empty list if no designer given
            args <- get_designer_args(react$design)
            arg_defs <- react$design_argdefinitions   # is NULL on first run, otherwise data frame of argument definitions (class, min/max)
            
            if (is.null(arg_defs)) {
                return(output_args)    # empty list
            }
            
            fixed_args <- c('design_name')   # vector of fixed arguments. design_name is always fixed
            
            all_default <- TRUE
            for (argname in names(args)) {
                if (argname %in% args_control_skip_design_args) next()
                
                argdefault <- args[[argname]]
                argdefinition <- as.list(arg_defs[arg_defs$names == argname,])
                inp_value <- input[[paste0('design_arg_', argname)]]
                
                # convert an input value to a argument value of correct class
                if (length(argdefinition) != 0){
                argvalue <- design_arg_value_from_input(inp_value, argdefault, argdefinition, class(argdefault), typeof(argdefault))
                
                if (!is.null(argvalue) && argvalue != '' && argvalue != argdefault) {
                    all_default <- FALSE
                }
                
                if (!is.null(argvalue)) {  # add the value to the list of designer arguments
                    output_args[[argname]] <- argvalue
                }}
                
                # determine whether argument was set to "fixed"
                arg_is_fixed_value <- input[[paste0('design_arg_', argname, '_fixed')]]
                if (isTruthy(arg_is_fixed_value)) {
                    fixed_args <- c(fixed_args, argname)
                    
                    # if at least one arg. is set to fixed, the toggle button is set to "Unfix all"
                    react$fix_toggle <- 'unfix'
                    updateActionButton(session, 'fix_toggle', label = 'Unfix all')
                }
            }
            
            # additional designer arguments: design name and vector of fixed arguments
            if (is.null(react$design_argdefinitions)) {
                output_args$design_name <- react$design_id   # should always be a valid R object name
                updateTextInput(session, 'design_arg_design_name', value = output_args$design_name)
            } else if (!is.null(input$design_arg_design_name) && (!all_default || react$design_name_once_changed)) {
                output_args$design_name <- make_valid_r_object_name(input$design_arg_design_name)
                # updateTextInput(session, 'design_arg_design_name', value = output_args$design_name)
                react$design_name_once_changed <- TRUE
            }
            
            if (design_supports_fixed_arg()) {
                output_args$fixed <- fixed_args
            }
            
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
            
            print('design instance changed')
        }
        
        d_inst
    })
    
    # return TRUE if designer supports "fixed" argument, else FALSE
    design_supports_fixed_arg <- reactive({
        req(react$design)
        #return(FALSE)  # for testing
        'fixed' %in% names(formals(react$design))
    })
    
    # return a character vector that lists the arguments set as "fixed"
    get_fixed_design_args <- reactive({
        req(react$design)
        
        if (!design_supports_fixed_arg()) {
            return(character())   # empty char vector
        }
        
        args <- get_designer_args(react$design)
        
        args_fixed <- sapply(names(args), function(argname) {
            inp_elem_name_fixed <- paste0('design_arg_', argname, '_fixed')
            if (!is.null(input[[inp_elem_name_fixed]])) {
                input[[inp_elem_name_fixed]]
            } else {
                NA
            }
        })
        
        args_fixed <- args_fixed[!is.na(args_fixed)]
        names(args_fixed)[args_fixed]
    })
    
    # returns TRUE if at least one designer argument was set to "fixed", otherwise FALSE
    at_least_one_design_arg_fixed <- reactive({
        length(get_fixed_design_args()) > 0
    })
    
    # returns TRUE if all design arguments were set to fixed, otherwise FALSE
    all_design_args_fixed <- reactive({
        all_args <- get_designer_args(react$design)
        args_fixed <- get_fixed_design_args()
        length(args_fixed) == length(all_args)
    })
    
    
    ### input observers ###
    
    # input observer for click on design import
    observeEvent(input$import_from_design_lib, {
        # loads a pre-defined designer from the library
        print(paste('loading designer', input$import_design_library))
        
        shinyjs::show('design_params_panel_wrapper')
        
        react$design <- getFromNamespace(input$import_design_library, 'DesignLibrary')
        react$design_id <- input$import_design_library
        react$design_argdefinitions <- attr(react$design, 'definitions')  # get the designer's argument definitions
        react$design_name_once_changed <- FALSE
        react$fix_toggle <- 'fix'
        
        shinyjs::enable('download_r_script')
        shinyjs::enable('download_rds_obj')
        shinyjs::enable('simdata_redraw')
        shinyjs::enable('simdata_download')
        
        # replace xx_designer as xx_design
        updateTextInput(session, 'design_arg_design_name', value = gsub("designer","design",react$design_id))
    })
    
    # input observer for click on "Fix/Unfix all" button
    observeEvent(input$fix_toggle_click, {
        args <- get_designer_args(react$design)

        checkbox_val <- react$fix_toggle == 'fix'

        for (argname in names(args)) {
            inp_elem_name_fixed <- paste0('design_arg_', argname, '_fixed')
            if (!is.null(input[[inp_elem_name_fixed]])) {
                updateCheckboxInput(session, inp_elem_name_fixed, value = checkbox_val)
            }
        }
    })
    
    # input observer for click on "redraw data" button in "simulated data" section
    observeEvent(input$simdata_redraw, {
        d <- req(design_instance())
        simdata <- draw_data(d)
        simdata <- round_df(simdata, 4)
        react$simdata <- simdata
    })
    
    ### output elements ###
    
    # hidden (for conditional panel)
    output$design_supports_fixed_arg <- design_supports_fixed_arg
    outputOptions(output, 'design_supports_fixed_arg', suspendWhenHidden = FALSE)

    # left side: designer description
    output$design_description <- renderUI({
        req(react$design)
        HTML(attr(react$design, 'description'))
    })
    
    # left side: designer parameters
    output$design_parameters <- renderUI({
        req(react$design)
        
        nspace <- NS('tab_design')
        
        create_design_parameter_ui(type = 'design', react = react, nspace =  nspace, 
                                   input = NULL, defaults = NULL,
                                   create_fixed_checkboxes = design_supports_fixed_arg())
    })
    
    # left side: "Fix/Unfix all" button
    output$fix_toggle_btn <- renderUI({
        nspace <- NS('tab_design')
        
        if (at_least_one_design_arg_fixed()) {
            fix_toggle_label <- 'Unfix all'
            react$fix_toggle <- 'unfix'
        } else {
            fix_toggle_label <- 'Fix all'
            react$fix_toggle <- 'fix'
        }
        
        actionButton(nspace('fix_toggle_click'), fix_toggle_label)
    })
    
    # left side: choose designers 
    output$import_design_lib_id <- renderUI({
        nspace <-  NS('tab_design')
        
        cached <- str_replace(grep("designer$", ls(as.environment("package:DesignLibrary")), value = TRUE), "_designer", "")
        option <- c()
        for (i in 1:length(cached)){
            if (is.null(attr(getFromNamespace(paste(cached[i], sep = "_", "designer"), 'DesignLibrary'), "shiny"))){
                next()
            }else{
                option[i] <- paste(cached[i], sep = "_", "designer")
            }
        }
        test <- sub("_", " ",sub("_", " ", sub("_", " ", sub("_designer", "", option[!is.na(option)]))))
        options_data <- data.frame(names = option[!is.na(option)],abbr = stri_trans_totitle(test), stringsAsFactors = FALSE)
        option_list <- as.list(options_data$names)
        names(option_list) <- options_data$abbr

        selectInput(nspace("import_design_library"), label = "Choose design name",
                    selected = "two_arm_designer", choices = option_list,
                    multiple = FALSE)
    })
    
    # center: design code
    output$section_design_code <- renderText({
        d <- design_instance()
        if(!is.null(d) && !is.null(attr(d, 'code'))) {
            # use the "code" attribute of a design instance and convert it to a single string
            code_text <- paste(attr(d, 'code'), collapse = "\n")
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
            d <- design_instance()
            if(!is.null(d) && !is.null(attr(d, 'code'))) {
                # use the "code" attribute of a design instance and write it to `file`
                writeLines(attr(d, 'code'), file)
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
        input = input,
        all_design_args_fixed = all_design_args_fixed,
        get_fixed_design_args = get_fixed_design_args
    ))
}
    


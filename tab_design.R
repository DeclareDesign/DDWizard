# UI and server module for "Design" tab.
#
# designTab function returns a list of objects that allow to access its reactive values and some functions (see end of file).
#
# Markus Konrad <markus.konrad@wzb.eu>
# Clara Bicalho <clara.bicalho@wzb.eu>
# Sisi Huang <sisi.huang@wzb.eu>
#
# Dec. 2018
#

# -------------- UI --------------

designTabUI <- function(id, label = 'Design') {
    
    nspace <- NS(id)
    
    material_tab_content(
        tab_id = id,
        material_row(
            material_column(  # left: input and design parameters
                width = 3,
                material_card("Choose design",
                              # tagList("Read about the library ", a("here", href="https://declaredesign.org/library/")),
                              div(style="text-align: center;",
                                  # add a selectbox to choose the design from DesignLibrary
                                  uiOutput(nspace("import_design_lib_id")),
                                  actionButton(nspace("import_from_design_lib"), 
                                               label = "Load", 
                                               disabled = "disabled"),
                                  actionButton(inputId='learn_more', label= NULL,
                                               icon = icon("question-circle"),
                                               style = "text-align: center; padding-left: 8px; padding-right: 8px;",
                                               onclick = "window.open('https://declaredesign.org/r/designlibrary/', '_blank')")
                              )
                ),
                # show designer parameters if a design was loaded
                hidden(div(id = nspace('design_params_panel_wrapper'),
                    material_card("Set design parameters",
                        uiOutput(nspace('design_vignette')),
                        br(),
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
                uiOutput(nspace("load_design_info")),
                material_card("Download",
                              downloadButton(nspace('download_r_script'), label = 'R code', disabled = 'disabled'),
                              downloadButton(nspace('download_rds_obj'), label = 'Design as RDS file', disabled = 'disabled')),
                bsCollapse(id=nspace('sections_container'), multiple = TRUE,
                           bsCollapsePanel('Warnings or errors', uiOutput(nspace("section_messages"))),
                           bsCollapsePanel('Summary', uiOutput(nspace("section_summary"))),
                           bsCollapsePanel('Code output', uiOutput(nspace('section_design_code'))),
                           bsCollapsePanel('Simulated data',
                                           p("The following table shows a single draw of the data."),
                                           actionButton(nspace("simdata_redraw"), label = "Redraw data", disabled = "disabled"),
                                           downloadButton(nspace("simdata_download"), label = "Download data", disabled = "disabled"),
                                           dataTableOutput(nspace("section_simdata_table"))),
                           bsCollapsePanel("About DeclareDesign Wizard",
                                           p("  This project is generously supported by a grant from the Laura and John Arnold Foundation and seed funding from Evidence in Governance and Politics (EGAP)."),
                                           tagList("  This software is in beta release. We welcome your feedback! Please report any issues ", a("here.", href="https://github.com/DeclareDesign/DDWizard/issues")))
                )
            )
        )
    )
}

# -------------- server --------------

designTab <- function(input, output, session) {
    options(warn = 1)    # always directly print warnings
    
    # -------------- reactive values definition --------------
    
    react <- reactiveValues(
        design = NULL,                  # parametric designer object (a closure)
        design_id = NULL,               # identifier for current design instance *after* being instantiated
        design_code = '',
        design_argdefinitions = NULL,   # argument definitions for current design instance
        design_name_once_changed = FALSE,  # records whether design name was changed after import
        fix_toggle = 'fix',             # toggle for fixing/unfixing all design parameters. must be either "fix" or "unfix"
        simdata = NULL,                 # a single draw of the data to be shown in the "simulated data" panel
        captured_stdout = NULL,         # captured output of print(design_instance). used in design summary
        captured_errors = NULL,         # captured errors and warnings during design creation
        error_occurred  = NULL,         # detect whether error occurred in design_instance
        warning_occurred  = NULL,       # detect whether warning occurred in design_instance 
        input_errors = NULL,            # errors related to invalid inputs
        captured_msgs = NULL,           # captured messages during design creation
        custom_state = list()           # additional state values for bookmarking
    )
    
    # -------------- helper functions --------------
    
    # Load the designer with the name `designer` (char string).
    # For mysterious reasons, it is necessary to pass a namespace function `nspace` (created with `NS(<id>)`)
    # *whenever this function is called for restoring a bookmark.*
    load_designer <- function(designer, nspace = function(x) { x }) {
        print(paste('loading designer', designer))
        
        shinyjs::show(nspace('design_params_panel_wrapper'))
        
        react$design_id <- designer
        react$design <- getFromNamespace(react$design_id, 'DesignLibrary')
        react$design_argdefinitions <- attr(react$design, 'definitions')  # get the designer's argument definitions
        react$design_name_once_changed <- FALSE
        react$fix_toggle <- 'fix'
        
        shinyjs::enable(nspace('download_r_script'))
        shinyjs::enable(nspace('download_rds_obj'))
        shinyjs::enable(nspace('simdata_redraw'))
        shinyjs::enable(nspace('simdata_download'))
        
        # replace xx_designer as xx_design
        updateTextInput(session, nspace('design_arg_design_name'), value = gsub("designer","design", react$design_id))
        
        # simulation data would react once new design is loaded
        isolate({
            d <- req(design_instance())
            if (!is.null(react$custom_state$simdata)) {
                simdata <- react$custom_state$simdata
                react$custom_state$simdata <- NULL
            } else {
                simdata <- draw_data(d)
            }
        })
        
        react$simdata <- simdata
        
        # save this to state because it is not automatically restored from bookmark
        react$custom_state$designer <- react$design_id
    }
    
    # -------------- reactive functions --------------
    
    # arguments/parameters for react$design and their values taken from the inputs
    design_args <- reactive({
        output_args <- list()
        
        if (!is.null(react$design)) {   # return empty list if no designer given
            switching <- isolate({ input$switching_designer })
            if (is.null(switching)) {   # happens on bookmark restore
                switching <- TRUE
            }
            
            args <- get_designer_args(react$design)
            args_eval <- evaluate_designer_args(args, attr(react$design, 'definitions'))
            arg_defs <- react$design_argdefinitions   # is NULL on first run, otherwise data frame of argument definitions (class, min/max)
            
            if (is.null(arg_defs)) {
                return(output_args)    # empty list
            }
        
            fixed_args <- NULL
            
            all_default <- TRUE
            
            for (argname in names(args)) {
                skip_specifc_args <- args_control_skip_specific_designer_args[[react$design_id]]
                if (argname %in% args_control_skip_design_args || (!is.null(skip_specifc_args) && argname %in% skip_specifc_args))
                    next()

                argdefault <- args_eval[[argname]]
                argdefinition <- as.list(arg_defs[arg_defs$names == argname,])
                inp_value <- input[[paste0('design_arg_', argname)]]
                
                if (switching) {  # if the designer was just changed, use its default values
                                  # because the inputs still hold values from the prev. designer which might be incompatible
                    output_args[[argname]] <- design_arg_value_from_input(argdefault, argdefault, argdefinition, class(argdefault), typeof(argdefault))
                } else {  # otherwise use the inputs as usual
                    # convert an input value to a argument value of correct class
                    if (length(argdefinition) != 0) {
                        argvalue <- design_arg_value_from_input(inp_value, argdefault, argdefinition, class(argdefault), typeof(argdefault))
                        has_NAs <- !is.null(argvalue) && any(is.na(argvalue))   # may contain NAs where invalid input was supplied
                        if (!has_NAs && ((!is.null(argvalue) && is.null(argdefault))
                            || (!is.null(argvalue) && argvalue != ''
                                && (length(argvalue) != length(argdefault) || argvalue != argdefault))))
                        {
                            all_default <- FALSE
                        }
                        
                        if (has_NAs || !is.null(argvalue)) {  # add the value to the list of designer arguments
                            output_args[[argname]] <- argvalue
                        }
                    }
                }
                
                # determine whether argument was set in "args_to_fix"
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
                # output_args$design_name <- react$design_id   # should always be a valid R object name
                updateTextInput(session, 'design_arg_design_name', value = react$design_id)
            } else if (!is.null(input$design_arg_design_name) && (!all_default || react$design_name_once_changed)) {
                # output_args$design_name <- make_valid_r_object_name(input$design_arg_design_name)
                # updateTextInput(session, 'design_arg_design_name', value = output_args$design_name)
                react$design_name_once_changed <- TRUE
            }
            
            if (design_supports_fixed_arg()) {
                output_args$args_to_fix <- fixed_args
            }
        }
        
        output_args
    })
    
    
    # specific design instance generated from above react$design with specific parameter values `design_args()`
    design_instance <- reactive({
        d_inst <- NULL  # design instance
        react$input_errors <- NULL
        react$captured_errors <- NULL
        react$design_code <- ''
        
        if (!is.null(react$design)) {  # return NULL if no designer is given
            msgs <- character()
            error_occur <- FALSE
            warning_occur <- FALSE
            d_args <- design_args()  # designer arguments
            
            print('creating design instance with arguments:')
            print(d_args)
            
            d_args_NAs <- sapply(d_args, function(arg) { any(is.na(arg)) })
            if (sum(d_args_NAs) > 0) {
                react$input_errors <- paste('Invalid values supplied to the following arguments:',
                                            paste(names(d_args_NAs)[d_args_NAs], collapse = ', '))
                react$captured_errors <- 'Please correct the errors in the argument values first.'
            } else {
                conditions <- tryCatch(
                    expr = {
                        capture.output({
                            d_inst <- do.call(react$design, d_args)
                            # capture message prints, e.g in block_cluster_two_arm_designer
                            msgs <-  paste0(capture.output(d_inst <- do.call(react$design, d_args)), collapse = "\n")
                            print(d_inst)
                        }, type = 'message')
                    }, error = function(e){
                        error_occur <<- TRUE
                        msgs <<- e
                    },warning = function(w){
                        warning_occur <<- TRUE 
                        msgs <<- w
                    })
                
                # create a design instance from the designer using the current arguments `d_args`
                if (!error_occur) {
                    if (is.null(d_inst)) { # try again if we only got warnings (not errors)
                        d_inst <- do.call(react$design, d_args)
                    }
                    
                    react$captured_stdout <- capture.output({  # capture output of `print(d_inst)`
                        print(d_inst)   # to create summary output if and only if there is no error in d_inst
                    }, type = 'output')
                } else {
                    react$captured_stdout <- ''
                }
                
                react$captured_errors <- conditions
                react$captured_msgs <- msgs
                react$error_occurred <- error_occur
                react$warning_occurred <- warning_occur
                
                if (!error_occur && !is.null(d_inst)){
                    # also update simulated data
                    react$simdata <- draw_data(d_inst)
                    
                    # update design code
                    code_text <- paste(attr(d_inst, 'code'), collapse = "\n")
                    default_designer_name <- gsub("designer","design", react$design_id)
                    react$design_code <- gsub(default_designer_name,
                                              make_valid_r_object_name(input$design_arg_design_name),
                                              code_text)
                    
                }else{
                    react$simdata <- NULL
                }
                
            }
            
            print('design instance changed')
        }
        
        d_inst
    })
    
    # return TRUE if designer supports "args_to_fix" argument, else FALSE
    design_supports_fixed_arg <- reactive({
        req(react$design)
        #return(FALSE)  # for testing
        'args_to_fix' %in% names(formals(react$design))
    })
    
    # return a character vector that lists the arguments set in "args_to_fix"
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
    
    # returns TRUE if at least one designer argument was set in "args_to_fix", otherwise FALSE
    at_least_one_design_arg_fixed <- reactive({
        length(get_fixed_design_args()) > 0
    })
    
    # returns TRUE if all design arguments were set to fixed, otherwise FALSE
    all_design_args_fixed <- reactive({
        all_args <- get_designer_args(react$design)
        args_fixed <- get_fixed_design_args()
        length(args_fixed) == length(all_args)
    })
    
    # observer for the error message to unfold the message panel
    # reactive expression returns true when there is no error or warning
    message_close <- reactive({
        req(react$warning_occurred)
        req(react$error_occurred)
        if (!isTRUE(react$warning_occurred) && !isTRUE(react$error_occurred)) {
            return(TRUE)
        } else {
            return(NULL)
        }
    })
    
    # reactive expression returns true when there is any error or warning
    message_open <- reactive({
        if (is.null(design_instance()) || isTRUE(react$warning_occurred) || isTRUE(react$error_occurred)){
            return(TRUE)
        } else {
            return(NULL)
        }  
    })
    
    # -------------- event observers --------------
    
    # input observer for click on design import
    observeEvent(input$import_from_design_lib, {
        # loads a pre-defined designer from the library
        if (!is.null(input$import_design_library)) {
            load_designer(input$import_design_library)
        }
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
        isolate({
            d <- req(design_instance())
            if (!is.null(react$custom_state$simdata)) {
                simdata <- react$custom_state$simdata
                react$custom_state$simdata <- NULL
            } else {
                if (react$warning_occurred || react$error_occurred) {
                    simdata <- NULL
                } else {
                    simdata <- draw_data(d)
                }
            }
        })
        
        react$simdata <- simdata
    })
    
    # unfold the message panel
    observeEvent(message_open(),ignoreInit = TRUE,{
        updateCollapse(session, "sections_container", open = 'Warnings or errors')
    })
    
    # fold back the message panel
    observeEvent(message_close(),ignoreInit = TRUE,{
        updateCollapse(session, "sections_container", close = 'Warnings or errors')
    })
    
    # -------------- output elements: hidden --------------
    
    # hidden (for conditional panel)
    output$design_supports_fixed_arg <- design_supports_fixed_arg
    outputOptions(output, 'design_supports_fixed_arg', suspendWhenHidden = FALSE)
    
    # -------------- output elements: left side --------------

    # left side: designer description
    output$design_description <- renderUI({
        req(react$design)
        HTML(attr(react$design, 'description'))
    })
    
    # create link to vignette based on design input from the library
    output$design_vignette <- renderUI({
        url <- paste0("window.open('https://declaredesign.org/r/designlibrary/articles/", gsub("_designer","",react$design_id), ".html', '_blank')")
        actionButton(inputId='vignette', label=" Read more", 
                     icon = icon("book"), 
                     onclick = url)
    })
    
    # left side: designer parameters
    output$design_parameters <- renderUI({
        req(react$design)
        
        nspace <- NS('tab_design')
        
        isolate({
            defaults <- design_args()
            param_boxes <- create_design_parameter_ui(type = 'design', react = react, nspace =  nspace, 
                                                      input = input, defaults = defaults,
                                                      create_fixed_checkboxes = design_supports_fixed_arg(),
                                                      use_only_argdefaults = input$switching_designer)
        })

        if (!is.null(react$input_errors) && length(react$input_errors) > 0) {
            list(tags$div(class = 'error_msgs', paste(react$input_errors, collapse = "\n")), tags$div(param_boxes))
        } else {
            list(tags$div(param_boxes))
        }
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
        
        test <- gsub("_", " ",gsub("_designer","", option[!is.na(option)]))
        options_data <- data.frame(names = option[!is.na(option)],abbr = stri_trans_totitle(test), stringsAsFactors = FALSE)
        if (any(options_data$names == 'binary_iv_designer')) options_data[options_data$names == 'binary_iv_designer',]$abbr = "Binary IV"
        option_list <- as.list(options_data$names)
        names(option_list) <- options_data$abbr
        
        shinyjs::enable("import_from_design_lib")
        selectInput(nspace("import_design_library"), 
                    label = "",
                    selected = "two_arm_designer", choices = option_list,
                    multiple = FALSE)
        
       
        
    })
    
    # -------------- output elements: center --------------
    
    # center: info about the name of loaded design
    output$load_design_info <- renderUI({
        req(react$design_id)
        title = str_cap(react$design_id)
        if (title == "Binary iv designer") title <- "Binary IV designer"
        material_card(title = title, HTML(attr(react$design, 'description')))
    })
    
    # center: design code
    output$section_design_code <- renderUI({
        req(design_instance())
        tags$pre(react$design_code)
    })
    
    # center: design summary
    output$section_summary <- renderUI({
        if(!is.null(react$captured_stdout)) {
            # show captured print() output
            txt <- paste(react$captured_stdout, collapse = "\n")
        } else {
            txt <- 'No summary.'
        }
        
        tags$pre(txt)
    })
    
    # center: design messages
    output$section_messages <- renderUI({
        got_errors <- !is.null(react$captured_errors) && length(react$captured_errors) > 0
        
        if(!is.null(react$captured_msgs) && react$captured_msgs != '') {
            # show captured messages
            txt <- paste(react$captured_msgs, collapse = "\n")
            
            if (got_errors) {
                txt <- tags$div(class = 'error_msgs', txt)
            }
        } else {
            txt <- 'No warnings/errors.'
        }

        tags$pre(txt)
    })
    
    # center: simulated data table
    output$section_simdata_table <- renderDataTable({
        req(react$simdata)
        round_df(react$simdata, 4)
    }, options = list(searching = FALSE,
                      ordering = FALSE,
                      paging = TRUE,
                      pageLength = 10,
                      info = FALSE,
                      lengthChange = FALSE,
                      scrollX = TRUE))

    

    # -------------- download handlers --------------
    
    # download design as R script
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
                code_lines <- c(paste('# code generated with DDWizard and DesignLibrary on', Sys.time()),
                                '# see https://declaredesign.org/',
                                '',
                                'library(DeclareDesign)',
                                '',
                                attr(d, 'code'))
                writeLines(code_lines, file)
            }
        }
    )
    
    # download design as RDS file
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
    
    # download simulated data
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
    
    
    # -------------- bookmarking --------------
    
    # customize bookmarking process: add additional data to bookmarked state
    onBookmark(function(state) {
        print('BOOKMARKING IN DESIGN TAB:')
        
        # add open panels, because they're not restored automatically
        react$custom_state$panels_state <- input$sections_container
        
        # store simulated data
        react$custom_state$simdata <- react$simdata
        
        print(react$custom_state)
        state$values$custom_state <- react$custom_state
    })
    
    # customize restoring process
    onRestore(function(state) {
        print('RESTORING IN DESIGN TAB:')
        react$custom_state <- state$values$custom_state
        
        print(react$custom_state)
        
        # design is not loaded automatically on restore (probably because list of available designers
        # is not loaded yet) so do it here
        # also, a namespace function must be passed when doing a restore (reason unknown)
        load_designer(react$custom_state$designer, NS('tab_design'))
        
        # re-open the panels
        updateCollapse(session, 'sections_container', open = react$custom_state$panels_state)
    })
    
    # -------------- return values of this module --------------
    
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
    


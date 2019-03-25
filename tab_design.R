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
                material_card("Choose design",
                              div(style="text-align: center;",
                                  # add a selectbox to choose the design from DesignLibrary
                                  uiOutput(nspace("import_design_lib_id")),
                                  actionButton(nspace("import_from_design_lib"), 
                                               label = "Load", 
                                               disabled = "disabled")
                              )
                ),
                # show designer parameters if a design was loaded
                hidden(div(id = nspace('design_params_panel_wrapper'),
                    material_card("Set design parameters",
                        htmlOutput(nspace('design_description')),
                        br(),
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
                material_card("Download",
                              downloadButton(nspace('download_r_script'), label = 'R code', disabled = 'disabled'),
                              downloadButton(nspace('download_rds_obj'), label = 'Design as RDS file', disabled = 'disabled')),
                bsCollapse(id=nspace('sections_container'),
                           bsCollapsePanel('Messages', uiOutput(nspace("section_messages"))),
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

### Server ###

designTab <- function(input, output, session) {
    options(warn = 1)    # always directly print warnings
    
    welcome_alert()
    
    ### reactive values  ###
    
    react <- reactiveValues(
        design = NULL,                  # parametric designer object (a closure)
        design_id = NULL,               # identifier for current design instance *after* being instantiated
        design_argdefinitions = NULL,   # argument definitions for current design instance
        design_name_once_changed = FALSE,  # records whether design name was changed after import
        fix_toggle = 'fix',             # toggle for fixing/unfixing all design parameters. must be either "fix" or "unfix"
        simdata = NULL,                 # a single draw of the data to be shown in the "simulated data" panel
        captured_stdout = NULL,         # captured output of print(design_instance). used in design summary
        captured_errors = NULL,         # captured errors and warnings during design creation
        input_errors = NULL,            # errors related to invalid inputs
        captured_msgs = NULL,           # captured messages during design creation
        custom_state = list()           # additional state values for bookmarking
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
            
            fixed_args <- NULL
            
            all_default <- TRUE
            for (argname in names(args)) {
                if (argname %in% args_control_skip_design_args) next()
                
                argdefault <- args[[argname]]
                argdefinition <- as.list(arg_defs[arg_defs$names == argname,])
                inp_value <- input[[paste0('design_arg_', argname)]]
                
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
            
            print('design args changed:')
            print(output_args)
        }
        
        output_args
    })
    
    # specific design instance generated from above react$design with specific parameter values `design_args()`
    design_instance <- reactive({
        d_inst <- NULL  # design instance
        react$input_errors <- NULL
        react$captured_errors <- NULL
        
        if (!is.null(react$design)) {  # return NULL if no designer is given
            msgs <- character()
            
            d_args <- design_args()  # designer arguments
            
            d_args_NAs <- sapply(d_args, function(arg) { any(is.na(arg)) })
            if (sum(d_args_NAs) > 0) {
                react$input_errors <- paste('Invalid values supplied to the following arguments:',
                                            paste(names(d_args_NAs)[d_args_NAs], collapse = ', '))
                react$captured_errors <- 'Please correct the errors in the argument values first.'
            } else {
                conditions <- tryCatch({
                    msgs <- capture.output({
                        d_inst <- do.call(react$design, d_args)
                    }, type = 'message')
                }, error = function(cond) {
                    cond$message
                })
                
                # create a design instance from the designer using the current arguments `d_args`
                react$captured_stdout <- capture.output({  # capture output of `print(d_inst)`
                    print(d_inst)   # to create summary output
                }, type = 'output')
                
                react$captured_errors <- conditions
                react$captured_msgs <- msgs
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
    
    ### non-reactive functions ###
    
    wrap_errors <- function(output) {
        if (!is.null(react$captured_errors) && length(react$captured_errors) > 0) {
            list(tags$div(class = 'error_msgs', paste(react$captured_errors, collapse = "\n")))
        } else {
            list(output)
        }
    }
    
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
        
        # save this to state because it is not automatically restored from bookmark
        react$custom_state$designer <- react$design_id
        
        # replace xx_designer as xx_design
        updateTextInput(session, nspace('design_arg_design_name'), value = gsub("designer","design", react$design_id))
    }
    
    ### bookmarking ###
    
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
    
    ### input observers ###
    
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
        d <- req(design_instance())
        if (!is.null(react$custom_state$simdata)) {
            simdata <- react$custom_state$simdata
            react$custom_state$simdata <- NULL
        } else {
            simdata <- draw_data(d)
        }
        
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
    
    # create link to vignette based on design input from the library
    output$design_vignette <- renderUI({
        url <- paste0("window.open('https://declaredesign.org/library/articles/", gsub("_designer","",react$design_id), ".html', '_blank')")
        actionButton(inputId='vignette', label=" Read more", 
                     icon = icon("book"), 
                     onclick = url)
    })
    
    # left side: designer parameters
    output$design_parameters <- renderUI({
        req(react$design)
        
        nspace <- NS('tab_design')
        
        defaults <- isolate({ design_args() })
        
        param_boxes <- create_design_parameter_ui(type = 'design', react = react, nspace =  nspace, 
                                                  input = input, defaults = defaults,
                                                  create_fixed_checkboxes = design_supports_fixed_arg())
        
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
        option_list <- as.list(options_data$names)
        names(option_list) <- options_data$abbr
        
        shinyjs::enable("import_from_design_lib")
        selectInput(nspace("import_design_library"), 
                    label = "",
                    selected = "two_arm_designer", choices = option_list,
                    multiple = FALSE)
        
       
        
    })
    
    # center: design code
    output$section_design_code <- renderUI({
        d <- design_instance()
        if(!is.null(d) && !is.null(attr(d, 'code'))) {
            # use the "code" attribute of a design instance and convert it to a single string
            code_text <- paste(attr(d, 'code'), collapse = "\n")
            default_designer_name <- gsub("designer","design",react$design_id)
            code_text <- gsub(default_designer_name, make_valid_r_object_name(input$design_arg_design_name), code_text)
        } else {
            code_text <- ''
        }
        
        wrap_errors(tags$pre(renderText(code_text)))
    })
    
    # center: design summary
    output$section_summary <- renderUI({
        if(!is.null(design_instance()) && !is.null(react$captured_stdout)) {   # call design_instance() will also create design
            # show captured print() output
            txt <- paste(react$captured_stdout, collapse = "\n")
        } else {
            txt <- 'No summary.'
        }
        
        wrap_errors(tags$pre(renderText(txt)))
    })
    
    # center: design messages
    output$section_messages <- renderUI({
        if(!is.null(design_instance()) && !is.null(react$captured_msgs) && length(react$captured_msgs) > 0) {   # call design_instance() will also create design
            # show captured messages
            txt <- paste(react$captured_msgs, collapse = "\n")
        } else {
            txt <- 'No messages.'
        }
        
        wrap_errors(renderText(txt))
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
        round_df(react$simdata, 4)
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
    


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
    shinyjs::useShinyjs(),
    material_row(
        material_column(
            width = 3,
            material_card("Input",
                          div(style="text-align: center;",
                              actionButton("import_from_design_lib",
                                           label = HTML("Import")
                              ))
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
#                       bsCollapsePanel('Warnings', verbatimTextOutput("section_warnings")),
                       bsCollapsePanel('Summary', verbatimTextOutput("section_summary")),
                       bsCollapsePanel('Code output', verbatimTextOutput("section_design_code"))
            )
        )
    )
)


server <- function(input, output) {
    options(warn = 1)    # always directly print warnings
    load_design <- 'simple_two_arm_designer'     # TODO: so far, design cannot be chosen from lib
    
    ### reactive values  ###
    
    react <- reactiveValues(
        design = NULL,            # parametric design / designer object (a closure)
        design_id = NULL,         # identifier for current design instance *after* being instantiated
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
            for (argname in names(args)) {
                if (argname %in% args_control_skip_design_args) next()
                
                argdefault <- args[[argname]]
                inp_value <- input[[paste0('design_arg_', argname)]]
                print(paste(argname, inp_value, class(argdefault), typeof(argdefault)))
                output_args[[argname]] <- design_arg_value_from_input(inp_value, argdefault, class(argdefault), typeof(argdefault))
            }
            
            print('design args changed')
        }
        
        output_args
    })
    
    # specific design instance generated from above react$design with specific parameter values `design_args()`
    design_instance <- reactive({
        d_inst <- NULL
        
        if (!is.null(react$design) && length(design_args()) > 0) {
            
            react$captured_msgs <- capture.output({
                react$captured_stdout <- capture.output({
                    e <- environment()
                    message('fake message')
                    d_inst <- do.call(react$design, design_args(), envir = parent.env(e))
                    warning('fake warning')
                    message('fake message 2')
                    warning('fake warning 2')
                    print(d_inst)   # to create summary output
                }, type = 'output')
            }, type = 'message')
            
            # somehow, this doesn't (always) work (we capture warnings together with messages instead)
            # react$captured_warnings <- NULL
            # if (!is.null(last.warning) && length(last.warning) > 0) {
            #     warning_msgs <- names(last.warning)
            #     warning_loc <- as.character(unlist(last.warning, use.names = FALSE))
            #     warning_msgs <- warning_msgs[!is.na(warning_loc)]
            #     warning_loc <- warning_loc[!is.na(warning_loc)]
            #     if (length(warning_msgs) > 0) {
            #         react$captured_warnings <- paste(warning_msgs, 'at', warning_loc)
            #     }
            # }
            
            react$design_id <- load_design   # TODO: use args$design_name here, should be a character string
            
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
    
    
    ### output elements ###
    
    output$design_parameters <- renderUI({
        boxes <- list()
        
        if (is.null(react$design)) {
            boxes <- list_append(boxes, p('Load a design first'))
        } else {
            boxes <- list_append(boxes, HTML(attr(react$design, 'description')))
            
            args <- formals(react$design)
            
            boxes <- list_append(boxes, textInput('design_arg_design_name', 'Design name', value = react$design_id))
            
            for (argname in names(args)) {
                if (argname %in% args_control_skip_design_args) next()
                argdefault <- args[[argname]]
                
                boxes <- list_append(boxes, input_elem_for_design_arg(argname, argdefault, class(argdefault), typeof(argdefault)))
            }
        }
        
        return(do.call(material_card, c('Design parameters', boxes)))
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
    
    # output$section_warnings <- renderText({
    #     if(!is.null(design_instance()) && !is.null(react$captured_warnings)) {   # call design_instance() will also create design
    #         txt <- paste(react$captured_warnings, collapse = "\n")
    #     } else {
    #         txt <- 'No warnings.'
    #     }
    #     
    #     txt
    # })
    
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
}

# Run the application 
shinyApp(ui = ui, server = server)

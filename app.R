library(DesignLibrary)
library(shiny)
library(shinymaterial)
library(shinythemes)
library(shinyBS)


source('conf.R')
source('common.R')


# Define UI for application that draws a histogram
ui <- material_page(
    title = app_title,
    nav_bar_color = nav_bar_color,
    shiny::tags$title(app_title),
    bootstrapLib(),
    withMathJax(),
    material_row(
        material_column(
            width = 3,
            material_card("Input",
                          div(style="text-align: center;",
                              actionButton("import_from_design_lib",
                                           label = HTML("Import")
                              ))
            ),
            uiOutput("designParameters")    # display *all* arguments of an imported design
        ),
        material_column(
            width = 6,
            material_card("Output", verbatimTextOutput("designCode"), p("Download..."))
        ),
        material_column(
            width = 3,
            material_card("Inspect", p("Plots..."))
        )
    )
)


server <- function(input, output) {
    ### reactive values  ###
    
    react <- reactiveValues(
        design = NULL            # parametric design / designer object (a closure)
    )
    
    ### reactive expressions ###
    
    # arguments/parameters for react$design and their values taken from the inputs
    design_args <- reactive({
        output_args <- list()
        
        if (!is.null(react$design)) {
            args <- formals(react$design)
            for (argname in names(args)) {
                argdefault <- args[[argname]]
                argtype <- class(argdefault)
                
                if (argtype %in% c('numeric', 'integer')) {
                    inp_value <- as.numeric(input[[paste0('design_arg_', argname)]])
                    output_args[[argname]] <- ifelse(length(inp_value) > 0, inp_value, argdefault)
                }
            }

            print('design args changed')
        }

        output_args
    })
    
    # specific design instance generated from above react$design with specific parameter values `design_args()`
    design_instance <- reactive({
        d_inst <- NULL

        if (!is.null(react$design) && length(design_args()) > 0) {
            e <- environment()
            d_inst <- do.call(react$design, design_args(), envir = parent.env(e))

            print('design instance changed')
        }

        d_inst
    })
    
    ### input observers ###
    
    observeEvent(input$import_from_design_lib, {
        react$design <- DesignLibrary::simple_two_arm_designer
        print('parametric design loaded')
    })
    
    
    ### output elements ###
    
    output$designParameters <- renderUI({
        boxes <- list()
        
        if (is.null(react$design)) {
            boxes <- list_append(boxes, p('Load a design first'))
        } else {
            boxes <- list_append(boxes, HTML(attr(react$design, 'description')))
            args <- formals(react$design)
            
            for (argname in names(args)) {
                argdefault <- args[[argname]]
                argtype <- class(argdefault)
                
                boxes <- list_append(boxes, input_elem_for_design_arg(argname, argdefault, argtype))
            }
        }
        
        return(do.call(material_card, c('Design parameters', boxes)))
    })
    
    output$designCode <- renderText({
        if(!is.null(design_instance()) && !is.null(attr(design_instance(), 'code'))){
            code_text <- paste(attr(design_instance(), 'code'), collapse = "\n")
        } else {
            code_text <- ''
        }
        
        code_text
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



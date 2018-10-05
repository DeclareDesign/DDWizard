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
            material_card("Output", p("Code..."), p("Download..."))
        ),
        material_column(
            width = 3,
            material_card("Inspect", p("Plots..."))
        )
    )
)


server <- function(input, output) {
    # reactive values for server
    react <- reactiveValues(design = NULL)        # design object (a closure)
    
    designLoaded <- function(d) {
        react$design <- d
    }
    
    observeEvent(input$import_from_design_lib, {
        d <- DesignLibrary::simple_two_arm_designer
        designLoaded(d)
    })
    
    output$designParameters <- renderUI({
        boxes <- list()
        
        if (is.null(react$design)) {
            boxes <- list_append(boxes, p('Load a design first'))
        } else {
            boxes <- list_append(boxes, HTML(attr(react$design, 'description')))
            design_args <- formals(react$design)
            
            for (argname in names(design_args)) {
                argdefault <- design_args[[argname]]
                argtype <- class(argdefault)
                
                boxes <- list_append(boxes, input_elem_for_design_arg(argname, argdefault, argtype))
            }
        }
        
        return(do.call(material_card, c('Design parameters', boxes)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



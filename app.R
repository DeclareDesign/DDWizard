library(DesignLibrary)
library(shiny)
library(shinymaterial)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(ggplot2)


source('conf.R')
source('common.R')
source('tab_design.R')
source('tab_inspect.R')


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
    designTabUI('tab_design'),
    
    # "Inspect" tab
    inspectTabUI('tab_inspect')
)

server <- function(input, output) {
    design_tab_proxy <- callModule(designTab, 'tab_design')
    callModule(inspectTab, 'tab_inspect', design_tab_proxy)
}

# Run the application 
shinyApp(ui = ui, server = server)

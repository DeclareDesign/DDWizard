# DeclareDesign Wizard shiny app.
#
# This shiny app displays several tabs, each of which is a separate shiny module (http://shiny.rstudio.com/articles/modules.html)
# implemented in the respective "tab_ ... .R" file. Hence this file only implements the "skeleton" of the application.
#
# Markus Konrad <markus.konrad@wzb.eu>
# Sisi Huang <sisi.huang@wzb.eu>
#
# Dec. 2018
#

library(DesignLibrary)
library(shiny)
library(shinymaterial)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(stringr)
library(stringi)
library(dplyr)

source('conf.R')
source('common.R')
source('uihelpers.R')
source('tab_design.R')
source('tab_inspect.R')


#######################################
# Frontend: User interface definition #
#######################################

piwik_code_file <- 'www/piwik.txt'
if (file.exists(piwik_code_file)) {
    piwik_code <- readChar(piwik_code_file, file.info(piwik_code_file)$size)
    print('using PiWik code')
} else {
    piwik_code <- ''
}

ui <- material_page(
    # title
    title = app_title,
    nav_bar_color = nav_bar_color,
    shiny::tags$title(app_title),
    
    # additional JS / CSS libraries
    bootstrapLib(),
    withMathJax(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        HTML(piwik_code)
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


###########################################################
# Backend: Input handling and output generation on server #
###########################################################


server <- function(input, output) {
    design_tab_proxy <- callModule(designTab, 'tab_design')
    callModule(inspectTab, 'tab_inspect', design_tab_proxy)
}

# Run the application 
shinyApp(ui = ui, server = server)

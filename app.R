# DeclareDesign Wizard shiny app.
#
# This shiny app displays several tabs, each of which is a separate shiny module (http://shiny.rstudio.com/articles/modules.html)
# implemented in the respective "tab_ ... .R" file. Hence this file only implements the "skeleton" of the application.
#
# Clara Bicalho <clara.bicalho@wzb.eu>
# Markus Konrad <markus.konrad@wzb.eu>
# Sisi Huang <sisi.huang@wzb.eu>
#
# Dec. 2018
#

library(DesignLibrary)
library(shiny)
library(shinymaterial)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(stringr)
library(stringi)
library(dplyr)
library(MASS)

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

ui <- function(request) {
    material_page(
        # title
        nav_bar_fixed = TRUE,
        nav_bar_color = "white",
        title = tags$img(src="brand.png", height = 52.5, width = 300),
        
        
        # additional JS / CSS libraries
        bootstrapLib(),
        withMathJax(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            HTML(piwik_code),
            includeScript('www/custom.js')
        ),
        shinyjs::useShinyjs(),
        
        
        bookmarkButton("SHARE", title = "Share the status of your design and diagnoses"),
        
        # tabs
        material_tabs(
            tabs = c(
                "Design" = "tab_design",
                "Diagnose" = "tab_inspect"
            ),
            color = "blue"
            
        ),
        
        # "Design" tab
        useShinyalert(),
        designTabUI('tab_design'),
        
        # "Inspect" tab
        inspectTabUI('tab_inspect'),
        
        #Footer
        tags$footer(
            actionLink("show_help_text", "Help"),
            span(' | '),
            actionLink('show_legal_notice', 'Legal notice'),
            span(' | '),
            actionLink('show_data_protection_policy', 'Data protection policy'),
         
            align = "center", style = "
              bottom:0;
              width:100%;
              color: black;
              padding: 10px;
              background-color: #F5F5F5;
              z-index: 1000;"
        )
    )
}

###########################################################
# Backend: Input handling and output generation on server #
###########################################################

server <- function(input, output, session) {
    insp_changed_args <- character()
    
    design_tab_proxy <- callModule(designTab, 'tab_design')
    inspect_tab_proxy <- callModule(inspectTab, 'tab_inspect', design_tab_proxy)
    
    ### observers global events ###
    
    # legal notice button clicked
    observeEvent(input$show_legal_notice, {
        alert_with_content_from_html_file('Legal notice', 'www/legal_notice.html', className = 'wide')
    })
    
    # data protection button clicked
    observeEvent(input$show_data_protection_policy, {
        alert_with_content_from_html_file('Data protection policy', 'www/data_protection_policy.html', className = 'wide')
    })
    
    # observe changes between tabs
    observe({
        if (!is.null(input$current_tab)) {  # initially, currentTab is NULL; it only gets a value after the first tab change
            if (input$current_tab == 'tab_inspect') {          # check if change to "inspect" tab occurred
                # when switching from design to inspect tab, pass the recorded arguments from below (i.e. those that the
                # user changed previously in the inspect tab). we will not override these arguments' values with values
                # from the design tab.
                inspect_tab_proxy$set_changed_args(insp_changed_args)
            } else if (input$current_tab == 'tab_design') {    # check if change to "design" tab occurred
                # when switching from inspect to design tab, record which arguments the user changed in the inspect tab,
                # i.e. which of the values in the inspect tab differ from those in the design tab.
                # when switching back to the inspect tab, we will not override these with values from the design tab.
                isolate({
                    insp_changed_args <<- inspect_tab_proxy$get_changed_args()
                })
            }
        }
    })
    
    ### handling of bookmarking via "SHARE" button on top right ###
    
    onBookmark(function(state) {
        print('BOOKMARKING IN APP:')
        state$values$current_tab <- input$current_tab
        print(state$values$current_tab)
    })
    
    onBookmarked(function(url) {
        
        shinyalert(
            sprintf('<p>Share and restore the status of your design and diagnoses by copying the link below into your browser:</i></p>
                    <pre class="share-url"><div class="shiny-text-output">%s</div></pre>', url),
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = TRUE,
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            timer = 0,
            imageUrl = "",
            confirmButtonCol = "light-blue darken-3", 
            animation = TRUE
            )
    })
    
    onRestore(function(state) {
        # open the bookmarked tab
        shinymaterial::select_material_tab(session, state$values$current_tab)
    })
    # once the app is loaded, intro page will come out 
    alert_with_content_from_html_file('Welcome to DDWizard', 'www/get_started.html', 'Get started')
    observeEvent(input$show_help_text,{
        alert_with_content_from_html_file('Welcome to DDWizard', 'www/get_started.html', 'Get started')
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = 'server')
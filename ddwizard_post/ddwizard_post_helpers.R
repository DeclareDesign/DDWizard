
imageGallery <- function(dir, step_descriptions, height = 650){
  
  n_images <- length(list.files(dir))
  
  ui <-  fluidPage(
    shinyjs::useShinyjs(),
    hr(),
    fluidRow(
      column(2, align = "right", actionButton('previous', '<<', disabled = 'disabled')),
      column(8, align = "center", htmlOutput('description')),
      column(2, align = "left", actionButton("following", ">>"))
    ),
    br(),
    fluidRow(column(12, align = "center", imageOutput("image")))
  )
  
  server <- function(input, output, session) {
    
    counter <- reactiveValues(i = 1)
    
    output$count <- renderText(counter$i)
    output$description <- renderText(step_descriptions[counter$i])
    
    observeEvent(input$previous, {
      counter$i <- counter$i - 1
    })
    
    observeEvent(input$following, {
      counter$i <- counter$i + 1
    })
    
    observeEvent(counter$i, {
      if(counter$i == 1 && counter$i < n_images){
        shinyjs::disable('previous')
        shinyjs::enable('following')
      } else {
        if(counter$i == n_images)
          shinyjs::disable('following')
        if(counter$i > 1) shinyjs::enable('previous')
      }
      
    })
    
    output$image <- renderImage({
      return(list(
        src = paste0(dir, "/", counter$i, ".png"),
        filetype = "image/png",
        # alt = "This is image 1",
        width = "80%" 
      ))
      
    }, deleteFile = FALSE)
    
  }
  
  shiny::shinyApp(ui = ui, server = server, options = list(height = height))
}

add_title <- function(title, text){
  paste0("<dt> ", title, " </dt> ", text)
}

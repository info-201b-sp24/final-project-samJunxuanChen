server <- function(input, output) {
  
  # test for now 
  output$histPlot <- renderPlot({
    data(mtcars)
    bins <- seq(min(mtcars[[input$selectVariable]]), max(mtcars[[input$selectVariable]]), length.out = input$slider1 + 1)
    hist(mtcars[[input$selectVariable]], breaks = bins, col = 'darkgray', border = 'white')
  })
  
  observeEvent(input$btn, {
    output$text <- renderText({
      paste("You pressed the button at", Sys.time())
    })
  })
}
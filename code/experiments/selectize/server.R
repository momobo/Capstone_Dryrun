library(shiny)

shinyServer(function(input, output) {
  output$ex_out <- renderPrint({
    str(sapply(sprintf('e%d', 0:7), function(id) {
      input[[id]]
    }, simplify = FALSE))
  })

})


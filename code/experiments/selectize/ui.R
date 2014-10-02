library(shiny)

shinyUI(fluidPage(
  title = 'Selectize examples',
  sidebarLayout(sidebarPanel(
      textInput('inText', label='in text')
      
  ),
  mainPanel(
    helpText('here main panel')
    
  )
)))
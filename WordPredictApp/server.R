library(shiny)
library(datasets)

source("prediction.R")

# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {

prediction <- eventReactive(input$goButton,{
  predictWord(input$phrase)})

output$phrase <- renderTable({
    prediction()
  })
  
  
})
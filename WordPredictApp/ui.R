library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Word Prediction App"),
  
  # Sidebar with controls to provide a caption, select a dataset,
  # and specify the number of observations to view. Note that
  # changes made to the caption in the textInput control are
  # updated in the output area immediately as you type
  sidebarLayout(
    sidebarPanel(
      textInput("phrase", "Text Input:", ""),
    
    actionButton("goButton", "Go!"),
    p("Click the button to update the value displayed in the main panel.")
  ),
    # Show the caption, a summary of the dataset and an HTML 
    # table with the requested number of observations
    mainPanel(
      h6(tableOutput("phrase"))
    )
)))
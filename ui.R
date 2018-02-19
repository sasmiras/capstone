#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict Next Word"),
  
  
  fluidRow(
    br(),HTML("<strong>Enter a Phrase. Press \"Next Word\" button to predict the next word</strong>") ),
  fluidRow( p("\n"),br() ),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("inputString", "Enter a Phrase here",value = ""),
      submitButton("Next Word")),
      
    
    mainPanel(
      h4("Predicted Next Word"),
      verbatimTextOutput("prediction",placeholder = TRUE)
   
    )
  )
))

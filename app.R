library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(shiny)
source("graph_test.R")

ui <- fluidPage(
  titlePanel("Drill Down Data Story"),
  sidebarLayout(
    sidebarPanel(selectInput(inputId = "state_select",
                           label = 'Select a state:',
                           choices = states)
                 ),
    mainPanel(plotOutput(outputId = 'chart')
    )
  )
)

server <- function(input, output) {
  
  output$chart <- renderPlot({
    state_df <- filter(tester_df, Recip_State == input$state_select)
    p <- ggplot(state_df, aes(x = Recip_State,  y = WA_PCT)) +
      geom_bar(stat = 'identity')
    return(p)
  })
}


shinyApp(ui = ui, server = server)

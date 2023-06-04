library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(shiny)

# UI Stuff goes here
data <- read.csv("refined_data_set.csv")
x_axis <- "WA_PCT"
y_axis <- "Series_Complete_Pop_Pct"

ethnic_groups <- c("White", "Black or African American", "American Indian or Alaskan Native", "Asian", "Native Hawaiian and Other Pacific Islander", "Hispanic")
ethnic_col <- c("WA_PCT", "BA_PCT", "IA_PCT", "AA_PCT", "NA_PCT", "H_PCT")

age_groups <- c("Child", "Adult", "Elder", "All Age Groups")
age_col <- c("Series_Complete_Child_Pct", "Series_Complete_Adult_Pct", "Series_Complete_Elder_Pct", "Series_Complete_Pop_Pct")


ui <- fluidPage(
  titlePanel("Contrast Data Story"),
  sidebarLayout(
    
    sidebarPanel(
      selectizeInput(
        inputId = "state_name",
        label = "Which state would you like to look at?",
        choices = sort(data$STNAME)
      ),
      selectizeInput(
        inputId = "ethnic_group",
        label = "Which ethnic group would you like displayed?",
        choices = ethnic_groups
      ),
      radioButtons(
        inputId = "age_group",
        label = "Which age group would you like displayed?",
        choices = age_groups
      )
    ),
    mainPanel(plotOutput(outputId = 'chart'))
  )
)

# Server stuff goes here
server <- function(input, output) {
  output$chart <- renderPlot({
    x_axis <- ethnic_col[which(ethnic_groups == input$ethnic_group)]
    y_axis <- age_col[which(age_groups == input$age_group)]
    state_df <- filter(data, Date == "07/01/2021", STNAME == input$state_name)
    p <- ggplot(state_df, aes_string(x = x_axis,  y = y_axis)) +
      geom_point(stat = 'identity') +
      geom_smooth(method="lm", formula=y~x) +
      labs(x = paste(input$ethnic_group, "Population Percentage"), y = paste(input$age_group, "Vaccination Percentage"))
    return(p)
  })
}

# Make the app
shinyApp(ui, server)

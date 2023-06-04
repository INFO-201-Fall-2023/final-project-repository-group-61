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

#################################################################
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(shinyWidgets)
library(reshape2)
library(purrr)

summary_df <- read_csv("summary.csv")
df <- read.csv("refined_data_set.csv")
states <- state.abb

ui <- fluidPage(
  titlePanel("Study on County Vaccination Rates"),
  p(paste("COVID-19 is an infectious disease caused by the SARS-CoV-2",
          "virus. In 2020, COVID-19 spread across the world at an",
          "extremely rapid pace, and was declared as a pandemic by the",
          "WHO on March 11, 2020. On December 15th, 2020, the COVID-19",
          "vaccine was made publicly available in the United States. After",
          "its release, many activists highlighted how the COVID-19 pandemic",
          "disproportionately affected people of color in many different",
          "areas, one being in relation to vaccination rates.",
          "While finding a correlation between a racial group and its",
          "average vaccination rate in a state doesn’t prove a lack of",
          "vaccine access, it allows us to get a closer look at how systemic",
          "racism affected the United States during the pandemic. For this",
          "study, we decided to delve deeper into this topic and created",
          "3 distinct data stories:")),
  p(paste("1. How is the population percentage of a race in a county correlated",
          "with that county’s overall vaccination rate?")),
  p(paste("2. What is the difference in overall county vaccination rates with",
          "above average populations of a specific race?")),
  p(paste("3. What is the distribution of counties in a state around the",
          "average vaccination rate of the state?")),
  p(strong("How did we conduct this study?")),
  p(paste("For this study, we merged two datasets; one from the CDC on county",
          "vaccination rates, and another from the Census on county demographics.",
          "While conducting our analysis, we filtered these datasets so that",
          "all the information in the final merged dataset was recorded on",
          "the 1st of July, 2021.")),
  p(paste("With the dataset, we created three interactive pages, one for",
          "each question we aimed to answer. These graphs along with our",
          "insights can be found on the next few pages of this application.")),
  p(strong("Link to datasets:")),
  p(strong("Link to GitHub:")),
  titlePanel("Drill Down Data Story"),
  sidebarLayout(
    sidebarPanel(selectInput(inputId = "state_select",
                             label = 'Select a state:',
                             choices = states),
                 p(strong("What does this question help us understand?"))
                 ),
    mainPanel(plotOutput(outputId = 'boxplot'),
              plotOutput(outputId = 'scatter')
              )
    )
)

server <- function(input, output) {

  output$boxplot <- renderPlot({

    state_df <- filter(summary_df, Recip_State == input$state_select,
                       Date == '07/01/2021')
    state_df <- mutate(state_df,
                       complete_per_capita =
                         (Series_Complete_Yes / (TOT_POP_CHILD + TOT_POP_ADULT +
                                                   TOT_POP_ELDER)))
    mean_state_df <- summarize(state_df, mean_completion = mean(complete_per_capita))
    mean_state <- mean_state_df$mean_completion

    wa_df <- filter(state_df, WA_ABOVE_AVERAGE == TRUE)
    wa_df <- select(wa_df, Recip_County, WA = complete_per_capita)

    ba_df <- filter(state_df, BA_ABOVE_AVERAGE == TRUE)
    ba_df <- select(ba_df, Recip_County, BA = complete_per_capita)

    na_df <- filter(state_df, NA_ABOVE_AVERAGE == TRUE)
    na_df <- select(na_df, Recip_County, "NA" = complete_per_capita)

    ia_df <- filter(state_df, IA_ABOVE_AVERAGE == TRUE)
    ia_df <- select(ia_df, Recip_County, IA = complete_per_capita)

    aa_df <- filter(state_df, AA_ABOVE_AVERAGE == TRUE)
    aa_df <- select(aa_df, Recip_County, AA = complete_per_capita)

    h_df <- filter(state_df, H_ABOVE_AVERAGE == TRUE)
    h_df <- select(h_df, Recip_County, H = complete_per_capita)

    dfs <- list(wa_df, aa_df, ia_df, na_df, h_df, ba_df)
    dfs <- reduce(dfs, full_join, by='Recip_County')
    dfs <- melt(dfs, id.vars = 'Recip_County', variable.name = 'complete')

    p <- ggplot(dfs, aes(x = complete, y = value, fill = complete)) +
      geom_boxplot() +
      geom_hline(yintercept=mean_state, color = 'blue')

    return(p)
    })

output$scatter <- renderPlot({

  state_df <- filter(summary_df, Recip_State == input$state_select,
                     Date == '07/01/2021')
  state_df <- mutate(state_df,
                     complete_per_capita =
                       (Series_Complete_Yes / (TOT_POP_CHILD + TOT_POP_ADULT +
                                                 TOT_POP_ELDER)))
  mean_state_df <- summarize(state_df, mean_completion = mean(complete_per_capita))
  mean_state <- mean_state_df$mean_completion

  wa_df <- filter(state_df, WA_ABOVE_AVERAGE == TRUE)
  wa_df <- select(wa_df, Recip_County, WA = complete_per_capita)

  ba_df <- filter(state_df, BA_ABOVE_AVERAGE == TRUE)
  ba_df <- select(ba_df, Recip_County, BA = complete_per_capita)

  na_df <- filter(state_df, NA_ABOVE_AVERAGE == TRUE)
  na_df <- select(na_df, Recip_County, "NA" = complete_per_capita)

  ia_df <- filter(state_df, IA_ABOVE_AVERAGE == TRUE)
  ia_df <- select(ia_df, Recip_County, IA = complete_per_capita)

  aa_df <- filter(state_df, AA_ABOVE_AVERAGE == TRUE)
  aa_df <- select(aa_df, Recip_County, AA = complete_per_capita)

  h_df <- filter(state_df, H_ABOVE_AVERAGE == TRUE)
  h_df <- select(h_df, Recip_County, H = complete_per_capita)

  dfs <- list(wa_df, aa_df, ia_df, na_df, h_df, ba_df)
  dfs <- reduce(dfs, full_join, by='Recip_County')
  dfs <- melt(dfs, id.vars = 'Recip_County', variable.name = 'complete')

  q <- ggplot(dfs, aes(x = complete, y = value, color = complete)) +
    geom_point() +
    geom_hline(yintercept=mean_state, color = 'blue')

  return(q)
})

}


shinyApp(ui = ui, server = server)

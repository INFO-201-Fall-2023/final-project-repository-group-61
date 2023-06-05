library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(shinyWidgets)
library(purrr)
library(reshape2)
library(plotly)

df_info <- read.csv("refined_data_set.csv")
df_info <- filter(df_info, Date == "07/01/2021")
states <- c("All States")
states <- append(states, state.abb)

ui <- fluidPage(
  titlePanel("Drill Down Data Story"),
   sidebarLayout(
     sidebarPanel(
       p("In this section, we are analyzing the distribution of county",
         "vaccination rates by state. Each racial group consists of counties in",
         "which the population of that racial group is above the state’s",
         "average. The box plot highlights the distribution of vaccination",
         "rates in these counties, while the scatterplot serves as a way to",
         "view where each county in the group falls on the box plot. The",
         "horizontal blue line is the median vaccination rate of that state."),
       p("For the analysis, we decided to look at two states on opposite ends of",
       "the political spectrum and take note of the differences. According to",
       "WiseVoter, Vermont (VT) is the most politically blue state and Wyoming (WY) is the",
       "most politically red state (WiseVoter, 2023). Our key insights include",
       "a more detailed analysis of these two states. The analysis also takes",
       "all age groups into account."),
       selectInput(inputId = "state_select",
                              label = 'Select a state:',
                              choices = states),
 radioButtons(inputId = "age",
              label = "Select an age group:",
              choices = c("Child", "Adult", "Elder", "All Age Groups")),
       p(strong("What does this question help us understand?")),
       p("There is a lot of variance between the distribution of vaccination",
         "rates in different states. This highlights the purpose of looking at",
         "each state separately — the situation in every state can be vastly",
         "different from each other, and it is important to have a way to check",
         "on the status of each state separately."),
       h4("Key Insights:"),
       p("The elder age group is generally the most vaccinated and the child",
      "age group is generally the least vaccinated. This is likely due to",
      "elderly citizens being prioritized for COVID-19 vaccinations, especially",
      "in the first year after the vaccine’s release."),
      p("For 4 out of 6 major racial groups, the vaccination rates of Vermont",
        "counties are above the state median of 43.36%. The other two racial",
        "groups, White Alone and Native American, have a high degree of",
        "variance, both with outliers with significantly above state median",
        "vaccination rates of 57.13% and 64.58% respectively."),
      p("Meanwhile, Wyoming has very low vaccination rates among the entire",
        "population. The state median vaccination rate is 31.8%, much lower",
        "than in Vermont. However, its counties have above state median",
        "vaccination rates for 5 out of the 6 racial groups. In addition,",
        "Teton County accounts for a large outlier in Wyoming, with a",
        "vaccination rate of 71.6%."),
      p("On average, people in the 'Asian' racial group are the most vaccinated",
      "in the United States with a median vaccination rate of 42.77%, and people",
      "in the 'Native American' racial group are the least vaccinated, with a median",
      "vaccination rate of 30.63%.")
      ),
    mainPanel(plotlyOutput(outputId = 'boxplot'),
              plotlyOutput(outputId = 'scatter')
    )
  )
)

server <- function(input, output) {

  output$boxplot <- renderPlotly({

    state_df <- filter(df, Date == '07/01/2021')

    if (input$state_select == "All States") {
      state_df <- state_df
    } else {
      state_df <- filter(df, Recip_State == input$state_select)
    }
    
    if (input$age == 'Child') {
      state_df <- mutate(state_df,
                         complete_per_capita = Series_Complete_Child_Pct)
      } else if (input$age == 'Adult') {
        state_df <- mutate(state_df,
                           complete_per_capita = Series_Complete_Adult_Pct)
        } else if (input$age == 'Elder') {
          state_df <- mutate(state_df,
                             complete_per_capita = Series_Complete_Elder_Pct)
          } else {
            state_df <- mutate(state_df,
                               complete_per_capita =
                                 ((Series_Complete_Child +
                                     Series_Complete_Adult +
                                     Series_Complete_Elder) /
                                    (TOT_POP_CHILD + TOT_POP_ADULT + TOT_POP_ELDER)) * 100)
}

    median_state_df <- filter(state_df, !is.na(complete_per_capita))
    median_state_df <- summarize(median_state_df, median_completion = median(complete_per_capita))
    median_state <- median_state_df$median_completion

    wa_df <- filter(state_df, WA_ABOVE_AVERAGE == TRUE)
    wa_df <- group_by(wa_df, Recip_State, Recip_County)
    wa_df <- summarize(wa_df, complete_per_capita = mean(complete_per_capita))
    wa_df <- select(wa_df, Recip_County, Recip_State,
                    'White Alone' = complete_per_capita)

    ba_df <- filter(state_df, BA_ABOVE_AVERAGE == TRUE)
    ba_df <- group_by(ba_df, Recip_State, Recip_County)
    ba_df <- summarize(ba_df, complete_per_capita = mean(complete_per_capita))
    ba_df <- select(ba_df, Recip_County, Recip_State,
                    'Black Alone' = complete_per_capita)

    na_df <- filter(state_df, NA_ABOVE_AVERAGE == TRUE)
    na_df <- group_by(na_df, Recip_State, Recip_County)
    na_df <- summarize(na_df, complete_per_capita = mean(complete_per_capita))
    na_df <- select(na_df, Recip_County, Recip_State,
                    'Native Hawaiian/Pacific Islander' = complete_per_capita)

    ia_df <- filter(state_df, IA_ABOVE_AVERAGE == TRUE)
    ia_df <- group_by(ia_df, Recip_State, Recip_County)
    ia_df <- summarize(ia_df, complete_per_capita = mean(complete_per_capita))
    ia_df <- select(ia_df, Recip_County, Recip_State,
                    'Native American' = complete_per_capita)

    aa_df <- filter(state_df, AA_ABOVE_AVERAGE == TRUE)
    aa_df <- group_by(aa_df, Recip_State, Recip_County)
    aa_df <- summarize(aa_df, complete_per_capita = mean(complete_per_capita))
    aa_df <- select(aa_df, Recip_County, Recip_State,
                    'Asian Alone' = complete_per_capita)

    h_df <- filter(state_df, H_ABOVE_AVERAGE == TRUE)
    h_df <- group_by(h_df, Recip_State, Recip_County)
    h_df <- summarize(h_df, complete_per_capita = mean(complete_per_capita))
    h_df <- select(h_df, Recip_County, Recip_State,
                   'Hispanic Alone' = complete_per_capita)

    dfs <- list(wa_df, aa_df, ia_df, na_df, h_df, ba_df)
    dfs <- reduce(dfs, full_join, by=c('Recip_County', 'Recip_State'))
    dfs <- melt(dfs, id.vars = c('Recip_County', 'Recip_State'),
                                 variable.name = 'complete')

    p <- ggplot(dfs, aes(x = complete, y = value, fill = complete)) +
      geom_boxplot() +
      geom_hline(yintercept=median_state, color = 'blue') +
      labs(x = 'Racial Group',
           y = '% of Fully Vaccinated People in Population',
           fill = 'Racial Group') +
       scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
       scale_fill_discrete(labels = function(fill) str_wrap(fill, width = 5))


     return(p)
     })

 output$scatter <- renderPlotly({

   state_df <- filter(df, Date == '07/01/2021')

   if (input$state_select == "All States") {
     state_df <- state_df
   } else {
     state_df <- filter(df, Recip_State == input$state_select)
   }

   if (input$age == 'Child') {
     state_df <- mutate(state_df,
                       complete_per_capita = Series_Complete_Child_Pct)
   } else if (input$age == 'Adult') {
     state_df <- mutate(state_df,
                        complete_per_capita = Series_Complete_Adult_Pct)
   } else if (input$age == 'Elder') {
     state_df <- mutate(state_df,
                        complete_per_capita = Series_Complete_Elder_Pct)
   } else {
     state_df <- mutate(state_df,
                        complete_per_capita = ((Series_Complete_Child +
                                                 Series_Complete_Adult +
                                                 Series_Complete_Elder) /
                          (TOT_POP_CHILD + TOT_POP_ADULT + TOT_POP_ELDER)) * 100)
   }

   median_state_df <- filter(state_df, !is.na(complete_per_capita))
   median_state_df <- summarize(median_state_df, median_completion = median(complete_per_capita))
   median_state <- median_state_df$median_completion

   wa_df <- filter(state_df, WA_ABOVE_AVERAGE == TRUE)
   wa_df <- group_by(wa_df, Recip_State, Recip_County)
   wa_df <- summarize(wa_df, complete_per_capita = mean(complete_per_capita))
   wa_df <- select(wa_df, Recip_County, Recip_State,
                   'White Alone' = complete_per_capita)

   ba_df <- filter(state_df, BA_ABOVE_AVERAGE == TRUE)
   ba_df <- group_by(ba_df, Recip_State, Recip_County)
   ba_df <- summarize(ba_df, complete_per_capita = mean(complete_per_capita))
   ba_df <- select(ba_df, Recip_County, Recip_State,
                   'Black Alone' = complete_per_capita)

   na_df <- filter(state_df, NA_ABOVE_AVERAGE == TRUE)
   na_df <- group_by(na_df, Recip_State, Recip_County)
   na_df <- summarize(na_df, complete_per_capita = mean(complete_per_capita))
   na_df <- select(na_df, Recip_County, Recip_State, 'Native Hawaiian/Pacific Islander'
                   = complete_per_capita)

   ia_df <- filter(state_df, IA_ABOVE_AVERAGE == TRUE)
   ia_df <- group_by(ia_df, Recip_State, Recip_County)
   ia_df <- summarize(ia_df, complete_per_capita = mean(complete_per_capita))
   ia_df <- select(ia_df, Recip_County, Recip_State,
                   'Native American' = complete_per_capita)

   aa_df <- filter(state_df, AA_ABOVE_AVERAGE == TRUE)
   aa_df <- group_by(aa_df, Recip_State, Recip_County)
   aa_df <- summarize(aa_df, complete_per_capita = mean(complete_per_capita))
   aa_df <- select(aa_df, Recip_County, Recip_State,
                   'Asian Alone' = complete_per_capita)

   h_df <- filter(state_df, H_ABOVE_AVERAGE == TRUE)
   h_df <- group_by(h_df, Recip_State, Recip_County)
   h_df <- summarize(h_df, complete_per_capita = mean(complete_per_capita))
   h_df <- select(h_df, Recip_County, Recip_State,
                  'Hispanic Alone' = complete_per_capita)

   dfs <- list(wa_df, aa_df, ia_df, na_df, h_df, ba_df)
   dfs <- reduce(dfs, full_join, by=c('Recip_County', 'Recip_State'))
   dfs <- melt(dfs, id.vars = c('Recip_County', 'Recip_State'),
               variable.name = 'complete')

   q <- ggplot(dfs, aes(x = complete, y = value, color = complete,
                        text = paste("County:", Recip_County, "\n",
                                     "% of County Vaccinated:", value, "\n",
                                     "State:", Recip_State))) +
     geom_point() +
     geom_hline(yintercept=median_state, color = 'blue') +
     labs(x = 'Racial Group',
          y = '% of Fully Vaccinated People in Population',
          color = 'Racial Group') +
     scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
     scale_color_discrete(labels = function(fill) str_wrap(fill, width = 5))
   q <- ggplotly(q, tooltip = "text")
   return(q)

 })

 }

 shinyApp(ui = ui, server = server)

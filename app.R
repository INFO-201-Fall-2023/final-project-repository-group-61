library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(shiny)
library(shiny.router)
library(purrr)
library(shinyWidgets)
library(reshape2)
library(shinythemes)
library(plotly)


df <- read.csv("refined_data_set.csv")
states <- state.abb

x_axis <- "WA_PCT"
y_axis <- "Series_Complete_Pop_Pct"

ethnic_groups <- c("White", "Black or African American", "American Native or Alaskan Native", "Asian", "Native Hawaiian and Other Pacific Islander", "Hispanic")
ethnic_col <- c("WA_PCT", "BA_PCT", "IA_PCT", "AA_PCT", "NA_PCT", "H_PCT")

age_groups <- c("Child", "Adult", "Elder", "All Age Groups")
age_col <- c("Series_Complete_Child_Pct", "Series_Complete_Adult_Pct", "Series_Complete_Elder_Pct", "Series_Complete_Pop_Pct")


ui <- fluidPage(
  navbarPage("Navbar!", theme = shinytheme("yeti"),
            tabPanel("Introduction",
                     h2("Introduction"),
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
                     tags$div(
                       "Link to datasets: ",
                       tags$a(href="https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh", "County Vaccination Rates Dataset"),
                       "|",
                       tags$a(href="https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-detail.html", "County Demographics Dataset")
                     ),
                     tags$div(
                       "Sources: ",
                       tags$a(href="https://wisevoter.com/state-rankings/red-and-blue-states/", "Wise Voter")
                     ),
                     p(""),
                     tags$a(href= "https://wisevoter.com/state-rankings/red-and-blue-states/", "GitHub Link")

             ),
             navbarMenu("Data Stories",
               tabPanel("Contrast Data Story",
                       titlePanel("Contrast Data Story"),
                       p(paste("In this story, we compare each state's vaccination rates to its",
                               "ethnic population in each county, as well as different age groups.")),
                       sidebarLayout(
                         sidebarPanel(
                          selectizeInput(
                            inputId = "state_name",
                            label = "Which state would you like to look at?",
                            choices = sort(df$STNAME)
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
                          ),
                          p(strong("What does this help us understand?")),
                          p("Comparing the different state vaccination rates to their ethnic population will help us to find if there is a pattern between states with higher populations of certain racial groups and lower vaccination rates across the entire county. Along with this, we can see if these patterns change between age groups or if it is constant overall."),
                          h4("Key Takeaways"),
                          p("Overall, age group does not seem to affect the trend within the ethnic group and state shown that much, so we can conclude that age group does not affect the vaccination rate within states and their counties."),
                          p("Some states do not have complete vaccination information which could affect our overall analysis such as Hawaii and Texas."),
                          p("Throughout most of the states, there seems to be a trend that the higher the Asian population within the county, the higher the vaccination rate seems to be. Contrary to that, for a majority of the states, the higher the White population, the lower the rate of vaccination within each county. There is a downward trend. For the rest of the ethnic groups, there doesn’t seem to be a common upward or downward trend of vaccination rates compared to the ethnic group population and it is hard to conclude anything based on that.")
                        ),
                        mainPanel(plotlyOutput(outputId = 'chart'))
                      )
               ),
               tabPanel("Drill Down Data Story",
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
                        mainPanel(plotOutput(outputId = 'boxplot'),
                                  plotlyOutput(outputId = 'scatter')
                        )
                      )    
               ),
               tabPanel("Zoom Out Data Story",
                 titlePanel("Zoom Out Data Story"),
                 sidebarLayout(
                   sidebarPanel(
                    br("This is a graph that shows the amount of fully vaccinated adults within each racial group in the United States. The user can select any US state to see the differences among the vaccination percentages among the racial groups"),
                    selectInput(inputId =  "state_names",
                                label= "Select a US State",
                                choices = states),
                    radioButtons(inputId = "age",
                                 label = "Select an age group:",
                                 choices = c("Child", "Adult", "Elder", "All Age Groups")
                    ),
                    p(strong("What does this question help us understand?")),
                    p("In this section, we are looking at the average vaccination rate of counties with above average populations of each race by state. This allows us to gain an idea the overall patterns and trends when it comes to racial groups and their vaccination rates."),
                    h4("Key Insights:"),
                    p("There is a lot of variation among the states for which racial groups are most vaccinated. For example, Asian Americans have the highest average vaccination rate in Virginia while Black Americans have the highest average vaccination rate in Iowa. "),
                    p("While this pattern does not appear in all states, overall, Asian Americans are the most likely to be vaccinated while Native Americans or Black Americans are the least likely to be vaccinated.")
                  ),
                  mainPanel(
                    plotlyOutput(outputId = 'df_res')
                  )
                )
              )
            ),
            tabPanel("Summary & About",
                     h2("Summary"),
                     p(strong("Vermont and Wyoming Drill Down")),
                     fluidRow(
                        column(6, imageOutput("vermont")),
                        column(6, imageOutput("wyoming"))
                     ),
                     p("Due to the large variance in distribution between states, we decided to conduct a more detailed analysis of Vermont and Wyoming, respectively the most politically blue and most politically red states. Vermont had a significantly higher median vaccination rate than Wyoming, but both states had a large amount of outliers towards higher vaccination rates. "),
                     p("Most states seem to have a few outliers with significantly higher vaccination rates than others. This could possibly be due to confounding factors like population, whether the region is considered urban or not, and political leaning, as seen in the Vermont vs. Wyoming analysis. Overall, while states seem to have differing median vaccination rates, a high degree of variance is common throughout the states."),
                     p(strong("Virginia and Iowa Zoom Out")),
                     fluidRow(
                        column(6, imageOutput("virginia")),
                        column(6, imageOutput("iowa"))
                     ),
                     p("In this section of our analysis, we found that while there was a pattern in county vaccination rates and their ethnic populations, there is a great degree of variance involved. As we saw in the differences between Iowa and Virginia, different states can have very different vaccination rates based on ethnic group. However, we did find that a large number of the states followed a pattern where counties with above average Asian American populations tend to have the highest vaccination rates, and counties with above average Black or Native American populations tend to have the lowest vaccination rates."),
                     p(strong("Florida and West Virginia Contrast")),
                     fluidRow(
                       column(6, imageOutput("florida")),
                       column(6, imageOutput("westvirginia"))
                     ),
                     p("In the contrast section of the analysis, we determined that throughout many of the states, there was not a strong enough trend for many of the ethnic groups to conclude that there was a correlation between their population within the county and the county's vaccination rate. However, we found two groups to have such a trend: White population and the Asian population. The graphs of Florida's Asian population vs. county vaccination rates and West Virginia's White population vs. county vaccination rate display these two trends. In most states' counties, the higher the Asian population within each county, the higher the vaccination rate that county has, and the opposite is true for the White population: the higher the White population, the lower the vaccination rate. This is contrast to what we first thought would be the case and could have many reasons behind it such as political ideology and the likes."),
                     h2("About"),
                     p("For our study, we narrowed our merged dataset down to July 1, 2021. While we initially intended to conduct a time related analysis, the results were not significant. We decided on this date as it was the date that the demographics dataset was recorded on."),
                     p("Each instance in the dataset refers to the vaccination rates of a county in the United States on July 1 2021, as well as demographic data. The original dataset consisted of many other race sub-groups, but we decided to use the White Alone, Asian Alone, Black Alone, Hispanic Alone, Native American Alone, and Native Hawaiian/Pacific Islander Alone groups. This was because these groups had the largest populations overall."),
                     p("To decide how to group counties, we calculated the average population percent of each racial group in each state. We then labeled each county with TRUE or FALSE for each race group, based upon whether or not that county’s population percent of that race was above the state average."),
                     p("The ranges of our age groups are:"),
                     p("- Child: 5 to 18"),
                     p("- Adult: 18 to 65"),
                     p("- Elder: 65+")
            )
  )
)

# Server stuff goes here
server <- function(input, output) {
  output$chart <- renderPlotly({
    x_axis <- ethnic_col[which(ethnic_groups == input$ethnic_group)]
    y_axis <- age_col[which(age_groups == input$age_group)]
    state_df <- filter(df, Date == "07/01/2021", STNAME == input$state_name)
    p <- ggplot(state_df, aes_string(x = x_axis,  y = y_axis)) +
      geom_point(stat = 'identity') +
      geom_smooth(method="lm", formula=y~x) +
      labs(x = paste(input$ethnic_group, "Population Percentage"), y = paste(input$age_group, "Vaccination Percentage"))
    return(p)
  })
  
  output$boxplot <- renderPlot({
    state_df <- filter(df, Date == '07/01/2021')
    
    if (input$state_select == "All States") {
      state_df <- state_df
    } else {
      state_df <- filter(state_df, Recip_State == input$state_select)
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
    wa_df <- select(wa_df, Recip_County, Recip_State,
                    'White Alone' = complete_per_capita)
    
    ba_df <- filter(state_df, BA_ABOVE_AVERAGE == TRUE)
    ba_df <- select(ba_df, Recip_County, Recip_State,
                    'Black Alone' = complete_per_capita)
    
    na_df <- filter(state_df, NA_ABOVE_AVERAGE == TRUE)
    na_df <- select(na_df, Recip_County, Recip_State,
                    'Native Hawaiian/Pacific Islander' = complete_per_capita)
    
    ia_df <- filter(state_df, IA_ABOVE_AVERAGE == TRUE)
    ia_df <- select(ia_df, Recip_County, Recip_State,
                    'Native American' = complete_per_capita)
    
    aa_df <- filter(state_df, AA_ABOVE_AVERAGE == TRUE)
    aa_df <- select(aa_df, Recip_County, Recip_State,
                    'Asian Alone' = complete_per_capita)
    
    h_df <- filter(state_df, H_ABOVE_AVERAGE == TRUE)
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
      state_df <- filter(state_df, Recip_State == input$state_select)
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
    wa_df <- select(wa_df, Recip_County, Recip_State,
                    'White Alone' = complete_per_capita)
    
    ba_df <- filter(state_df, BA_ABOVE_AVERAGE == TRUE)
    ba_df <- select(ba_df, Recip_County, Recip_State,
                    'Black Alone' = complete_per_capita)
    
    na_df <- filter(state_df, NA_ABOVE_AVERAGE == TRUE)
    na_df <- select(na_df, Recip_County, Recip_State, 'Native Hawaiian/Pacific Islander'
                    = complete_per_capita)
    
    ia_df <- filter(state_df, IA_ABOVE_AVERAGE == TRUE)
    ia_df <- select(ia_df, Recip_County, Recip_State,
                    'Native American' = complete_per_capita)
    
    aa_df <- filter(state_df, AA_ABOVE_AVERAGE == TRUE)
    aa_df <- select(aa_df, Recip_County, Recip_State,
                    'Asian Alone' = complete_per_capita)
    
    h_df <- filter(state_df, H_ABOVE_AVERAGE == TRUE)
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
  
  output$df_res <-renderPlotly({
    
    df_info <- filter(df, Date == "07/01/2021")
    df_info <- filter(df_info, Recip_State == input$state_names)
    print(df_info)
    
    if (input$age == 'Child') {
      df_info <- mutate(df_info,
                        pop_percent = Series_Complete_Child_Pct)
    } else if (input$age == 'Adult') {
      df_info <- mutate(df_info,
                        pop_percent = Series_Complete_Adult_Pct)
    } else if (input$age == 'Elder') {
      df_info <- mutate(df_info,
                        pop_percent = Series_Complete_Elder_Pct)
    } else {
      df_info <- mutate(df_info,
                        pop_percent = ((Series_Complete_Child +
                                          Series_Complete_Adult +
                                          Series_Complete_Elder) /
                                         (TOT_POP_CHILD + TOT_POP_ADULT + TOT_POP_ELDER)) * 100)
    }
    
    wa_df <- select(df_info, Recip_County, pop_percent, WA_ABOVE_AVERAGE)
    wa_df <- filter(wa_df, WA_ABOVE_AVERAGE == TRUE)
    wa_df <- group_by(wa_df, WA_ABOVE_AVERAGE)
    wa_df <- summarize(wa_df, 'White Alone' = mean(pop_percent))
    wa_df <- rename(wa_df, 'ABOVE_AVG'='WA_ABOVE_AVERAGE')
    
    aa_df <- select(df_info, Recip_County, pop_percent, AA_ABOVE_AVERAGE)
    aa_df <- filter(aa_df, AA_ABOVE_AVERAGE == TRUE)
    aa_df <- group_by(aa_df, AA_ABOVE_AVERAGE)
    aa_df <- summarize(aa_df, 'Asian Alone' = mean(pop_percent))
    aa_df <- rename(aa_df, 'ABOVE_AVG'='AA_ABOVE_AVERAGE')
    
    ba_df <- select(df_info, Recip_County, pop_percent, BA_ABOVE_AVERAGE)
    ba_df <- filter(ba_df, BA_ABOVE_AVERAGE == TRUE)
    ba_df <- group_by(ba_df, BA_ABOVE_AVERAGE)
    ba_df <- summarize(ba_df, 'Black Alone' = mean(pop_percent))
    ba_df <- rename(ba_df, 'ABOVE_AVG'='BA_ABOVE_AVERAGE')
    
    ia_df <- select(df_info, Recip_County, pop_percent, IA_ABOVE_AVERAGE)
    ia_df <- filter(ia_df, IA_ABOVE_AVERAGE == TRUE)
    ia_df <- group_by(ia_df, IA_ABOVE_AVERAGE)
    ia_df <- summarize(ia_df, 'Native American Alone' = mean(pop_percent))
    ia_df <- rename(ia_df, 'ABOVE_AVG'='IA_ABOVE_AVERAGE')
    
    na_df <- select(df_info, Recip_County, pop_percent, NA_ABOVE_AVERAGE)
    na_df <- filter(na_df, NA_ABOVE_AVERAGE == TRUE)
    na_df <- group_by(na_df, NA_ABOVE_AVERAGE)
    na_df <- summarize(na_df, "Native Hawaiian/Pacfiic Islander Alone" = mean(pop_percent))
    na_df <- rename(na_df, 'ABOVE_AVG'='NA_ABOVE_AVERAGE')
    
    h_df <- select(df_info, Recip_County, pop_percent, H_ABOVE_AVERAGE)
    h_df <- filter(h_df, H_ABOVE_AVERAGE == TRUE)
    h_df <- group_by(h_df, H_ABOVE_AVERAGE)
    h_df <- summarize(h_df, 'Hispanic Alone' = mean(pop_percent))
    h_df <- rename(h_df, 'ABOVE_AVG'='H_ABOVE_AVERAGE')
    
    dfs <- list(wa_df, aa_df, ba_df, ia_df, na_df, h_df)
    dfs <- reduce(dfs, full_join, by='ABOVE_AVG')
    dfs <- melt(dfs, id.vars = 'ABOVE_AVG', variable.name = 'variable') 
    p <- ggplot(dfs, aes(x= variable, y=value, fill=variable, text = paste(value, '%'))) +
      geom_bar(stat = "identity") +
      labs(x = "Racial Groups", y = "% Population of Fully Vacinated", fill = 'Racial Groups') +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
  
  output$vermont <- renderImage({
    list(src = "vermont.png",
         width = "100%",
         height = 330)
  }, deleteFile = F)
  
  output$wyoming <- renderImage({
    list(src = "wyoming.png",
         width = "100%",
         height = 330)
  }, deleteFile = F)
  
  output$virginia <- renderImage({
    list(src = "virginia.png",
         width = "100%",
         height = 330)
  }, deleteFile = F)
  
  output$iowa <- renderImage({
    list(src = "iowa.png",
         width = "100%",
         height = 330)
  }, deleteFile = F)
  
  output$florida <- renderImage({
    list(src = "florida.png",
         width = "100%",
         height = 330)
  }, deleteFile = F)
  
  output$westvirginia <- renderImage({
    list(src = "westvirginia.png",
         width = "100%",
         height = 330)
  }, deleteFile = F)
}

# Make the app
shinyApp(ui, server)

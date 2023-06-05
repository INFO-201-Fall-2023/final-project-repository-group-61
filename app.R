library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(purrr)

states<- state.abb

df_info <- read.csv("refined_data_set.csv")
df_info <- filter(df_info, Date == "07/01/2021")

ui <- fluidPage(
  titlePanel("Zoom Out Data Story"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId =  "state_name",
                  label= "Select a US State",
                  choices = states),
      radioButtons(inputId = "age",
                   label = "Select an age group:",
                   choices = c("Child", "Adult", "Elder", "All Age Groups")
      ),
    ),
    mainPanel(
      plotlyOutput(outputId = 'df_res'),
      h3("Key Insights:"),
      p("Add text here")))
)

#server stuff goes here 


server <- function(input, output) {
  output$df_res <-renderPlotly({
    
    df_info <- filter(df_info, Recip_State== input$state_name)
    
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
    dfs = melt(dfs)
    p <- ggplot(dfs, aes(x= variable, y=value, fill=variable,
                         text = paste(value, '%'))) +
      geom_bar(stat = "identity") +
      labs(x = "Racial Groups", y = "% Population of Fully Vacinated",
           fill = 'Racial Groups') +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
}

shinyApp(ui=ui, server=server)

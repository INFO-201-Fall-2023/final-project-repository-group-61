library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(purrr)

states<- state.abb

df_info <- read.csv("refined_data_set.csv")
df_info <- filter(df_info, Date == "07/01/2021")


#UI 
#ui <- fluidPage(
  
#)

#Server 
#server <- function(input, output){
  
#}

#Making app
#library(shiny)

ui <- fluidPage(selectInput(
  inputId =  "state_name",
  label= "Select a US State",
  choices = states),
  plotOutput(outputId = 'df_res'))
  

  


#server stuff goes here 


server <- function(input, output) {
     output$df_res <-renderPlot({
       
       df_info <- filter(df_info, Recip_State== input$state_name)
    

    
    wa_df <- select(df_info, Recip_County, Series_Complete_Yes, WA_ABOVE_AVERAGE)
    wa_df <- filter(wa_df, WA_ABOVE_AVERAGE == TRUE)
    wa_df <- group_by(wa_df, WA_ABOVE_AVERAGE)
    wa_df <- summarize(wa_df, WA = mean(Series_Complete_Yes))
    wa_df <- rename(wa_df, 'ABOVE_AVG'='WA_ABOVE_AVERAGE')
    
    aa_df <- select(df_info, Recip_County, Series_Complete_Yes, AA_ABOVE_AVERAGE)
    aa_df <- filter(aa_df, AA_ABOVE_AVERAGE == TRUE)
    aa_df <- group_by(aa_df, AA_ABOVE_AVERAGE)
    aa_df <- summarize(aa_df, AA = mean(Series_Complete_Yes))
    aa_df <- rename(aa_df, 'ABOVE_AVG'='AA_ABOVE_AVERAGE')
    
    ba_df <- select(df_info, Recip_County, Series_Complete_Yes, BA_ABOVE_AVERAGE)
    ba_df <- filter(ba_df, BA_ABOVE_AVERAGE == TRUE)
    ba_df <- group_by(ba_df, BA_ABOVE_AVERAGE)
    ba_df <- summarize(ba_df, BA = mean(Series_Complete_Yes))
    ba_df <- rename(ba_df, 'ABOVE_AVG'='BA_ABOVE_AVERAGE')
    
    ia_df <- select(df_info, Recip_County, Series_Complete_Yes, IA_ABOVE_AVERAGE)
    ia_df <- filter(ia_df, IA_ABOVE_AVERAGE == TRUE)
    ia_df <- group_by(ia_df, IA_ABOVE_AVERAGE)
    ia_df <- summarize(ia_df, IA = mean(Series_Complete_Yes))
    ia_df <- rename(ia_df, 'ABOVE_AVG'='IA_ABOVE_AVERAGE')
    
    na_df <- select(df_info, Recip_County, Series_Complete_Yes, NA_ABOVE_AVERAGE)
    na_df <- filter(na_df, NA_ABOVE_AVERAGE == TRUE)
    na_df <- group_by(na_df, NA_ABOVE_AVERAGE)
    na_df <- summarize(na_df, "NA_" = mean(Series_Complete_Yes))
    na_df <- rename(na_df, 'ABOVE_AVG'='NA_ABOVE_AVERAGE')
    
    h_df <- select(df_info, Recip_County, Series_Complete_Yes, H_ABOVE_AVERAGE)
    h_df <- filter(h_df, H_ABOVE_AVERAGE == TRUE)
    h_df <- group_by(h_df, H_ABOVE_AVERAGE)
    h_df <- summarize(h_df, H = mean(Series_Complete_Yes))
    h_df <- rename(h_df, 'ABOVE_AVG'='H_ABOVE_AVERAGE')
    
    dfs <- list(wa_df, aa_df, ba_df, ia_df, na_df, h_df)
    dfs <- reduce(dfs, full_join, by='ABOVE_AVG')
    dfs = melt(dfs)
    p <- ggplot(dfs, aes(x= variable, y=value, fill=variable)) +
      geom_bar(stat = "identity")+
      labs(x = "Racial Groups", y = "% Population of Fully Vacinated")
    return(p)
      })
}

shinyApp(ui=ui, server=server)
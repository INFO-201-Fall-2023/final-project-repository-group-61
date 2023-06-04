library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)


df <- read.csv("/Users/srijamidas/Downloads/refined_data_set.csv")

summary_cols <- select(summary_df, Recip_County, Recip_State, c(107:112))
df <- merge(df, summary_cols, by=c('Recip_County', 'Recip_State'))
df <- mutate(df, complete_per_capita_18plus = (Series_Complete_18Plus - Series_Complete_65Plus) / TOT_POP_ADULT)
df <- mutate(df, complete_per_capita_elder = (Series_Complete_65Plus / TOT_POP_ADULT))

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
  choices = df$Recip_State
  
 
),

tableOutput(
  outputId = "df_res"
)
)
  


#server stuff goes here 
server <- function(input, output) {
 output$df_res <-renderTable({
 
   df_info <- filter(df, Series_Complete_Pop_pct== input$state_name)
   
  vwa_df <- select(df_info, Date, complete_per_capita_18plus, WA_ABOVE_AVERAGE)
  vwa_df <- filter(vwa_df, WA_ABOVE_AVERAGE == TRUE)
  vwa_df <- group_by(vwa_df, Date)
  vwa_df <- summarize(vwa_df, wa_mean_complete_per_capita_18plus = mean(complete_per_capita_18plus))
  
  vaa_df <- select(df_info, Date, complete_per_capita_18plus, AA_ABOVE_AVERAGE)
  vaa_df <- filter(vaa_df, AA_ABOVE_AVERAGE == TRUE)
  vaa_df <- group_by(vaa_df, Date)
  vaa_df <- summarize(vaa_df, aa_mean_complete_per_capita_18plus = mean(complete_per_capita_18plus))
  
  vba_df <- select(df_info, Date, complete_per_capita_18plus, BA_ABOVE_AVERAGE)
  vba_df <- filter(vba_df, BA_ABOVE_AVERAGE == TRUE)
  vba_df <- group_by(vba_df, Date)
  vba_df <- summarize(vba_df, ba_mean_complete_per_capita_18plus = mean(complete_per_capita_18plus))
  
  via_df <- select(df_info, Date, complete_per_capita_18plus, IA_ABOVE_AVERAGE)
  via_df <- filter(via_df, IA_ABOVE_AVERAGE == TRUE)
  via_df <- group_by(via_df, Date)
  via_df <- summarize(via_df, ia_mean_complete_per_capita_18plus = mean(complete_per_capita_18plus))
  
  vna_df <- select(df_info, Date, complete_per_capita_18plus, NA_ABOVE_AVERAGE)
  vna_df <- filter(vna_df, NA_ABOVE_AVERAGE == TRUE)
  vna_df <- group_by(vna_df, Date)
  vna_df <- summarize(vna_df, na_mean_complete_per_capita_18plus = mean(complete_per_capita_18plus))
  
  vh_df <- select(df_info, Date, complete_per_capita_18plus, H_ABOVE_AVERAGE)
  vh_df <- filter(vh_df, H_ABOVE_AVERAGE == TRUE)
  vh_df <- group_by(vh_df, Date)
  vh_df <- summarize(vh_df, h_mean_complete_per_capita_18plus = mean(complete_per_capita_18plus))
  
  dfs <- list(vwa_df, vaa_df, via_df, vna_df, vh_df, vba_df)
  dfs <- reduce(dfs, full_join, by='Recip_County')
  dfs <- melt(dfs, id.vars = 'Recip_County', variable.name = 'complete')
  return(df_info)
  

  
  })
  }
  
    
    
 
  



shinyApp(ui, server)
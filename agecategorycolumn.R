library(readr)
library(data.table)
library(dplyr)

vacc_df <- read.csv("county_vacc_jul2021.csv")
raw_demographics_df <- read.csv("county_demographics_jul2021.csv")



#filtering dates in demographics df to 1st july 2021
demographics_df <- filter(raw_demographics_df, YEAR == 3)

# separating date column
vacc_df$Month <- substr(vacc_df$Date, 1, 2)
vacc_df$Day <- substr(vacc_df$Date, 4, 5)
vacc_df$Year <- substr(vacc_df$Date, 7, 10)

# filtering out weekly data from july 2021 in vaccinations df
days <- c('01', '08', '15', '22', '29')
july_vacc_df <- filter(vacc_df, Day %in% days, Month == '07', Year == '2021')

#making age categories: child, adult, elder

#demographics_df <- demographics_df %>% mutate(demographics_df, age_category = (AGEGRP %in% c(2, 3 , 4) ,"CHILD" & ifelse(AGEGRP %in% c(5,6,7,8,9,10,11,12,13) ,"ADULT", "ELDER")

#demographics_df <- demographics_df %>% mutate(demographics_df, age_category = ifelse(AGEGRP %in% c(5,6,7,8,9,10,11,12,13) ,"ADULT", "ELDER" )) 
#ages  
#demographics_df$age_category <- ifelse(demographics_df$AGEGRP <= 4, "CHILD", "ADULT")


demographics_df <- demographics_df %>% mutate(age_category = ifelse(AGEGRP <= 4, "CHILD",
                               ifelse(AGEGRP %in% c(5,6,7,8,9,10,11,12,13), "ADULT", "ELDER")))


 
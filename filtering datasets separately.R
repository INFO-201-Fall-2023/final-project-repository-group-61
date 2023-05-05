library(readr)
library(data.table)
library(dplyr)

# reading in datasets
vacc_df <- fread('county_vaccinations.csv')
raw_demographics_df <- read_csv("county_demographics.csv")

#filtering dates in demographics df to 1st july 2021
demographics_df <- filter(raw_demographics_df, YEAR == 3)

# separating date column
vacc_df$Month <- substr(vacc_df$Date, 1, 2)
vacc_df$Day <- substr(vacc_df$Date, 4, 5)
vacc_df$Year <- substr(vacc_df$Date, 7, 10)

# filtering out weekly data from july 2021 in vaccinations df
days <- c('01', '08', '15', '22', '29')
july_vacc_df <- filter(vacc_df, Day %in% days, Month == '07', Year == '2021')

# creating new csv files
write.csv(july_vacc_df, '~/Documents/info201/project/county_vacc_jul2021.csv')
write.csv(demographics_df, '~/Documents/info201/project/county_demographics_jul2021.csv')
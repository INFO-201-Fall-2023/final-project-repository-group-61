library(readr)
library(dplyr)

vacc_df <- read.csv("county_vacc_jul2021.csv")
demo_df <- read.csv("county_demographics_jul2021.csv")

#making age categories: child, adult, elder

demo_df <- filter(demo_df, AGEGRP >= 2)
demo_df <-  mutate(demo_df, age_category = ifelse(AGEGRP <= 4, "CHILD",
                                               ifelse(AGEGRP %in% c(5,6,7,8,9,10,11,12,13), "ADULT", "ELDER")))
demo_df <- relocate(demo_df, age_category, .after=AGEGRP)

# adding up totals

demo_df <- mutate(demo_df, TOT_WA = WA_MALE + WA_FEMALE)
demo_df <- relocate(demo_df, TOT_WA, .after=WA_FEMALE)

demo_df <- mutate(demo_df, TOT_BA = BA_MALE + BA_FEMALE)
demo_df <- relocate(demo_df, TOT_BA, .after=BA_FEMALE)

demo_df <- mutate(demo_df, TOT_IA = IA_MALE + IA_FEMALE)
demo_df <- relocate(demo_df, TOT_IA, .after=IA_FEMALE)

demo_df <- mutate(demo_df, TOT_AA = AA_MALE + AA_FEMALE)
demo_df <- relocate(demo_df, TOT_AA, .after=AA_FEMALE)

demo_df <- mutate(demo_df, TOT_NA = NA_MALE + NA_FEMALE)
demo_df <- relocate(demo_df, TOT_NA, .after=NA_FEMALE)

demo_df <- mutate(demo_df, TOT_H = H_MALE + H_FEMALE)
demo_df <- relocate(demo_df, TOT_H, .after=H_FEMALE)

# adding up age groups
col_names <- names(demo_df)
col_names <- col_names[10:88]
grouped_demo_df <- group_by(demo_df, STNAME, CTYNAME, age_category)
grouped_demo_df <- summarise(grouped_demo_df, across(all_of(col_names), sum))

# removing extra rows & any NA

vacc_df <- vacc_df[,0:29]
vacc_df_col_names <- names(vacc_df)
vacc_df[is.na(vacc_df)] <- 0

# merging dataframes

grouped_demo_df$state_code <- state.abb[match(grouped_demo_df$STNAME, state.name)]
grouped_demo_df <- ungroup(grouped_demo_df, STNAME)

vector_app <- function(target_col_name, age_cat) {
  target_df <- grouped_demo_df[grouped_demo_df$age_category == age_cat, ]
  vec_append <- target_df[c(1:nrow(target_df)), c(target_col_name, 'CTYNAME', 'state_code')]
  return(vec_append)
}


demo_tester <- filter(grouped_demo_df, age_category == 'ADULT')
demo_tester <- select(demo_tester, state_code, CTYNAME, total_adult_pop = TOT_POP)


merger_df <- left_join(x = vacc_df,
                       y = demo_tester,
                       by = c('Recip_State' = 'state_code', 'Recip_County' = 'CTYNAME'))

demo_tester <- filter(grouped_demo_df, age_category == 'CHILD')
demo_tester <- select(demo_tester, state_code, CTYNAME, total_child_pop = TOT_POP)


merger_df <- left_join(x = merger_df,
                       y = demo_tester,
                       by = c('Recip_State' = 'state_code', 'Recip_County' = 'CTYNAME'))

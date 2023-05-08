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
demo_tester <- select(demo_tester, state_code, CTYNAME, TOT_POP_ADULT = TOT_POP)

df <- left_join(x = vacc_df,
                y = demo_tester,
                by = c('Recip_State' = 'state_code', 'Recip_County' = 'CTYNAME'))

demo_tester <- filter(grouped_demo_df, age_category == 'CHILD')
demo_tester <- select(demo_tester, state_code, CTYNAME, TOT_POP_CHILD = TOT_POP)

df <- left_join(x = df,
                y = demo_tester,
                by = c('Recip_State' = 'state_code', 'Recip_County' = 'CTYNAME'))

demo_tester <- filter(grouped_demo_df, age_category == 'ELDER')
demo_tester <- select(demo_tester, state_code, CTYNAME, TOT_POP_ELDER = TOT_POP)

df <- left_join(x = df,
                y = demo_tester,
                by = c('Recip_State' = 'state_code', 'Recip_County' = 'CTYNAME'))

age_cat <- c('CHILD', 'ADULT', 'ELDER')
wanted_cols <- grouped_demo_df[,5:82]
wanted_col_names <- names(wanted_cols)

for (name in wanted_col_names) {
  for (category in age_cat) {
    new_merge = vector_app(name, category)
    colnames(new_merge)[1] <- paste(name, '_', category, sep = "")
    df <- left_join(x = df,
                    y = new_merge,
                    by = c('Recip_State' = 'state_code', 'Recip_County' = 'CTYNAME'))
  }
}

df <- na.omit(df)

# Summart Dataset
summary_df <- df[, 1:200]
summary_df <- summary_df[-c(84:191)]

summary_df <- mutate(summary_df, TOT_WA = TOT_WA_CHILD + TOT_WA_ADULT + TOT_WA_ELDER)
summary_df <- relocate(summary_df, TOT_WA, .after=TOT_WA_ELDER)

summary_df <- mutate(summary_df, TOT_BA = TOT_BA_CHILD + TOT_BA_ADULT + TOT_BA_ELDER)
summary_df <- relocate(summary_df, TOT_BA, .after=TOT_BA_ELDER)

summary_df <- mutate(summary_df, TOT_IA = TOT_IA_CHILD + TOT_IA_ADULT + TOT_IA_ELDER)
summary_df <- relocate(summary_df, TOT_IA, .after=TOT_IA_ELDER)

summary_df <- mutate(summary_df, TOT_AA = TOT_AA_CHILD + TOT_AA_ADULT + TOT_AA_ELDER)
summary_df <- relocate(summary_df, TOT_AA, .after=TOT_AA_ELDER)

summary_df <- mutate(summary_df, TOT_NA = TOT_NA_CHILD + TOT_NA_ADULT + TOT_NA_ELDER)
summary_df <- relocate(summary_df, TOT_NA, .after=TOT_NA_ELDER)

summary_df <- mutate(summary_df, TOT_H = TOT_H_CHILD + TOT_H_ADULT + TOT_H_ELDER)
summary_df <- relocate(summary_df, TOT_H, .after=TOT_H_ELDER)

demo_sum <- group_by(demo_df, STNAME, CTYNAME)
demo_sum <- summarise(demo_sum, across(all_of(col_names), sum))

demo_sum_2 <- group_by(demo_df, STNAME)
demo_sum_2 <- summarise(demo_sum, across(all_of(col_names), sum))

demo_sum_2 <- mutate(demo_sum_2, WA_PCT = TOT_WA / TOT_POP * 100)
demo_sum_2 <- mutate(demo_sum_2, BA_PCT = TOT_BA / TOT_POP* 100)
demo_sum_2 <- mutate(demo_sum_2, IA_PCT = TOT_IA / TOT_POP * 100)
demo_sum_2 <- mutate(demo_sum_2, AA_PCT = TOT_AA / TOT_POP * 100)
demo_sum_2 <- mutate(demo_sum_2, NA_PCT = TOT_NA / TOT_POP * 100)
demo_sum_2 <- mutate(demo_sum_2, H_PCT = TOT_H / TOT_POP * 100)
demo_sum_2 <- demo_sum_2[, -c(2:80)]
demo_sum_2$state_code <- state.abb[match(demo_sum_2$STNAME, state.name)]


summary_df <- full_join(x = summary_df, y = demo_sum_2, by = c('Recip_State' = 'state_code'))
summary_df <- relocate(summary_df, STNAME, .after = Recip_County)

summary_df <- mutate(summary_df, TOT_POP = TOT_POP_CHILD + TOT_POP_ADULT + TOT_POP_ELDER)
summary_df <- mutate(summary_df, WA_ABOVE_AVERAGE = (TOT_WA / TOT_POP* 100 >= WA_PCT))
summary_df <- mutate(summary_df, BA_ABOVE_AVERAGE = (TOT_BA / TOT_POP* 100 >= BA_PCT))
summary_df <- mutate(summary_df, IA_ABOVE_AVERAGE = (TOT_IA / TOT_POP* 100 >= IA_PCT))
summary_df <- mutate(summary_df, AA_ABOVE_AVERAGE = (TOT_AA / TOT_POP* 100 >= AA_PCT))
summary_df <- mutate(summary_df, NA_ABOVE_AVERAGE = (TOT_NA / TOT_POP* 100 >= NA_PCT))
summary_df <- mutate(summary_df, H_ABOVE_AVERAGE = (TOT_H / TOT_POP* 100 >= H_PCT))

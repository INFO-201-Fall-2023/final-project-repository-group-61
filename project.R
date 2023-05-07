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





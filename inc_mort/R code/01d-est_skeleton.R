#' 
#' title: Create skeleton dataset for estimates up to 2025
#' author: Mathieu Bastard
#' date: 29/05/2026
#' 



#'  Given the new system, a country/year with no data is not exported from WIDP
#'  This script creates an empty skeleton for each 215 countries and territories
#'  And for each of them, a time-series from 2000 to 2025 in year
#'  Output: est.Rda


rm(list=ls())

library(data.table)
library(dplyr)
library(tidyr)
library(readxl)



# Load country list

source(here("import/load_gtb.R"))
cty <- load_gtb("cty",convert_dashes = TRUE)
load(here("inc_mort/estimates2025/est.rda"))
oest <- copy(est)
rm(est)


# Estimate year max
yr=2025

# Create the skeleton dataset from last year estimate dataset

est <- oest[,.(iso3,year)]
est <- merge(est,cty[,.(iso3,country,g.whoregion,g.whostatus)],by="iso3")

# Create 2025 obs by duplicating 2024
data_2025 <- est %>%
  filter(year == 2024) %>%
  mutate(year = 2025)

# Append it to original dataset
est <- bind_rows(est, data_2025)

# Order by iso3/year

est <- est %>%
  arrange(iso3, year)


head(est)


# Update Member state list from WHO website

who_ms <- as.data.table(read_excel(
  "data/gtb/other/WHO_member_states.xlsx",
  col_names = TRUE
  ))

who_ms <- who_ms %>%
  mutate(
    member_state="Yes"
  ) %>%
  dplyr::select(iso3,member_state)

# Merge to est

est <- merge(est,who_ms,by="iso3",all.x=T)
est[is.na(member_state), member_state := "No"]

cross_tab <- table(est$g.whostatus, est$member_state, useNA = "always")
print(cross_tab)





#Save file
attr(est, "timestamp") <- Sys.Date()
save(est, file = here('inc_mort/analysis/est.rda'))






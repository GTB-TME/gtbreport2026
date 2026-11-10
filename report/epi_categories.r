# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Determine the epidemiological categorization of countries and write to CSV by Hazim Timimi
# This is based on incidence rates:
#
# severely endemic: e_inc_100k >= 500
# highly endemic: e_inc_100k 300–499
# endemic: e_inc_100k 100–299
# upper-moderate incidence: e_inc_100k 50–99
# lower-moderate incidence: e_inc_100k 10-49
# low incidence: e_inc_100k < 10
#
#
# This will inform the Annex 3 table in GTB report 2025.
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# load libraries
library(RODBC)
library(dplyr)
library(ggplot2)

# KILL factors 
options(stringsAsFactors = FALSE)

load(here::here("inc_mort/analysis/est.rda"))

names(est) <- gsub('[.]', '_', names(est))

source(here::here("import/load_gtb.R"))

countries <- load_gtb("cty")

epi_categories <- est |> 
  select(iso3, 
         year, 
         e_inc_100k = inc,
         e_inc_num = inc_num) |> 
  filter(year == 2024) |> 
  inner_join(countries, by="iso3") |> 
  select(g_whoregion, country, e_inc_100k, e_inc_num)

epi_categories$category <- cut(epi_categories$e_inc_100k,
                          c(0, 9.999, 49.999, 99.999, 299.999, 499.999, Inf),
                          c("low", "lower-moderate", "upper-moderate", "endemic", "highly endemic", "severely endemic"),
                          right = FALSE)

# Write country lists to CSV in order of category (high to low), region and country
epi_categories |> 
  select(category, g_whoregion, country, e_inc_100k, e_inc_num) |> 
  arrange(desc(category), g_whoregion, country) |> 
  write.csv(file = paste0(here::here("report/local"), "/epi_categories_", Sys.Date(), ".csv"),
          row.names = FALSE)

# Calculate share of global incidence numbers accounted for by each category

epi_cat_shares <- epi_categories %>%
  group_by(category) %>%
  summarise(members = n(),
            incidence = sum(e_inc_num))

inc_tot <- epi_cat_shares %>%
  summarise(global_incidence = sum(incidence))

epi_cat_shares <- epi_cat_shares %>%
  mutate(share = incidence * 100 / inc_tot$global_incidence)

# write result to CSV to your local drive
epi_cat_shares %>%
  select(category, members, share) %>%
  arrange(desc(category), members, share) %>%
  write.csv(file = paste0(here::here("report/local"), "/epi_categories_summary", Sys.Date(), ".csv"),
          row.names = FALSE)

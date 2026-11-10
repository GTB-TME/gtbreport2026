# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch5.rmd UHC and TB determinants
# Takuya Yamanaka, May 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(dplyr)
library(tidyr)
library(here)

library(tidyverse)
library(readxl)
library(magrittr)
library(scales)
library(stringr)
library(gtbreport)
library(metafor)
library("zoo")
library(english)

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))
source(here("report/functions/NZ.R"))

load(here("data/imf/imfweo.Rda"))

# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

report_year <- 2026
snapshot_date <- latest_snapshot_date()

show_estimates <- T
latest_estimates <- T

# Load GTB data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

notification <- load_gtb("tb")
TBHIV_for_aggregates <- load_gtb("agg")
estimates_population <- load_gtb("pop")

grpmbr <- load_gtb("grpmbr")
sdg <- load_gtb("sdg")
sp <- load_gtb("maftb")
diag_treatment <- load_gtb("diag_treatment")

en_name <- load_gtb("cty") |>
  select(iso3, country_in_text_EN)

who_region_shortnames <- region_shortnames()


# merge snapshots
notification <- notification |>
  left_join(diag_treatment, by = c("iso3","year","country","g_whoregion"))

# calc c_newinc
notification <- notification |>
  mutate(
    c_newinc = if_else(
      year >= 2025,
      
      {
        tmp <- pick(any_of(c(
          "pulm_labconf_new",
          "pulm_labconf_unk",
          "pulm_labconf_rec",
          "pulm_clindx_new",
          "pulm_clindx_unk",
          "pulm_clindx_rec",
          "ep_newinc"
        )))
        
        
        ifelse(
          rowSums(!is.na(tmp)) == 0,
          NA_real_,
          rowSums(tmp, na.rm = TRUE)
        )
      },
      
      {
        tmp <- pick(any_of(c(
          "new_labconf",
          "new_clindx",
          "ret_rel_labconf",
          "ret_rel_clindx",
          "new_ep",
          "ret_rel_ep"
        )))
        
        ifelse(
          rowSums(!is.na(tmp)) == 0,
          NA_real_,
          rowSums(tmp, na.rm = TRUE)
        )
      }
    )
  )

# load imf data for converting cost data from current USD to constant USD
load(here("data/imf/imfweo.Rda"))

# list iso3 - country
list_iso3_country <-
  grpmbr %>% filter(group_type == 'g_whoregion') %>%
  select(iso3,country)

list_watch_list <-
  list_iso3_country %>%
  filter(iso3 %in% c('GAB','LBR'))

list_hbcs <-
  grpmbr %>%
  filter(group_type == "g_hb_tb" & group_name == 1) %>%
  select(iso3,country) %>%
  arrange(country)

list_hbcs_plus_wl <- list_hbcs %>% add_row(list_watch_list) %>%   arrange(country)

iso3_hbc <- list_hbcs$iso3
iso3_hbc_plus_wl <- list_hbcs_plus_wl$iso3

iso3_income <- grpmbr %>%
  filter(group_type == "g_income") %>% select(iso3,group_name) %>% rename(g_income=2)

list_hbcs_income <-
  list_hbcs %>%
  left_join(iso3_income)

list_hbcs_plus_wl_income <-
  list_hbcs_plus_wl %>%
  left_join(iso3_income)

dummydf <-
  list_hbcs_income %>%
  mutate(year=0,value=0) %>%
  select(iso3,year,value,country,g_income)


# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)

# Get global and regional estimates directly from the files that get imported into the database
if(show_estimates) {
  if(latest_estimates) {
  
  load(here::here("inc_mort/analysis/est.rda"))
  
  # Functions to adjust estimates based on country requests for the 2025 report !!!
  source(here::here("report/functions/adjust_estimates_2025.R"))
  
  # Apply the adjustments to country-level burden estimates
  est <- adjust_country_est(est)
  
  est_country <- est
  load(here::here("inc_mort/analysis/global.rda"))
  est_global <- global
  load(here::here("inc_mort/analysis/regional.rda"))
  est_regional <- regional
  
  # Get Pete's aggregate incidence estimates by age group and sex
  load(here('disaggregation/output/db_estimates_country_all.Rdata'))
  
  # Apply the adjustments to country-level burden estimates
  db_estimates_country_all <- adjust_country_disaggs(db_estimates_country_all, est)
  
  load(here('disaggregation/output/db_estimates_group_all.Rdata'))
  
  load(here::here("inc_mort/analysis/rf.global.rda"))
  rf_global <- rf.global %>%
    rename(risk_factor = risk.factor)
  load(here::here("inc_mort/analysis/rf.rda"))
  rf_country <- rf %>%
    rename(risk_factor = risk.factor) %>%
    #Blanking DPRK
    mutate(best = ifelse(iso3 == "PRK", NA,best)) %>%
    mutate(lo = ifelse(iso3 == "PRK", NA,lo)) %>%
    mutate(hi = ifelse(iso3 == "PRK", NA,hi)) %>%
    mutate(best = ifelse(iso3 == "IND", NA,best)) %>%
    mutate(lo = ifelse(iso3 == "IND", NA,lo)) %>%
    mutate(hi = ifelse(iso3 == "IND", NA,hi))
    
  
  
  
  } else {
    # until the first Rd estimates ready, use last year's estimates that were copied from last year's github repo to this year's local - DO NOT push!
    load(here::here("local/inc_mort/analysis/est.rda"))
    est_country <- est |>
      select(-c.newinc, -g.whoregion)
    est_country_ym2 <- est_country |>
      filter(year == report_year-2)
    est_country_ym2 <- est_country_ym2 |>
      mutate(year = report_year-1)
    est_country <- rbind(est_country, est_country_ym2) |>
      arrange(iso3, year)
    
    newinc_coutry <- notification |>
      select(iso3, year, g.whoregion = g_whoregion, c_newinc) |>
      filter(year>=2000)
    
    est_country <- newinc_coutry |>
      left_join(est_country, by = c("iso3", "year"))
    
    load(here::here("local/inc_mort/analysis/regional.rda"))
    est_regional <- regional|>
      select(-c.newinc)
    est_regional_ym2 <- est_regional |>
      filter(year == report_year-2)
    est_regional_ym2 <- est_regional_ym2 |>
      mutate(year = report_year-1)
    est_regional <- rbind(est_regional, est_regional_ym2) |>
      arrange(g.whoregion,year)
    
    newinc_regional <- newinc_coutry |>
      select(-iso3) |>
      group_by(year, g.whoregion) |>
      summarise_all(sum, na.rm = T) |>
      ungroup()
    est_regional <- newinc_regional |>
      left_join(est_regional, by = c("g.whoregion", "year"))
    
    load(here::here("local/inc_mort/analysis/global.rda"))
    est_global <- global |>
      select(-c.newinc)
    est_global_ym2 <- est_global |>
      filter(year == report_year-2)
    est_global_ym2 <- est_global_ym2 |>
      mutate(year = report_year-1)
    est_global <- rbind(est_global, est_global_ym2) |>
      arrange(year)
    
    newinc_global <- newinc_regional |>
      select(-g.whoregion) |>
      group_by(year) |>
      summarise_all(sum, na.rm = T) |>
      ungroup()
    est_global <- newinc_global |>
      left_join(est_global, by = c("year"))
    
    
    # Get Pete's aggregate incidence estimates by age group and sex
    load(here('local/disaggregation/output/db_estimates_country_all.Rdata'))
    db_estimates_country_all <- db_estimates_country_all |>
      mutate(year = report_year-1)
    
    load(here('local/disaggregation/output/db_estimates_group_all.Rdata'))
    db_estimates_group_all <- db_estimates_group_all |>
      mutate(year = report_year-1)
    
  
  } 
}

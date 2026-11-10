# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Common data preparation script for chapters (sections) 2
# Takuya Yamanaka, June 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load data packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(stringr)
library(dplyr)
library(tidyr)
library(here)
library(readr) # to save csv
library(magrittr) # to use tee pipe
library(data.table)
library(jsonlite) # for provisional monthly notification in India
library(gtbreport)

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))
source(here("report/functions/NZ.R"))
source(here("report/functions/calculate_outcomes.R"))


# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2026
snapshot_date <- latest_snapshot_date()
csv_datestamp <- snapshot_date
lsaved_year <- 2010

# Set whether or not to include objects with estimates in the output ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

show_estimates <- T
latest_estimates <- T
latest_dr_estimates <- T
latest_agesex_estimates_global <- T
latest_agesex_estimates_regional <- T
latest_lsaved <- T
datacoll <- F

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))
source(here("report/functions/NZ.R"))


# Load GTB data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

countries <- load_gtb("cty") 
en_name <- load_gtb("cty") |>
  select(iso3, country_in_text_EN)

notification <- load_gtb("tb")
tbhiv <- load_gtb("tbhiv")
tpt <- load_gtb("tpt")
diag_treatment <- load_gtb("diag_treatment")
lab <- load_gtb("lab_diag")
TBHIV_for_aggregates <- load_gtb("agg")
# estimates_ltbi <- load_gtb("ltbi")
# strategy <- load_gtb("sty")
estimates_population <- load_gtb("pop")
grpmbr <- load_gtb("grpmbr")
grp <- load_gtb("grp")
# data_collection <- load_gtb("datacoll")
dr_surveillance <- load_gtb("drs")

outcomes <- load_gtb("outcomes")
prisons <- load_gtb("prisons")
ppm <- load_gtb("ppm")

cb_data <- read_csv(here::here("data/amend/cb_data.csv")) # digital surveillance data from the 2025 repo

who_region_shortnames <- region_shortnames()

# Fix lists of the three sets of 30 high burden countries (used to filter records for some figures)
hbc30 <- hb_list("tb")

hbmdr30 <- grpmbr |>
  filter(group_type == "g_hb_mdr" & group_name == 1) |>
  select(iso3,group_type)

hbtbhiv30 <- grpmbr |>
  filter(group_type == "g_hb_tbhiv" & group_name == 1) |>
  select(iso3,group_type)


# list iso3 - country
list_iso3_country <-
  grpmbr |> filter(group_type == 'g_whoregion') |>
  select(iso3,country)

list_watch_list <-
  grpmbr |> filter(group_type == 'g_watchlist') |>
  select(iso3,country)

list_hbcs <-
  grpmbr |>
  filter(group_type == "g_hb_tb" & group_name == 1) |>
  select(iso3,country) |>
  arrange(country)

list_hbcs_plus_wl <- list_hbcs |> add_row(list_watch_list) |>   arrange(country)

iso3_hbc <- list_hbcs$iso3
iso3_hbc_plus_wl <- list_hbcs_plus_wl$iso3
iso3_hmdrc <- hbmdr30$iso3


iso3_income <- grpmbr |>
  filter(group_type == "g_income") |> select(iso3,group_name) |> rename(g_income=2)

list_hbcs_income <-
  list_hbcs |>
  left_join(iso3_income)

list_hbcs_plus_wl_income <-
  list_hbcs_plus_wl |>
  left_join(iso3_income)

# WB income group list
wb_incomelist <- grpmbr |>
  filter(group_type == "g_income") |> select(iso3,group_name) |> rename(income=2)

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

notification <- notification |>
  mutate(c_newinc = if_else(year <= 2012, 

                            {
                              tmp <- pick(any_of(c(
                                "tot_newrel",
                                "newret_oth"
                              )))
                              
                              ifelse(
                                rowSums(!is.na(tmp)) == 0,
                                NA_real_,
                                rowSums(tmp, na.rm = TRUE)
                              )
                            },
                            c_newinc))

# calc c_notified
notification <- notification |>
  mutate(
    c_notified = if_else(
      year >= 2025,
      
      {
        
        tmp <- pick(any_of(c(
          "pulm_labconf_new",
          "pulm_labconf_unk",
          "pulm_labconf_rec",
          "pulm_labconf_pulm_rereg",
          "pulm_clindx_new",
          "pulm_clindx_unk",
          "pulm_clindx_rec",
          "pulm_clindx_pulm_rereg",
          "ep_newinc",
          "ep_ep_rereg"
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
          "ret_rel_ep",
          "ret_nrel"
        )))
        
        ifelse(
          rowSums(!is.na(tmp)) == 0,
          NA_real_,
          rowSums(tmp, na.rm = TRUE)
        )
      }
    )
  )

notification <- notification |>
  mutate(c_notified = if_else(year <= 2012, rowSums(pick(tot_newrel, ret_taf,	ret_tad, ret_oth, newret_oth), na.rm = TRUE), c_notified))


# merge snapshots
notification <- notification |>
  left_join(tbhiv, by = c("iso3","year","country","g_whoregion")) |>
  left_join(diag_treatment, by = c("iso3","year","country","g_whoregion"))

# additional variables
notification <- notification |>
  mutate(
    new_labconf = if_else(year >= 2025, rowSums(pick(pulm_labconf_new, pulm_labconf_rec, pulm_labconf_unk), na.rm = TRUE), new_labconf),
    new_clindx = if_else(year >= 2025, rowSums(pick(pulm_clindx_new, pulm_clindx_rec, pulm_clindx_unk), na.rm = TRUE), new_clindx),
    new_ep = if_else(year >= 2025, ep_newinc, new_ep),
    ret_rel_labconf = if_else(year >= 2025, pulm_labconf_pulm_rereg, ret_rel_labconf),
    ret_rel_clindx = if_else(year >= 2025, pulm_clindx_pulm_rereg, ret_rel_clindx),
    ret_rel_ep = if_else(year >= 2025, ep_ep_rereg, ret_rel_ep),
    pulm_labconf_ret = if_else(year >= 2025, rowSums(pick(pulm_labconf_pulm_rereg, pulm_labconf_rec), na.rm = TRUE), pulm_labconf_ret),
    c_ret = if_else(is.na(ret_rel_labconf) & is.na(ret_rel_clindx) & is.na(ret_rel_ep) & is.na(ret_nrel), NA, rowSums(pick(ret_rel_labconf, ret_rel_clindx, ret_rel_ep, ret_nrel), na.rm = TRUE)),
    c_ret = if_else(year < 2013 & (!is.na(ret_rel) | !is.na(ret_tad) | !is.na(ret_taf) | !is.na(ret_oth)), rowSums(pick(ret_rel, ret_tad, ret_taf, ret_oth), na.rm = TRUE), c_ret)
  )

notification <- notification |>
  mutate(
    newrel_m15plus = if_else(is.na(newrel_m15plus) & agegroup_option == 220, rowSums(pick(newrel_m1519, newrel_m2024, newrel_m2534, newrel_m3544, newrel_m4554, newrel_m5564, newrel_m65), na.rm = TRUE), newrel_m15plus),
    newrel_m15plus = if_else(is.na(newrel_m15plus) & (is.na(agegroup_option) | agegroup_option == 221|agegroup_option == 222), rowSums(pick(newrel_m1524, newrel_m2534, newrel_m3544, newrel_m4554, newrel_m5564, newrel_m65), na.rm = TRUE), newrel_m15plus),
    newrel_f15plus = if_else(is.na(newrel_f15plus) & agegroup_option == 220, rowSums(pick(newrel_f1519, newrel_f2024, newrel_f2534, newrel_f3544, newrel_f4554, newrel_f5564, newrel_f65), na.rm = TRUE), newrel_f15plus),
    newrel_f15plus = if_else(is.na(newrel_f15plus) & (is.na(agegroup_option) | agegroup_option == 221|agegroup_option == 222), rowSums(pick(newrel_f1524, newrel_f2534, newrel_f3544, newrel_f4554, newrel_f5564, newrel_f65), na.rm = TRUE), newrel_f15plus),
    c_new_014 = if_else(agegroup_option == 220, rowSums(pick(newrel_m04, newrel_m59, newrel_m1014, newrel_f04, newrel_f59, newrel_f1014), na.rm = TRUE), NA),
    c_new_014 = if_else(agegroup_option == 221, rowSums(pick(newrel_m04, newrel_m514, newrel_f04, newrel_f514), na.rm = TRUE), c_new_014),
    c_new_014 = if_else(agegroup_option == 222, rowSums(pick(newrel_m014, newrel_f014), na.rm = TRUE), c_new_014),
    c_new_014 = if_else(is.na(agegroup_option), rowSums(pick(newrel_m04, newrel_m59, newrel_m514, newrel_m1014, newrel_f04, newrel_f59, newrel_f1014, newrel_f514), na.rm = TRUE), c_new_014)
  )


dr_surveillance <- dr_surveillance |>
  mutate(
    dst_rlt_new = if_else(year >= 2025, rowSums(pick(contains("dst_new_rr"),contains("dst_new_rs")), na.rm = TRUE), dst_rlt_new),
    dst_rlt_ret = if_else(year >= 2025, rowSums(pick(contains("dst_ret_rr"),contains("dst_ret_rs")), na.rm = TRUE), dst_rlt_ret),
    rdst_new = if_else(year >= 2025, rowSums(pick(r_rlt_dst_new, r_rlt_dst_unk), na.rm = TRUE), NA),
    rdst_ret = if_else(year >= 2025, rowSums(pick(r_rlt_dst_ret), na.rm = TRUE), NA)
  )

# Get global and regional estimates directly from the files that get imported into the database
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
  }

if(latest_dr_estimates) {
  load(here::here('drtb/output/db_dr_country.rda'))
  
  # Apply the adjustments to country-level burden estimates
  db_dr_country <- adjust_country_dr(db_dr_country, est)
  # 
  est_dr_country <- db_dr_country
  
  load(here::here('drtb/output/db_dr_group.rda'))
  est_dr_group <- db_dr_group
  
  } else {
  
    load(here::here('local/drtb/output/db_dr_country.rda'))
    est_dr_country <- db_dr_country
    est_dr_country_ym2 <- est_dr_country |>
      filter(year == report_year-2)
    est_dr_country_ym2 <- est_dr_country_ym2 |>
      mutate(year = report_year-1)
    est_dr_country <- rbind(est_dr_country, est_dr_country_ym2) |>
      arrange(year)
    
    load(here::here('local/drtb/output/db_dr_group.rda'))
    est_dr_group <- db_dr_group
    est_dr_group_ym2 <- est_dr_group |>
      filter(year == report_year-2)
    est_dr_group_ym2 <- est_dr_group_ym2 |>
      mutate(year = report_year-1)
    est_dr_group <- rbind(est_dr_group, est_dr_group_ym2) |>
      arrange(year)
  }


if(latest_agesex_estimates_regional) {  
  
  # Get Pete's aggregate incidence estimates by age group and sex
  load(here('disaggregation/output/db_estimates_country_all.Rdata'))
  
  # # Apply the adjustments to country-level burden estimates
  db_estimates_country_all <- adjust_country_disaggs(db_estimates_country_all, est)
  # 
  load(here('disaggregation/output/db_estimates_group_all.Rdata'))
  
  
} else {
  
    # Get Pete's aggregate incidence estimates by age group and sex
    load(here('local/disaggregation/output/db_estimates_country_all.Rdata'))
  db_estimates_country_all <- db_estimates_country_all |>
      mutate(year = report_year-1)
    
    load(here('local/disaggregation/output/db_estimates_group_all.Rdata'))
    db_estimates_group_all <- db_estimates_group_all |>
      mutate(year = report_year-1)
    
    
  } 


if(latest_lsaved) {
  load(here::here(paste0('lives_saved/output/regional_saved_',lsaved_year,'.rda')))
  lives_saved <- saved.regional.print
  
  load(here::here(paste0('lives_saved/output/regional_saved_',2005,'.rda')))
  lives_saved_hiv <- saved.regional.print
} else {
  load(here::here(paste0('local/lives_saved/output/regional_saved_',lsaved_year,'.rda')))
  lives_saved <- saved.regional.print
  
  load(here::here(paste0('local/lives_saved/output/regional_saved_',2005,'.rda')))
  lives_saved_hiv <- saved.regional.print
  
}

# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)







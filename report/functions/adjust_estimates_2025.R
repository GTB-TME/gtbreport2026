# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Adjust TB burden estimates for publication in the global TB report
# and its associated products
# 
# Run these functions before using TB burden estimate records in generating
# figures, tables, web pages, export files etc.
#
# This version is for the 2025 report and is based on requests from
# country ministries of health

# Hazim Timimi, September 2025
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# In 2025 we decided that we should only publish overall incidence and mortality with no disaggregations
# for countries which had fewer than 10 cases in any year since 2000. These are entities with usually a 
# small population and also a very low TB burden, so disaggregations do not make sense.

small_entities_iso3_codes <- function(est_table) {
  est_table[c_newinc<10, unique(iso3)]
}


adjust_country_est <- function(est_table){
  
  # Adjust the estimates data table, usually the one loaded from here::here("inc_mort/analysis/est.rda")
  # Note that the := assignment in data.table works by reference so the original table passed into the function
  # IS updated. It does not create a copy of the table, unlike in standard R operations
  # See https://stackoverflow.com/a/14293056
  #
  # Call as est <- adjust_country_est(est)
  
  # Make sure the data.table package is loaded
  library(data.table)

  
  # blank out TBHIV and mortality estimates in DPRK
  # requested by NTP/PRK in Aug 2020
  # Code adapted from PG's 09-export.R script

  est_table[iso3 == "PRK", 
            c("tbhiv", "tbhiv.lo", "tbhiv.hi",
              "inc.h", "inc.h.lo", "inc.h.hi",
              "inc.h.num", "inc.h.lo.num", "inc.h.hi.num",
              "mort.h", "mort.h.lo", "mort.h.hi",
              "mort.h.num", "mort.h.lo.num", "mort.h.hi.num",
              "mort.nh", "mort.nh.lo", "mort.nh.hi",
              "mort.nh.num", "mort.nh.lo.num", "mort.nh.hi.num",
              "mort", "mort.lo", "mort.hi",
              "mort.num", "mort.lo.num", "mort.hi.num",
              "source.mort") := NA]

  
  # In August 2025 DPRK requested the suppression of all estimates pending a decision to publish 
  # So remove the main incidence and case fatality ratio estimates too
  est_table[iso3 == "PRK",
            c("source.inc",
              "inc", "inc.lo", "inc.hi",
              "inc.num", "inc.lo.num", "inc.hi.num",
              "cfr", "cfr.lo", "cfr.hi") := NA] 

  # In July 2023 Republic of Korea also questioned why we have TB/HIV estimates
  # when they have not provided any data on this for many years, so we agreed to blank out
  # their TB/HIV incidence estimates

  est_table[iso3 == "KOR", 
            c("tbhiv", "tbhiv.lo", "tbhiv.hi",
              "inc.h", "inc.h.lo", "inc.h.hi",
              "inc.h.num", "inc.h.lo.num", "inc.h.hi.num") := NA]
  
  
  # In 2025 we decided that we should only publish overall incidence and mortality with no disaggregations
  # for countries which had fewer than 10 cases in any year since 2000. These are entities with usually a 
  # small population and also a very low TB burden, so disaggregations do not make sense.

  est_table[iso3 %in% small_entities_iso3_codes(est_table), 
            c("tbhiv", "tbhiv.lo", "tbhiv.hi",
              "inc.h", "inc.h.lo", "inc.h.hi",
              "inc.h.num", "inc.h.lo.num", "inc.h.hi.num",
              "mort.h", "mort.h.lo", "mort.h.hi",
              "mort.h.num", "mort.h.lo.num", "mort.h.hi.num",
              "mort.nh", "mort.nh.lo", "mort.nh.hi",
              "mort.nh.num", "mort.nh.lo.num", "mort.nh.hi.num",
              "cfr", "cfr.lo", "cfr.hi") := NA]
  
  
  return(est_table)
  
}

adjust_country_disaggs <- function(disaggs_table, est_table) {
  
  # Adjust the risk factor estimates data table, usually the one loaded from here::here("inc_mort/analysis/rf.rda"
  # Also works with the age/sex incidence estimates data table, usually loaded from here::here("disaggregation/output/db_estimates_country_all.Rdata")
  # Note that the := assignment in data.table works by reference so the original table passed into the function
  # IS updated. It does not create a copy of the table, unlike in standard R operations
  # See https://stackoverflow.com/a/14293056
  #
  # Call as rf <- adjust_country_disaggs(rf, est)
  # and
  # db_estimates_country_all <- adjust_country_disaggs(db_estimates_country_all, est)
  
  # Make sure the data.table package is loaded
  library(data.table)
  
  # In August 2025 DPRK requested the suppression of all estimates pending a decision to publish them

  disaggs_table[iso3 == 'PRK', 
           c("best", "lo", "hi") := NA] 
  
  
  # In 2025 we decided that we should only publish overall incidence and mortality with no disaggregations
  # for countries which had fewer than 10 cases in any year since 2000. These are entities with usually a 
  # small population and also a very low TB burden, so disaggregations do not make sense.
  
  # Remove any estimates for small entities

  disaggs_table[iso3 %in% small_entities_iso3_codes(est_table), 
           c("best", "lo", "hi") := NA] 
  
  return(disaggs_table)

}

adjust_country_dr <- function(dr_table, est_table) {
  
  # Adjust the country drug-resistant TB estimates data table, usually the one loaded from here::here("drtb/output/db_dr_country.rda")
  # Note that the := assignment in data.table works by reference so the original table passed into the function
  # IS updated. It does not create a copy of the table, unlike in standard R operations
  # See https://stackoverflow.com/a/14293056
  #
  # Call as db_dr_country <- adjust_country_dr(db_dr_country, est)

  # Make sure the data.table package is loaded
  library(data.table)
  
  # In August 2025 DPRK requested the suppression of all estimates pending a decision to publish them
  
  dr_table[iso3 == 'PRK', 
           c("source_new", "source_ret", 
             "e_rr_prop_new", "e_rr_prop_new_lo", "e_rr_prop_new_hi", "e_rr_prop_new_se",
             "e_rr_prop_ret", "e_rr_prop_ret_lo", "e_rr_prop_ret_hi", "e_rr_prop_ret_se",
             "e_inc_rr_num", "e_inc_rr_num_lo", "e_inc_rr_num_hi" ) := NA] 

  # In 2025 we decided that we should only publish overall incidence and mortality with no disaggregations
  # for countries which had fewer than 10 cases in any year since 2000. These are entities with usually a 
  # small population and also a very low TB burden, so disaggregations do not make sense.
  
  # Remove any estimates for small entities

  dr_table[iso3 %in% small_entities_iso3_codes(est_table),
           c("source_new", "source_ret", 
             "e_rr_prop_new", "e_rr_prop_new_lo", "e_rr_prop_new_hi", "e_rr_prop_new_se",
             "e_rr_prop_ret", "e_rr_prop_ret_lo", "e_rr_prop_ret_hi", "e_rr_prop_ret_se",
             "e_inc_rr_num", "e_inc_rr_num_lo", "e_inc_rr_num_hi" ) := NA] 
  
  return(dr_table)
  
}

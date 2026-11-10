# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation/imputation script, Part I up to running cleaning script
# Translated from Stata version written by A SiroKa and P Nguhiu
# Before running this code, you must make sure that ch4_clean_data.R is revised and 
# up-to-date according to the latest finance data review 
# Takuya Yamanaka, last update July 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load cleaning script
source(here::here('finance/2a_clean_raw_data.R'))
# source(here::here('finance/2a_clean_raw_data.R'))

# Zero treated same as missing
finance_merged <- finance_merged |>
  mutate(
    rcvd_tot = if_else(rcvd_tot == 0, NA_real_, rcvd_tot),
    rcvd_tot_sources = if_else(rcvd_tot == 0, NA_real_, rcvd_tot_sources),
    exp_tot = if_else(exp_tot == 0, NA_real_, exp_tot),
    cf_tot = if_else(cf_tot == 0, NA_real_, cf_tot),
    cf_tot_sources = if_else(cf_tot == 0, NA_real_, cf_tot_sources)
  )

# Manually setting countries where not enough data was available to accurately fill in rcvd_tot
# These THREE low/middle income countries are Dominica, Grenada, and Uzbekistan who have last reported data from 2014, 2016, and 2018, respectively.
# FOUR Other countries: Costa Rica, Turkey, Mauritius, and St. Vincent
# There are a further TWO countries which should not be backwards filled as the reported data is more than 4 years into the future: Albania, and Gambia.
finance_merged <- finance_merged |>
  mutate(
    do_not_fill = if_else(
      iso3 %in% c(#"ALB", "CRI", 
        "DMA", #"GMB", 
        "GRD", "MUS", "TUR", "UZB", "VCT"),1,0))

finance_merged <- finance_merged |>
  mutate(flag = 0)

# Reported data (1)
finance_merged <- finance_merged |>
  mutate(flag = if_else(!is.na(rcvd_tot) & year != report_year, 1, flag))

# Use expenditure data (2)
finance_merged <- finance_merged |>
  mutate(flag = if_else(is.na(rcvd_tot) & !is.na(exp_tot) & year != report_year & do_not_fill != 1 & flag == 0, 2, flag))

# Use committed funding (3)
finance_merged <- finance_merged |>
  arrange(country, year) |>
  mutate(flag = if_else(is.na(rcvd_tot) & !is.na(cf_tot) & year != report_year & do_not_fill != 1 & flag == 0, 3, flag))




# Use last year's rcvd (4)
finance_merged <- finance_merged |>
  arrange(country, year) |>
  group_by(iso3) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(rcvd_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0, 4, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0, 5, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(rcvd_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0, 4, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0, 5, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(rcvd_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0, 4, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0, 5, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(rcvd_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0, 4, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0, 5, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(rcvd_tot, 5)) & year != report_year & do_not_fill != 1 & flag == 0, 4, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 5)) & year != report_year & do_not_fill != 1 & flag == 0, 5, flag)) |>
  ungroup()
  

# If year 2020 rcvd is based on expenditure and year 2021 is blank, we want a flag of 4 for 2021 saying it is based 
# off last year's rcvd, which itself would have a flag of 2 (based off of expenditure or CF)
finance_merged <- finance_merged |>
  arrange(country, year) |>
  group_by(iso3) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(exp_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 1) == 2, 4, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(exp_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 2) == 2, 4, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(exp_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 3) == 2, 4, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(exp_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 4) == 2, 4, flag)) |>
  ungroup()

finance_merged <- finance_merged |>
  arrange(country, year) |>
  group_by(iso3) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(cf_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 1) == 3, 4, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(cf_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 2) == 3, 4, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(cf_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 3) == 3, 4, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(cf_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 4) == 3, 4, flag))  |>
  ungroup()

# same logic for the year using next year's data
finance_merged <- finance_merged |>
  arrange(country, year) |>
  group_by(iso3) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(exp_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 1) == 2, 5, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(exp_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 2) == 2, 5, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(exp_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 3) == 2, 5, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(exp_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 4) == 2, 5, flag)) |>
  ungroup()

finance_merged <- finance_merged |>
  arrange(country, year) |>
  group_by(iso3) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(cf_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 1) == 3, 5, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(cf_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 2) == 3, 5, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(cf_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 3) == 3, 5, flag)) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(cf_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 4) == 3, 5, flag))  |>
  ungroup()


# Fill holes in earlier years making all blanks equal to next year with rcvd_tot (5)
# finance_merged <- finance_merged |>
#   arrange(country, year) |>
#   mutate(flag = case_when(
#     is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0 ~ 5,
#     is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0 ~ 5,
#     is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0 ~ 5,
#     is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0 ~ 5,
#     TRUE ~ flag
#   ))

finance_merged <- finance_merged |>
  arrange(country, year) |>
  mutate(flag = ifelse(is.na(rcvd_tot) & year == 2013 & do_not_fill != 1 & flag == 4, 5, flag)) #|> # temporal arrangement for Angola 2013, not to use 2012 data

finance_merged <- finance_merged |>
  dplyr::select(country:g_whoregion, g_income, g_hb_tb:g_hb_tbhiv, everything())

finance_merged <- finance_merged |>
  mutate(flag = ifelse(iso3 == "SRB" & year == 2024, 4, flag))
  
# save intermediate output .rda
# rm(notification, outcomes, #regional, 
#    tpt, wb, wb_incomelist, weo, finance, finance_pre2014#, estimates_population
#    )
# save.image(here::here("finance/local/finance_cleaned_raw.Rda"))



# Save as .rda files
save(finance_merged, file = paste0("./finance/local/finance_cleaned_raw", ".rda"))
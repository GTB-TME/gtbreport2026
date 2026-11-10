  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Data loading script - testing version
  # Takuya Yamanaka, last update July 2026
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
  library(readxl)
  
  # Load functions ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  source(here("import/load_gtb.R"))
  source(here("report/functions/country_region_lists.R"))
  source(here("report/functions/NZ.R"))
  # source(here("report/functions/calculate_outcomes.R"))
  
  # Create output folder (only if it doesn't yet exist)
  fin_local <- here::here("finance/local")
  dir.create(fin_local, showWarnings = FALSE, recursive = TRUE)
  
  # Set the report year ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  report_year <- 2026
  start_year <- 2015
  snapshot_date <- latest_snapshot_date()
  
  # Set variables
  base_year <- report_year - 1
  
  # Set whether or not to include objects with estimates in the output ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  show_estimates <- F
  
  # Load GTB data ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  notification <- load_gtb("tb")
  tbhiv <- load_gtb("tbhiv")
  tpt <- load_gtb("tpt")
  diag_treatment <- load_gtb("diag_treatment")
  TBHIV_for_aggregates <- load_gtb("agg")
  estimates_population <- load_gtb("pop")
  grpmbr <- load_gtb("grpmbr")
  grp <- load_gtb("grp")
  expenditure <- load_gtb("expenditure") 
  budget <- load_gtb("budget") 
  short <- load_gtb("finance_short") 
  utilization <- load_gtb("utilization") 
  
  outcomes <- load_gtb("outcomes")
  
  # - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # merging finance dataset ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  short <- short |>
    mutate(short = 1)
  
  
  convert_numeric <- function(df) {
    df |>
      mutate(
        across(
          -c(country, iso3, g_whoregion),
          ~ as.numeric(.)
        )
      )
  }
  
  budget      <- convert_numeric(budget)
  expenditure      <- convert_numeric(expenditure)
  utilization      <- convert_numeric(utilization)
  
  # calc gaps in budget
  budget <- budget |>
    select(-starts_with("bf_"))
  
  
  vars <- c(
    "lab", "staff", "fld", "prog", "sld",
    "mdrmgt", "tpt", "tbhiv", "patsup",
    "orsrvy", "oth"
  )
  
  for(v in vars) {
    budget[[paste0("gap_", v)]] <-
      ifelse(
        !is.na(budget[[paste0("budget_", v)]]) &
          !is.na(budget[[paste0("cf_", v)]]),
        budget[[paste0("budget_", v)]] -
          budget[[paste0("cf_", v)]],
        NA
      )
  }
  
  short <- short |>
    select(!bf_tot_req) |>
    filter(!is.na(exp_tot) | !is.na(budget_tot))
  
  
  finance <- budget |>
    full_join(expenditure, by = c("country", "iso3", "year", "g_whoregion")) |>
    full_join(utilization, by = c("iso3","year","country","g_whoregion")) |>
    full_join(short, by = c("country", "iso3", "year", "g_whoregion"))|>
    select(-starts_with("bf_"))
  
  
  finance <- finance |>
    mutate(rcvd_tot_domestic = if_else(!is.na(short), exp_tot, rcvd_tot_domestic),
           cf_tot_domestic = if_else(!is.na(short), budget_tot, cf_tot_domestic),
           rcvd_staff = if_else(!is.na(short), exp_tot, rcvd_staff),
           budget_staff = if_else(!is.na(short), budget_tot, budget_staff)
    ) 
    
  
  # calc total
  finance <- finance |>
    mutate(
      cf_tot = if_else(
        if_all(contains("cf_") & !contains("cf_tot"), is.na),
        NA_real_,
        rowSums(pick(contains("cf_") & !contains("cf_tot")), na.rm = TRUE)
      ),
      
      budget_tot = if_else(
        is.na(budget_tot),
        if_else(
          if_all(contains("budget_") & !contains("budget_tot") & !contains("budget_cpp_"), is.na),
          NA_real_,
          rowSums(pick(contains("budget_") & !contains("budget_tot") & !contains("budget_cpp_")), na.rm = TRUE)
        ),
        budget_tot
      ),
      
      gap_tot = if_else(
        if_all(contains("gap_") & !contains("gap_tot"), is.na),
        NA_real_,
        rowSums(pick(contains("gap_") & !contains("gap_tot")), na.rm = TRUE)
      ),
      
      exp_tot = if_else(
        is.na(exp_tot),
        if_else(
          if_all(contains("exp_") & !contains("exp_tot") & !contains("exp_cpp_"), is.na),
          NA_real_,
          rowSums(pick(contains("exp_") & !contains("exp_tot") & !contains("exp_cpp_")), na.rm = TRUE)
        ),
        exp_tot
      ),
      
      rcvd_tot = if_else(
        if_all(contains("rcvd_") & !contains("rcvd_tot"), is.na),
        NA_real_,
        rowSums(pick(contains("rcvd_") & !contains("rcvd_tot")), na.rm = TRUE)
      ),
      
      cf_tot_sources = if_else(
        if_all(contains("cf_tot_"), is.na),
        NA_real_,
        rowSums(pick(contains("cf_tot_")), na.rm = TRUE)
      ),
      
      rcvd_tot_sources = if_else(
        if_all(contains("rcvd_tot_"), is.na),
        NA_real_,
        rowSums(pick(contains("rcvd_tot_")), na.rm = TRUE)
      )
    )
  
  # rename WIDP variables 
  finance <- finance |>
    rename(
      hospd_dstb_dur = hospd_dur_dstb,
      hospd_mdr_dur = hospd_dur_mdr,
      hospd_dstb_prct = hospd_prct_dstb,
      hospd_mdr_prct = hospd_prct_mdr
    )

  
  finance      <- finance     |>
    filter(year>2014) |>
    arrange(iso3, year)
  
  finance <- finance |>
    complete(
      iso3,
      year = 2015:report_year
    ) |>
    group_by(iso3) |>
    fill(country, g_whoregion, .direction = "downup") |>
    ungroup()
  
  finance      <- finance     |>
    left_join(select(estimates_population, year, iso3, e_pop_num), by = c("year","iso3"))
  
  save(finance, file = paste0("./finance/output/finance", ".rda"))

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  who_region_shortnames <- region_shortnames()
  
  # Fix lists of the three sets of 30 high burden countries (used to filter records for some figures)
  hbc30 <- hb_list("tb") |>
    mutate(g_hb_tb = 1) |>
    select(-group_type)
  
  hbmdr30 <- grpmbr |>
    filter(group_type == "g_hb_mdr" & group_name == 1) |>
    mutate(g_hb_mdr = 1) |>
    select(iso3, g_hb_mdr)
  
  hbtbhiv30 <- grpmbr |>
    filter(group_type == "g_hb_tbhiv" & group_name == 1) |>
    mutate(g_hb_tbhiv = 1) |>
    select(iso3, g_hb_tbhiv)
  
  
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
  
  
  # Load WB data ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # source(here("import/save_wb.R")) # running this code may take 2-3 minutes
  load(here("data/wb/wb.Rda"))
  
  # Load IMF data ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # source(here("import/save_imfweo.R")) # running this code may take 2-3 minutes
  load(here("data/imf/imfweo.Rda"))
  
  weo <- weo |>
    # select(-dataset,-country) |>
    select(iso3, year, imf_cabal_pcgdp=BCA_NGDPD, imf_debt_pcgdp=GGXWDG_NGDP, imf_govexp_pcgdp=GGX_NGDP,
           imf_gdp_pc_cur_usd=NGDPDPC, imf_gdp_pc_cur_lcu=NGDPPC, imf_gdp_pc_con_lcu=NGDPRPC, imf_deflator=NGDP_D,
           imf_ppp_conv=PPPEX, imf_gdp_pc_cur_int=PPPPC, deflator:deflator_us)
  
  # recalculate 
  def_base <- weo |>
    group_by(iso3) |>
    filter(year == report_year-1) |>
    select(iso3, base_deflator = deflator, base_deflator_us = deflator_us, base_imf_deflator = imf_deflator)
  
  weo <- weo |>
    left_join(def_base, by = "iso3") |>
    mutate(deflator = deflator/base_deflator,
           deflator_us = deflator_us/base_deflator_us,
           imf_deflator = imf_deflator/base_imf_deflator)
  
  # Kill any attempt at using factors, unless we explicitly want them!
  options(stringsAsFactors=FALSE)
  
  # Load pre-2014 finance data and merge with the snapshot data----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  finance_pre2014 <- haven::read_dta(here::here("finance/raw/finance1.dta")) |>
    filter(year <= 2014)
  finance_pre2014 <- finance_pre2014[, names(finance_pre2014) %in% names(finance)]
  
  finance <- finance |>
    filter(year > 2014) |>
    plyr::rbind.fill(finance_pre2014) |>
    arrange(country, year)
  
  # UN operational rates of exchange ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  treasury <- haven::read_dta(here::here("finance/raw/treasury.dta"))
  
  treasury_iso3 <- treasury |>
    select(country, iso3) |>
    distinct()
  
  unore <-  read_excel(here::here('data/unore/unore.xlsx')) |>
    select(abbrv_name:rate) |>
    rename(country=1, currenty=2, un_pa_exch_rate=5) |>
    mutate(eff_date = str_trim(eff_date),
           eff_date = as.Date(eff_date, format = "%d %b %Y")) |>
    mutate(year = as.numeric(format(eff_date, "%Y"))) |>
    group_by(country, year) |>
    summarize(un_pa_exch_rate = mean(un_pa_exch_rate, na.rm = TRUE)) |>
    ungroup() |>
    filter(year >= 2023) |>
    left_join(treasury_iso3, by = "country")
  
  treasury <- plyr::rbind.fill(treasury,unore) |>
    arrange(country, year)

  
  # ------------------------------
  # edit notification etc ----
  # ------------------------------  
  notification <- notification |>
    left_join(tbhiv, by = c("iso3","year","country","g_whoregion")) |>
    left_join(diag_treatment, by = c("iso3","year","country","g_whoregion"))
  
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
    mutate(
      mdr_tx = rowSums(pick(conf_mdr_tx,
                            unconf_rrmdr_tx,
                            conf_rrmdr_tx,
                            unconf_rr_nfqr_tx,
                            conf_rr_nfqr_tx,
                            conf_rr_fqr_tx,
                              rr_tx_lt12m,
                              rr_tx_gt12m_lt24m), na.rm = TRUE)) |>
    select(iso3, year, c_notified, mdr_tx) |>
    filter(year > 2000)
  
  # ------------------------------
  # Merge datasets ----
  # ------------------------------
  
  wb <- wb |>
    mutate(year = as.integer(year))
  
  finance_merged <- finance |>
    left_join(iso3_income, by = c("iso3"), suffix = c("", ".y")) |>
    # left_join(select(outcomes,-country,-g_whoregion), by = c("iso3","year"), suffix = c("", ".y")) |>
    left_join(notification, by = c("iso3","year"), suffix = c("", ".y")) |>
    select(-contains(".y"))
  
  finance_merged <- left_join(finance_merged, wb, by = c("iso3", "year"))
  finance_merged <- left_join(finance_merged, weo, by = c("iso3", "year"))
  
  finance_merged <- finance_merged |>
    left_join(hbc30, by = c("iso3")) |>
    left_join(hbmdr30, by = c("iso3")) |>
    left_join(hbtbhiv30, by = c("iso3"))
  
  # Drop and keep observations
  finance_merged <- finance_merged |>
    filter((country!="" | is.na(country)) & year >= 2013)
  
  finance_merged <- finance_merged |>
    left_join(select(treasury, iso3, year, un_pa_exch_rate), by = c("iso3", "year"))
  
  # FILLING IN MISSING NOTIFICATIONS
  # After speaking with PG we decided to not attempt to scale up notifications.
  # Instead we will use the previous year's notification
  finance_merged <- finance_merged |>
    arrange(iso3, year) |>
    mutate(c_notified = if_else(is.na(c_notified), lag(c_notified), c_notified)) %>%
    mutate(mdr_tx = if_else(year == report_year-1 & mdr_tx == 0 & lag(mdr_tx)>0, lag(mdr_tx), mdr_tx)) 
  
  
  # Percentage of TB burden for base year
  finance_merged <- finance_merged |>
    group_by(country) |>
    arrange(country, year) |>
    mutate(percent_of_cases = ifelse(year == base_year, c_notified / sum(c_notified, na.rm = TRUE) * 100, NA)) |>
    mutate(percent_of_cases = if_else(is.na(percent_of_cases), lag(percent_of_cases), percent_of_cases)) |>
    ungroup()
  
  # Zero treated same as missing
  finance_merged <- finance_merged |>
    mutate(
      rcvd_tot = if_else(rcvd_tot == 0, NA_real_, rcvd_tot),
      exp_tot = if_else(exp_tot == 0, NA_real_, exp_tot),
      cf_tot = if_else(cf_tot == 0, NA_real_, cf_tot)
    )

  
    # Save as .rda files
    save(finance_merged, file = paste0("./finance/local/finance_raw", ".rda"))
    save(treasury, file = paste0("./finance/local/treasury", ".rda"))
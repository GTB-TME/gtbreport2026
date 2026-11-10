# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch2-4.rmd
# Takuya Yamanaka, June 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load chapter 3 and 4 packages, settings and data
source(here::here('report/ch2_load_data.r'))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.1 (previously 2.4.3) ----
# (Bar chart of numbers treated for MDR-TB each year since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.4.1_data <- filter(notification, year >=2010) |>
  
  mutate(
    across(
      c( conf_rr_nfqr_tx, unconf_rr_nfqr_tx, conf_rr_fqr_tx),
      ~ replace(
        .x,
        year >= 2025 & if_any(c(rr_tx_lt12m, rr_tx_gt12m_lt24m), ~ !is.na(.x)),
        NA
      )
    )) |>

  select(year,
         rapid_dx_dr_r,
         conf_mdr,
         unconf_rrmdr_tx,
         conf_rrmdr_tx,
         unconf_rr_nfqr_tx,
         conf_rr_nfqr_tx,
         conf_rr_fqr_tx,
         rr_tx_lt12m,
         rr_tx_gt12m_lt24m,
         rrmdr_014_tx,
         rr_nfqr_014_tx) |>

  group_by(year) |>
  summarise(across(rapid_dx_dr_r:rr_nfqr_014_tx, sum, na.rm=TRUE)) |>
  ungroup() |>

  # Calculate total treated (all ages and over-15s). Set all ages to NA when we have numbers for children
  # and set adults to NA when we don't have children. That way we can have a stacked bar chart that
  # handles the early years without children and the latter with children
  mutate(rrmdr_all_tx = ifelse(rrmdr_014_tx > 0 | rr_nfqr_014_tx > 0, NA,
                               ifelse(year < 2014, rapid_dx_dr_r + conf_mdr,
                                      unconf_rrmdr_tx + conf_rrmdr_tx + unconf_rr_nfqr_tx + conf_rr_nfqr_tx + conf_rr_fqr_tx + rr_tx_lt12m + rr_tx_gt12m_lt24m))) |>
  mutate(rrmdr_15plus_tx = ifelse(rrmdr_014_tx > 0 | rr_nfqr_014_tx > 0,
                                  unconf_rrmdr_tx + conf_rrmdr_tx + unconf_rr_nfqr_tx + conf_rr_nfqr_tx + rr_tx_lt12m + rr_tx_gt12m_lt24m + conf_rr_fqr_tx - rrmdr_014_tx - rr_nfqr_014_tx,
                                  NA)) |>
  mutate(rrmdr_014_tx = ifelse(rrmdr_014_tx == 0 & rr_nfqr_014_tx == 0,
                               NA,
                               rrmdr_014_tx + rr_nfqr_014_tx)) |>

  # restrict to essential variables
  select(year,
         rrmdr_all_tx,
         rrmdr_014_tx,
         rrmdr_15plus_tx) |>

  # pivot to long format
  pivot_longer(cols = starts_with("rrmdr_"),
               names_to = "age_group",
               names_pattern = "rrmdr_(.*)_tx",
               values_to = "how_many")

f2.4.1_txt <- f2.4.1_data |>
  group_by(year) |>
  summarise(across(how_many, sum, na.rm=TRUE)) |>
  filter(year >=2019) |>
  pivot_wider(names_from = year, values_from = how_many) |>
  mutate(pct_increase_2524 = (`2025`/`2024`-1)*100,
         pct_increase_2523 = (`2025`/`2023`-1)*100,
         pct_increase_2522 = (`2025`/`2022`-1)*100
  ) |>
  rename(rr_2019=`2019`,
         rr_2020=`2020`,
         rr_2021=`2021`,
         rr_2022=`2022`,
         rr_2023=`2023`,
         rr_2024=`2024`,
         rr_2025=`2025`)


f2.4.1b_data <- filter(notification, year >=2010) |>
  
  mutate(
    across(
      c(conf_rr_nfqr_tx, unconf_rr_nfqr_tx, conf_rr_fqr_tx),
      ~ replace(
        .x,
        year >= 2025 & if_any(c(rr_tx_lt12m, rr_tx_gt12m_lt24m), ~ !is.na(.x)),
        NA
      )
    )) |>
  
  select(year,
         rapid_dx_dr_r,
         conf_mdr,
         unconf_rrmdr_tx,
         conf_rrmdr_tx,
         unconf_rr_nfqr_tx,
         conf_rr_nfqr_tx,
         conf_rr_fqr_tx,
         rr_tx_lt12m,
         rr_tx_gt12m_lt24m,
         rrmdr_014_tx,
         rr_nfqr_014_tx) |>
  
  group_by(year) |>
  summarise(across(rapid_dx_dr_r:rr_nfqr_014_tx, sum, na.rm=TRUE)) |>
  ungroup() |>
  
  # Calculate total treated (all ages and over-15s). Set all ages to NA when we have numbers for children
  # and set adults to NA when we don;t have children. That way we can have a stacked bar chart that
  # handles the early years without children and the latter with children
  mutate(rrmdr_all_tx = ifelse(year < 2014, rapid_dx_dr_r + conf_mdr,
                               unconf_rrmdr_tx + conf_rrmdr_tx + unconf_rr_nfqr_tx + conf_rr_nfqr_tx + conf_rr_fqr_tx + rr_tx_lt12m + rr_tx_gt12m_lt24m)) |>
  mutate(rrmdr_15plus_tx = ifelse(rrmdr_014_tx > 0 | rr_nfqr_014_tx > 0,
                                  unconf_rrmdr_tx + conf_rrmdr_tx + unconf_rr_nfqr_tx + conf_rr_nfqr_tx + rr_tx_lt12m + rr_tx_gt12m_lt24m + conf_rr_fqr_tx - rrmdr_014_tx - rr_nfqr_014_tx,
                                  NA)) |>
  mutate(rrmdr_014_tx = ifelse(rrmdr_014_tx == 0 & rr_nfqr_014_tx == 0,
                               NA,
                               rrmdr_014_tx + rr_nfqr_014_tx)) |>
  
  # restrict to essential variables
  select(year,
         rrmdr_all_tx,
         rrmdr_014_tx,
         rrmdr_15plus_tx) |>
  
  # pivot to long format
  pivot_longer(cols = starts_with("rrmdr_"),
               names_to = "age_group",
               names_pattern = "rrmdr_(.*)_tx",
               values_to = "how_many") |>
  filter(!is.na(how_many)) |>
  mutate(age_group = factor(age_group, levels = c("all", "15plus", "014" ),
                          labels = c("All ages", "People aged \u226515 years\nor age not reported", "People aged 0\u201314 years")))


calculate_y_limits <- function(data) {
  data |>
    group_by(age_group) |>
    summarize(y_max = max(how_many)) |>
    mutate(y_upper_limit = ifelse(y_max>10000, ceiling(y_max / 100000) * 100000, ceiling(y_max / 1000) * 1000)) |>
    select(age_group, y_upper_limit) |>
    ungroup()
}

y_limits <- calculate_y_limits(f2.4.1b_data)
f2.4.1b_data <- f2.4.1b_data |>
  left_join(y_limits, by = "age_group")

f2.4.1b_txt <- f2.4.1b_data |>
  filter(year == report_year - 6) |>
  arrange(desc(how_many)) |>
  mutate(pcnt = how_many/lag(how_many) * 100) |>
  filter(age_group == "People aged ≥15 years\nor age not reported")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.2 ----
# (Number of diagnosed with MDR/RR-TB and enrolled on treatment, compared with incident cases)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# notification
f2.4.2_data <- filter(notification, year >= 2015 ) |>
  
  mutate(
    across(
      c(conf_rr_nfqr_tx, unconf_rr_nfqr_tx, conf_rr_fqr_tx),
      ~ replace(
        .x,
        year >= 2025 & if_any(c(rr_tx_lt12m, rr_tx_gt12m_lt24m), ~ !is.na(.x)),
        NA
      )
    )) |>
  
  select(country,
         g_whoregion,
         iso3,
         year,
         rapid_dx_dr_r,
         conf_mdr,
         conf_rrmdr,
         conf_rr_nfqr,
         conf_rr_fqr,
         conf_mdr_tx,
         unconf_mdr_tx,
         conf_rrmdr_tx,
         unconf_rrmdr_tx,
         conf_rr_nfqr_tx,
         rr_tx_lt12m,
         rr_tx_gt12m_lt24m,
         unconf_rr_nfqr_tx,
         conf_rr_fqr_tx) |>

  rowwise() |>
  # Derive total number detected and total enrolled on treatment
  mutate(rr_detected = ifelse(year < 2014,
                              sum(across(rapid_dx_dr_r:conf_mdr), na.rm = TRUE),
                              # the next three are mutually exclusive so can be added
                              sum(across(conf_rrmdr:conf_rr_fqr), na.rm = TRUE)),
         # treatment variables are in mutually exclusive sets so again can be added
         rr_treated = sum(across(conf_mdr_tx:conf_rr_fqr_tx), na.rm = TRUE)) |>

  # calculate regional aggregates
  group_by(g_whoregion,year) |>
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) |>

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  arrange(entity) |>
  select(-g_whoregion) |>

  ungroup() |>

  # drop unneeded variables
  select(entity,
         year,
         rr_detected,
         rr_treated)

# Add global summary to the regional summary
f2.4.2_global <- f2.4.2_data |>
  group_by(year) |>
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) |>
  mutate(entity="Global") |>
  ungroup()

f2.4.2_data <- rbind(f2.4.2_data, f2.4.2_global)

if(show_estimates){
  
f2.4.2_drest <- est_dr_group |>
  mutate(g_whoregion=group_name) |>
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  arrange(entity) |>
  select(-g_whoregion) |>
  select(entity,
         year,
         e_inc_rr_num,
         e_inc_rr_num_lo,
         e_inc_rr_num_hi)

# Add global summary to the regional summary
f2.4.2_drest_global <- est_dr_group |>
  mutate(entity=group_name) |>
  subset(entity=="global") |>
  mutate(entity="Global") |>
  select(entity,
         year,
         e_inc_rr_num,
         e_inc_rr_num_lo,
         e_inc_rr_num_hi)

f2.4.2_drest <- rbind(f2.4.2_drest, f2.4.2_drest_global)

# merge notification and estimates
f2.4.2_data <- left_join(f2.4.2_data,f2.4.2_drest, by=c("entity","year"))


f2.4.2_data <- f2.4.2_data |>
  mutate(entity = factor(entity,
                         levels = c("Global","African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))

f2.4.2_txt <- f2.4.2_data |>
  filter(entity == "Global" & year == report_year-1)

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.3 ----
# (Panel of line charts of MDR/RR-TB cases detected, MDR/RR-TB put on treatment
# since 2009 for 30 countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.4.3_data <- filter(notification, year >= 2015 ) |>
  select(country,
         g_whoregion,
         iso3,
         year,
         rapid_dx_dr_r,
         conf_mdr,
         conf_rrmdr,
         conf_rr_nfqr,
         conf_rr_fqr,
         conf_mdr_tx,
         unconf_mdr_tx,
         conf_rrmdr_tx,
         unconf_rrmdr_tx,
         conf_rr_nfqr_tx,
         rr_tx_lt12m,
         rr_tx_gt12m_lt24m,
         unconf_rr_nfqr_tx,
         conf_rr_fqr_tx) |>
  
  rowwise() |>
  # Derive total number detected and total enrolled on treatment
  mutate(
    rr_detected = if_else(
      year < 2014,
      
      {
        x <- pick(rapid_dx_dr_r:conf_mdr)
        ifelse(
          rowSums(!is.na(x)) == 0,
          NA_real_,
          rowSums(x, na.rm = TRUE)
        )
      },
      
      {
        x <- pick(conf_rrmdr:conf_rr_fqr)
        ifelse(
          rowSums(!is.na(x)) == 0,
          NA_real_,
          rowSums(x, na.rm = TRUE)
        )
      }
    ),
    
    rr_treated = {
      x <- pick(conf_mdr_tx:conf_rr_fqr_tx)
      ifelse(
        rowSums(!is.na(x)) == 0,
        NA_real_,
        rowSums(x, na.rm = TRUE)
      )
    }
  )|>
  arrange(iso3) |>
  # drop unneeded variables
  select(entity = country,
         iso3,
         year,
         rr_detected,
         rr_treated)

if(show_estimates){
  
  f2.4.3_drest_country <- est_dr_country |>
    select(iso3,
           year,
           e_inc_rr_num,
           e_inc_rr_num_lo,
           e_inc_rr_num_hi) |>
    # In 2026, IND data needed to be removed
    mutate(e_inc_rr_num = if_else(iso3 == "IND", NA, e_inc_rr_num),
           e_inc_rr_num_lo = if_else(iso3 == "IND", NA, e_inc_rr_num_lo),
           e_inc_rr_num_hi = if_else(iso3 == "IND", NA, e_inc_rr_num_hi))
    
  
  # merge notification and estimates
  f2.4.3_data <- left_join(f2.4.3_data,f2.4.3_drest_country, by=c("iso3","year")) |>
    filter(iso3 %in% hbmdr30$iso3)
  
  f2.4.3_data <-  f2.4.3_data  |> 
    mutate(entity = ifelse(entity == "Democratic People's Republic of Korea", "Democratic People's\nRepublic of Korea",
                            ifelse(entity == "Democratic Republic of the Congo", "Democratic Republic\nof the Congo",
                                   entity
                            )))
  
}


f2.4.3_txt <- f2.4.3_data |>
  filter(year == report_year-1 & rr_treated>e_inc_rr_num)  |> 
  inner_join(en_name, by = "iso3") 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.4  ----
# (Bubble map of difference between MDR/RR-TB patients starting treatment and
#  estimated MDR/RR-TB incidence for 10 countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if(show_estimates){

  f2.4.4_data  <- filter(est_dr_country, year == report_year - 1) |>
    select(iso3,
           year,
           e_inc_rr_num) |>

    # Link to notifications
    inner_join(notification, by = c("iso3", "year")) |>
    select(iso3,
           country,
           e_inc_rr_num,
           conf_rr_nfqr_tx,
           unconf_rr_nfqr_tx,
           conf_rr_fqr_tx,
           rr_tx_lt12m,
           rr_tx_gt12m_lt24m) |>

    # Filter out countries that have not reported anything yet for the latest year
    filter(!is.na(conf_rr_nfqr_tx) | !is.na(unconf_rr_nfqr_tx) | !is.na(conf_rr_fqr_tx) | !is.na(rr_tx_lt12m) | !is.na(rr_tx_gt12m_lt24m) ) |>

    # Calculate the gap and use that for the bubble sizes
    mutate(size = e_inc_rr_num - (NZ(conf_rr_nfqr_tx) + NZ(unconf_rr_nfqr_tx) + NZ(conf_rr_fqr_tx) + NZ(rr_tx_lt12m) + NZ(rr_tx_gt12m_lt24m))) |>

    # limit to the top 10 by size of gap
    top_n(10, size) |>

    # sort in descending order so can list the country names in the figure footnote
    arrange(desc(size))

  # Summary number of gaps for the section text
  # Get global incidence
  f2.4.4_txt <- filter(est_dr_group, year == report_year-1 & group_type == "global") |>
    select(e_inc_rr_num)

  # Add global enrolments
  f2.4.4_txt <- filter(notification, year == report_year-1) |>
    select(year,
           conf_rr_nfqr_tx,
           unconf_rr_nfqr_tx,
           conf_rr_fqr_tx,
           rr_tx_lt12m,
           rr_tx_gt12m_lt24m) |>
    # calculate global aggregate
    group_by(year) |>
    summarise(across(conf_rr_nfqr_tx:rr_tx_gt12m_lt24m, sum, na.rm=TRUE)) |>
    ungroup() |>
    mutate(enrolled = conf_rr_nfqr_tx + unconf_rr_nfqr_tx + conf_rr_fqr_tx + rr_tx_lt12m + rr_tx_gt12m_lt24m) |>
    cbind(f2.4.4_txt) |>

    # Calculate the global gap and drop the other variables
    mutate(gap = e_inc_rr_num - enrolled) |>
    select(gap)

  # Calculate % of global gap contributed by the top 10 countries
  f2.4.4_txt <- f2.4.4_data |>
    summarise(gap_top_ten = sum(size)) |>
    cbind(f2.4.4_txt) |>
    mutate(pct_gap_top_ten = gap_top_ten * 100 / gap)
  
  f2.4.4_data <- f2.4.4_data |>
    mutate(pct_gap = size / f2.4.4_txt$gap *100) |> 
    inner_join(en_name, by = "iso3") 

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.5  ----
# (horizontal bar chart showing TB treatment outcomes globally by year since 2012 for MDR/RR-TB)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f2.4.5_data <- outcomes |>

  filter(between(year, 2012, report_year - 3)) |>
  select(iso3,
         year,
         contains("mdr_"),
         starts_with("rr_tx_")
  ) |>
  rowwise() |>
  mutate(mdr_succ = sum(across(contains("succ")), na.rm = T),
         mdr_fail = sum(across(contains("fail")), na.rm = T),
         mdr_lost = sum(across(matches("lost|Lost")), na.rm = T),
         mdr_died = sum(across(matches("died|Died")), na.rm = T),
         mdr_neval = sum(across(contains("neval")), na.rm = T)) |>
  
  select(iso3, year,
         mdr_succ,
         mdr_fail,
         mdr_lost,
         mdr_died,
         mdr_neval) |>

  mutate(mdr_coh = rowSums(pick(contains("mdr_succ"),contains("mdr_fail"),contains("mdr_died"),contains("mdr_lost"),contains("mdr_neval")), na.rm = TRUE)) |>
  
  # calculate global aggregates
  group_by(year) |>
  summarise(across(mdr_succ:mdr_coh, sum, na.rm=TRUE)) |>
  ungroup() |>

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("mdr_") |>

  # Drop the actual numbers and keep percentages, plu sother unwanted variables
  select(-coh, -succ, -fail, -died, -lost, -neval) |>

  # Add tx group type
  mutate(subgroup = "MDR/RR-TB cases") |>

  # flip into long format
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome")

# Summary for section text
f2.4.5_txt <- filter(f2.4.5_data, year %in% c(2012, report_year-5, report_year-4,  report_year-3) & outcome=="Treatment success") |>
  select(-outcome) |>
  pivot_wider(names_from = year,
              names_prefix = "c_tsr_",
              values_from = value)


# high MDR-TB burden countries
f2.4.5b_data <- outcomes |>
  
  filter(between(year, 2012, report_year - 3), iso3 %in% iso3_hmdrc) |>
  select(iso3,
         year,
         country,
         contains("mdr_"),
         starts_with("rr_tx_")
  ) |>
  rowwise() |>
  mutate(mdr_succ = sum(across(contains("succ")), na.rm = T),
         mdr_fail = sum(across(contains("fail")), na.rm = T),
         mdr_lost = sum(across(matches("lost|Lost")), na.rm = T),
         mdr_died = sum(across(matches("died|Died")), na.rm = T),
         mdr_neval = sum(across(contains("neval")), na.rm = T)) |>
  
  select(iso3, year, country,
         mdr_succ,
         mdr_fail,
         mdr_lost,
         mdr_died,
         mdr_neval) |>
  
  mutate(mdr_coh = rowSums(pick(contains("mdr_succ"),contains("mdr_fail"),contains("mdr_died"),contains("mdr_lost"),contains("mdr_neval")), na.rm = TRUE)) |>
  
  # calculate global aggregates
  group_by(year,country) |>
  summarise(across(mdr_succ:mdr_coh, sum, na.rm=TRUE)) |>
  ungroup() |>
  
  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("mdr_") |>
  
  # Drop the actual numbers and keep percentages, plu sother unwanted variables
  select(-coh, -succ, -fail, -died, -lost, -neval) |>
  
  # Add tx group type
  mutate(subgroup = "MDR/RR-TB cases") |>
  
  # flip into long format
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.6 ----
# (Horizontal bar chart showing TB treatment outcomes in MDR/RR-TB cases for WHO regions and globally")
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Code similar to others so maybe make a reusable function?

# A. Regional aggregates
# - - - - - - - - - - - -
txout_region  <- filter(outcomes, year==report_year - 3) |>

  select(iso3,
         year,
         g_whoregion,
         contains("mdr_"),
         starts_with("rr_tx_")
  ) |>
  rowwise() |>
  mutate(mdr_succ = sum(across(contains("succ")), na.rm = T),
         mdr_fail = sum(across(contains("fail")), na.rm = T),
         mdr_lost = sum(across(matches("lost|Lost")), na.rm = T),
         mdr_died = sum(across(matches("died|Died")), na.rm = T),
         mdr_neval = sum(across(contains("neval")), na.rm = T)) |>
  
  select(iso3, year, g_whoregion,
         mdr_succ,
         mdr_fail,
         mdr_lost,
         mdr_died,
         mdr_neval) |>
  
  mutate(mdr_coh = rowSums(pick(contains("mdr_succ"),contains("mdr_fail"),contains("mdr_died"),contains("mdr_lost"),contains("mdr_neval")), na.rm = TRUE)) |>

  group_by(g_whoregion) |>
  summarise(across(mdr_succ:mdr_coh, sum, na.rm=TRUE)) |>
  ungroup() |>

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(-g_whoregion) |>

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("mdr_") |>

  # Sort regions in descending order of success rate
  arrange(desc(`Treatment success`))


# B. Global aggregates
# - - - - - - - - - - - -
txout_global  <- filter(outcomes, year==report_year - 3) |>

  select(iso3,
         contains("mdr_"),
         starts_with("rr_tx_")
  ) |>
  rowwise() |>
  mutate(mdr_succ = sum(across(contains("succ")), na.rm = T),
         mdr_fail = sum(across(contains("fail")), na.rm = T),
         mdr_lost = sum(across(matches("lost|Lost")), na.rm = T),
         mdr_died = sum(across(matches("died|Died")), na.rm = T),
         mdr_neval = sum(across(contains("neval")), na.rm = T)) |>
  
  select(iso3,
         mdr_succ,
         mdr_fail,
         mdr_lost,
         mdr_died,
         mdr_neval) |>
  
  mutate(mdr_coh = rowSums(pick(contains("mdr_succ"),contains("mdr_fail"),contains("mdr_died"),contains("mdr_lost"),contains("mdr_neval")), na.rm = TRUE)) |>
  
  ungroup()  |>
  summarise(across(mdr_succ:mdr_coh, sum, na.rm=TRUE)) |>
  ungroup()  |>
  mutate(entity="Global")  |>

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("mdr_")


# Create a dummy record a gap in the output to separate countries, regions and global parts
txout_dummy <- data.frame(entity = " ", coh = NA, succ = NA, fail = NA,
                          died = NA, lost = NA, neval = NA,
                           Died = NA)

# Had to use mutate to create the next 3 fields because data.frame converted spaces to dots. Grrr
txout_dummy <- txout_dummy |>
  mutate(`Treatment success` = NA,
         `Treatment failed` = NA,
         `Lost to follow-up` = NA,
         `Not evaluated` = NA)


# Create combined table in order of countries then regional and global estimates
f2.4.6_data <- rbind(txout_region, txout_dummy, txout_global) |>

  # Keep record of current order (in reverse) so plot comes out as we want it
  mutate(entity = factor(entity, levels=rev(entity))) |>

  # Drop the actual numbers and keep percentages
  select(-coh, -succ, -fail, -died, -lost, -neval) |>

  # Flip into long mode for stacked bar plotting
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome")

# Summary for section text
f2.4.6_txt <- filter(txout_region, entity %in% c("African Region", "European Region")) |>
  select(entity,
         c_tsr = `Treatment success`) |>
  pivot_wider(names_from = entity,
              names_prefix = "c_tsr_",
              values_from = c_tsr) |>
  # deal with spaces in variable names
  select(c_tsr_AFR = `c_tsr_African Region`,
         c_tsr_EUR = `c_tsr_European Region`)

# tidy up
rm(list = ls(pattern = "^txout_"))

f2.4.6_txt2 <- filter(f2.4.6_data, outcome == "Treatment success" & entity!="Global") |>
  arrange(desc(value)) |>
  slice(1,6) |>
  select(entity,value) |>
  pivot_wider(names_from = entity,
              values_from = entity:value) |>
  rename(hi_region = 1, lo_region = 2, hi_region_value = 3, lo_region_value = 4)
  


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.7  ----
# (World map showing which countries used all-oral 6-month (BPaLM/BPaL 6-9 months) treatment regimens)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.4.7_data <- filter(diag_treatment, year == report_year-1) |>
  select(country,
         iso3,
         dr_6m_used) |>
  
  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(dr_6m_used = ifelse(dr_6m_used==3, NA, dr_6m_used)) |>
  
  
  # Assign the categories for the map
  mutate(var = factor(dr_6m_used,
                      levels = c(1, 0),
                      labels = c("Used", "Not used")))


# number of people treated with 6-month regimens
f2.4.7_txt <- filter(diag_treatment, year >= 2022) |>
  
  select(country,
         iso3,
         year,
         mdrxdr_bpalm_tx,
         dr_6m_tx) |>
  mutate(dr_6m_tx = ifelse(year<=2023, mdrxdr_bpalm_tx, dr_6m_tx)) |>
  group_by(year) |>
  summarise(across(dr_6m_tx, sum, na.rm=TRUE))  |>
  ungroup() |>
  pivot_wider(names_from = year,
              names_prefix = "dr_6m_tx_",
              values_from = dr_6m_tx)

# number of countries using 6-month regimens 
f2.4.7_txt2 <-  filter(diag_treatment, year %in% c(2020:report_year-1)) |>
  
  select(country,
         iso3,
         year,
         mdrxdr_bpalm_used,
         dr_6m_used) |>
  
  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(mdrxdr_bpalm_used = ifelse(mdrxdr_bpalm_used==3, NA, mdrxdr_bpalm_used),
         dr_6m_used = ifelse(dr_6m_used==3, NA, dr_6m_used),
         dr_6m_used = ifelse(year <= 2023, mdrxdr_bpalm_used, dr_6m_used)) |>
  
  group_by(year) |>
  summarise(across(dr_6m_used, sum, na.rm=TRUE)) |>
  ungroup() |>
  
  pivot_wider(names_from = year,
              names_prefix = "dr_6m_used_",
              values_from = dr_6m_used)

f2.4.7_check1 <- filter(diag_treatment, year >= 2020) |>
  select(country,
         iso3,
         year,         
         mdrxdr_bpalm_used,
         dr_6m_used) |>
  mutate(mdrxdr_bpalm_used = ifelse(mdrxdr_bpalm_used==3, NA, mdrxdr_bpalm_used),
         dr_6m_used = ifelse(dr_6m_used==3, NA, dr_6m_used),
         dr_6m_used = ifelse(year <= 2023, mdrxdr_bpalm_used, dr_6m_used)) |>
  select(!mdrxdr_bpalm_used) |>
  pivot_wider(names_from = year, values_from = dr_6m_used) |>
  filter((`2024`==1 & `2025`!=1))

f2.4.7_check2 <- filter(f2.4.7_data, dr_6m_used==1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.8  ----
# (World map showing which countries used shorter MDR-TB treatment regimens)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.4.8_data <- filter(diag_treatment, year == report_year-1) |>
  select(country,
         iso3,
         dr_9m_used) |>

  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(dr_9m_used = ifelse(dr_9m_used==3, NA, dr_9m_used)) |>

  # Assign the categories for the map
  mutate(var = factor(dr_9m_used,
                      levels = c(1, 0),
                      labels = c("Used", "Not used")))


f2.4.8_txt <-  filter(diag_treatment, year %in% c(2020:report_year-1)) |>

  select(country,
         iso3,
         year,
         mdr_alloral_short_used,
         dr_9m_used) |>

  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(mdr_alloral_short_used = ifelse(mdr_alloral_short_used==3, NA, mdr_alloral_short_used),
         dr_9m_used = ifelse(dr_9m_used==3, NA, dr_9m_used)) |>
  mutate(dr_9m_used = ifelse(year<=2023, mdr_alloral_short_used, dr_9m_used)) |>
           
  group_by(year) |>
  summarise(across(dr_9m_used, sum, na.rm=TRUE)) |>
  ungroup() |>

  pivot_wider(names_from = year,
              names_prefix = "dr_9m_used_",
              values_from = dr_9m_used)

f2.4.8_check <- filter(diag_treatment, year >= 2020) |>
  select(country,
         iso3,
         year,
         mdr_alloral_short_used,
         dr_9m_used) |>
  mutate(mdr_alloral_short_used = ifelse(mdr_alloral_short_used==3, NA, mdr_alloral_short_used),
         dr_9m_used = ifelse(dr_9m_used==3, NA, dr_9m_used)) |>
  mutate(dr_9m_used = ifelse(year<=2023, mdr_alloral_short_used, dr_9m_used)) |>
  select(!mdr_alloral_short_used) |>
  pivot_wider(names_from = year, values_from = dr_9m_used) |>
  filter((`2024`==1 & `2025`!=1))

knitr::combine_words(f2.4.8_check$country, oxford_comma=FALSE)
f2.4.8_check |>
  filter(iso3 %in% f2.4.7_check2$iso3)

f2.4.7_8_txt <- filter(f2.4.8_data, dr_9m_used == 1) |>
  summarise(dr_9m_used = n()) 

f2.4.7_8_txt <- filter(f2.4.7_data, dr_6m_used == 1) |>
  summarise(dr_6m_used = n()) |>
  cbind(f2.4.7_8_txt)



f2.4.8_check2 <- filter(notification, year >= 2022) |>
  
  select(country,
         iso3,
         year,
         mdrxdr_bpalm_tx,
         dr_6m_tx) |>
  mutate(dr_6m_tx = ifelse(year<=2023, mdrxdr_bpalm_tx, dr_6m_tx)) |>
  group_by(year) |>
  filter(iso3 %in% f2.4.8_check$iso3) |>
  select(!mdrxdr_bpalm_tx) |>
  pivot_wider(names_from = year, values_from = dr_6m_tx) 

f2.4.8_check3 <- filter(notification, year >= 2022) |>
  
  select(country,
         iso3,
         year,
         mdr_alloral_short_tx,
         dr_9m_tx) |>
  mutate(dr_9m_tx = ifelse(year<=2023, mdr_alloral_short_tx, dr_9m_tx)) |>
  group_by(year) |>
  filter(iso3 %in% f2.4.8_check$iso3) |>
  select(!mdr_alloral_short_tx) |>
  pivot_wider(names_from = year, values_from = dr_9m_tx) 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.9 
# proportion of people treated with each MDR/RR-TB regimens - globally
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

mdr_regimen_country <- filter(diag_treatment, year > 2018) |>
  select(country,
         iso3,
         year,
         mdrxdr_bpalm_used,
         dr_6m_used,
         mdr_alloral_short_used,
         dr_9m_used,
         mdrxdr_alloral_used,
         dr_18mplus_used) |>
  
  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(mdrxdr_bpalm_used = ifelse(mdrxdr_bpalm_used==3, NA, mdrxdr_bpalm_used),
         mdr_alloral_short_used = ifelse(mdr_alloral_short_used==3, NA, mdr_alloral_short_used),
         mdrxdr_alloral_used = ifelse(mdrxdr_alloral_used==3, NA, mdrxdr_alloral_used),
         dr_6m_used = ifelse(dr_6m_used==3, NA, dr_6m_used),
         dr_9m_used = ifelse(dr_9m_used==3, NA, dr_9m_used),
         dr_18mplus_used = ifelse(dr_18mplus_used==3, NA, dr_18mplus_used)
  ) |>
  mutate(dr_6m_used = ifelse(year<=2023, mdrxdr_bpalm_used, dr_6m_used),
         dr_9m_used = ifelse(year<=2023, mdr_alloral_short_used, dr_9m_used),
         dr_18mplus_used = ifelse(year<=2023, mdrxdr_alloral_used, dr_18mplus_used)
         ) |>
  
  select(!contains("mdr")) |>
  group_by(year) |>
  summarise(across(dr_6m_used:dr_18mplus_used, sum, na.rm=TRUE)) |>
  ungroup() |>
  pivot_longer(cols = starts_with("dr"), names_to = "regimen") |>
  mutate(country = "global")


f2.4.2_treated_data <- f2.4.2_data |>
  select(entity, year, rr_treated) 


f2.4.9_global_data <- filter(diag_treatment, year > 2018) |>
  select(country,
         iso3,
         year,
         mdrxdr_bpalm_tx,
         mdr_alloral_short_tx,
         mdrxdr_alloral_tx,
         dr_6m_tx,
         dr_9m_tx,
         dr_18mplus_tx)  |>
  
  mutate(dr_6m_tx = ifelse(year<=2023, mdrxdr_bpalm_tx, dr_6m_tx),
         dr_9m_tx = ifelse(year<=2023, mdr_alloral_short_tx, dr_9m_tx),
         dr_18mplus_tx = ifelse(year<=2023, mdrxdr_alloral_tx, dr_18mplus_tx)
  ) |>
  
  mutate(dr_9m_tx = ifelse(iso3=="UGA"&year==2024, 612, dr_9m_tx)) |> # TEMPORAL HOT FIX !!
  
  select(!contains("mdr")) |>
  
  group_by(year) |>
  summarise(across(dr_6m_tx:dr_18mplus_tx, sum, na.rm=TRUE)) |>
  ungroup() |>
  mutate(entity = "Global") |>
  left_join(f2.4.2_treated_data, by = c("year","entity")) |>
  mutate(total_dr_tx = NZ(dr_6m_tx)+NZ(dr_9m_tx)+NZ(dr_18mplus_tx),
         dr_unk_tx = if_else(rr_treated > total_dr_tx, rr_treated-(NZ(dr_6m_tx)+NZ(dr_9m_tx)+NZ(dr_18mplus_tx)), total_dr_tx-(NZ(dr_6m_tx)+NZ(dr_9m_tx)+NZ(dr_18mplus_tx)))) |>
  pivot_longer(cols = starts_with("dr"), names_to = "regimen") 

 
f2.4.9_region_data <- filter(diag_treatment, year > 2018) |>
  select(iso3,
         year,
         g_whoregion,
         mdrxdr_bpalm_tx,
         mdr_alloral_short_tx,
         mdrxdr_alloral_tx,
         dr_6m_tx,
         dr_9m_tx,
         dr_18mplus_tx)  |>
  
  mutate(dr_6m_tx = ifelse(year<=2023, mdrxdr_bpalm_tx, dr_6m_tx),
         dr_9m_tx = ifelse(year<=2023, mdr_alloral_short_tx, dr_9m_tx),
         dr_18mplus_tx = ifelse(year<=2023, mdrxdr_alloral_tx, dr_18mplus_tx)
  ) |>
  
  select(!contains("mdr")) |>
  
  group_by(year,g_whoregion) |>
  summarise(across(dr_6m_tx:dr_18mplus_tx, sum, na.rm=TRUE)) |>
  ungroup() |>
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  left_join(f2.4.2_treated_data, by = c("year","entity")) |>
  mutate(total_dr_tx = NZ(dr_6m_tx)+NZ(dr_9m_tx)+NZ(dr_18mplus_tx),
         dr_unk_tx = if_else(rr_treated > total_dr_tx, rr_treated-(NZ(dr_6m_tx)+NZ(dr_9m_tx)+NZ(dr_18mplus_tx)), total_dr_tx-(NZ(dr_6m_tx)+NZ(dr_9m_tx)+NZ(dr_18mplus_tx)))) |>
  pivot_longer(cols = starts_with("dr"), names_to = "regimen") |>
  select(!g_whoregion)

f2.4.9_data <- rbind.data.frame(f2.4.9_global_data, f2.4.9_region_data) 


f2.4.9_data <- f2.4.9_data |>
  mutate(pcnt = if_else(rr_treated > total_dr_tx, value / rr_treated * 100, value / total_dr_tx * 100)) |>
  select(year, entity, regimen, pcnt)

f2.4.9_data <- f2.4.9_data |>
  mutate(pcnt = ifelse((regimen=="mdr_alloral_short_tx"&year==2019)|(regimen=="mdrxdr_bpalm_tx"&year<2022),NA,pcnt)) |>
  mutate(entity = factor(entity,
                         levels = c("Global","African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))

f2.4.9_txt <- f2.4.9_data |>
  filter(year == report_year-1 & entity == "Global") |>
  select(regimen, pcnt) |>
  pivot_wider(names_from = regimen,
              values_from = pcnt)

f2.4.9_9month_txt <- f2.4.9_data |>
  filter(year == report_year-1 & entity != "Global" & regimen == "dr_9m_tx") |>
  slice(1)

f2.4.9_6month_txt <- f2.4.9_data |>
  filter(year == report_year-1 & entity != "Global" & regimen == "dr_6m_tx") |>
  arrange(desc(pcnt)) |>
  slice(1:2) |>
  arrange(entity)

f2.4.9_EUR_txt <- f2.4.9_data |>
  filter(year == report_year-1 & entity == "European Region" & regimen == "dr_18mplus_tx") 

# (c) proportion of people treated with each MDR/RR-TB regimens - 30 high MDR/RR-TB burden countries
f2.4.3_country_data <- f2.4.3_data |>
  select(iso3, year, rr_treated)

f2.4.10_data <- filter(diag_treatment, year > 2018) |>
  select(country,
         iso3,
         year,
         mdrxdr_bpalm_tx,
         mdr_alloral_short_tx,
         mdrxdr_alloral_tx,
         dr_6m_tx,
         dr_9m_tx,
         dr_18mplus_tx)  |>
  
  mutate(dr_6m_tx = ifelse(year<=2023, mdrxdr_bpalm_tx, dr_6m_tx),
         dr_9m_tx = ifelse(year<=2023, mdr_alloral_short_tx, dr_9m_tx),
         dr_18mplus_tx = ifelse(year<=2023, mdrxdr_alloral_tx, dr_18mplus_tx)
  ) |>
  select(!contains("mdr")) |>
  
  filter(iso3 %in% hbmdr30$iso3) |>
  # group_by(country, iso3, year) |>
  # summarise(across(mdrxdr_bpalm_tx:mdrxdr_alloral_tx, sum, na.rm=TRUE)) |>
  # ungroup() |>
  left_join(f2.4.3_country_data, by = c("year","iso3")) |>
  mutate(total_dr_tx = NZ(dr_6m_tx)+NZ(dr_9m_tx)+NZ(dr_18mplus_tx),
         dr_unk_tx = if_else(rr_treated > total_dr_tx, rr_treated-(NZ(dr_6m_tx)+NZ(dr_9m_tx)+NZ(dr_18mplus_tx)), total_dr_tx-(NZ(dr_6m_tx)+NZ(dr_9m_tx)+NZ(dr_18mplus_tx)))) |>
  pivot_longer(cols = starts_with("dr"), names_to = "regimen")


f2.4.10_data <- f2.4.10_data |>
  mutate(pcnt = if_else(rr_treated > total_dr_tx, value / rr_treated * 100, value / total_dr_tx * 100)) |>
  select(iso3, country, year, regimen, pcnt) 

f2.4.10_data <-  f2.4.10_data  |> 
  mutate(country = ifelse(country == "Democratic People's Republic of Korea", "Democratic People's\nRepublic of Korea",
                          ifelse(country == "Democratic Republic of the Congo", "Democratic Republic\nof the Congo",
                                 country
                          )))


f2.4.10_9month_txt <- f2.4.10_data |>
  filter(year == report_year-1 & pcnt>50 & regimen == "dr_9m_tx") |>
  arrange(country)

f2.4.10_18mplus_txt <- f2.4.10_data |>
  filter(year == report_year-1 & pcnt>50 & regimen == "dr_18mplus_tx") |>
  arrange(country) |> 
  inner_join(en_name, by = "iso3") 


f2.4.10_6month_txt <- f2.4.10_data |>
  filter(year == report_year-1 & pcnt>40 & regimen == "dr_6m_tx") |>
  arrange(country) |> 
  inner_join(en_name, by = "iso3") 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.11  ----
# (World map showing proportion of drug-resistant TB patients followed up for adverse events)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.4.11_data <- filter(notification, year==report_year-1) |>

  select(country,
         iso3,
         unconf_rr_nfqr_tx,
         conf_rr_nfqr_tx,
         conf_rr_fqr_tx,
         rr_tx_lt12m,
         rr_tx_gt12m_lt24m,
         mdr_tx_adsm) |>

  # Calculate percent with active follow up
  mutate(adsm_pct = ifelse(NZ(unconf_rr_nfqr_tx) + NZ(conf_rr_nfqr_tx) + NZ(conf_rr_fqr_tx)+ NZ(rr_tx_gt12m_lt24m)+ NZ(rr_tx_lt12m) > 0,
                           mdr_tx_adsm * 100 / (NZ(conf_rr_nfqr_tx) + NZ(conf_rr_nfqr_tx) + NZ(conf_rr_fqr_tx) + NZ(rr_tx_gt12m_lt24m)+ NZ(rr_tx_lt12m)),
                           NA)) |>

  # Assign the categories for the map
  mutate(var = cut(adsm_pct,
                   c(0, 25, 50, 75, Inf),
                   c('0\u201324', '25\u201349', '50\u201374', '\u226575'),
                   right=FALSE)) |>

  # get rid of extra variables
  select(country,
         iso3,
         adsm_pct,
         var)



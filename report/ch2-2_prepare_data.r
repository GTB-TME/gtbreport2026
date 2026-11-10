# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch2-2.rmd
# Takuya Yamanaka, September 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load chapter 2 packages, settings and data
source(here::here('report/ch2_load_data.r'))

# dr_surveillance <- dr_surveillance |>
#   mutate(rr_dst_new = if_else(iso3 == "PAK" & year == 2019, 2946, rr_dst_new),
#          rr_dst_new = if_else(iso3 == "PAK" & year == 2020, 2302, rr_dst_new),
#          rr_dst_ret = if_else(iso3 == "PAK" & year == 2019, 874, rr_dst_ret),
#          rr_dst_ret = if_else(iso3 == "PAK" & year == 2020, 387, rr_dst_ret)) 
# 
# notification <- notification |>
#   mutate(newinc_rdx = if_else(iso3 == "ZAF" & year == 2023 & is.na(newinc_rdx), 176386, newinc_rdx))
# 
# notification <- notification |>
#   mutate(newinc_rdx = if_else(iso3 == "CHN" & year == 2025 & newinc_rdx != 479319, 479319, newinc_rdx))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.1 ----
# (Diagnostic cascade)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
drs_data <- dr_surveillance |>
  filter(year == report_year-1) |>
  select(iso3,
         g_whoregion,
         r_rlt_dst_new, r_rlt_dst_ret,
         dst_rlt_new, dst_rlt_ret,
         rr_dst_new, rr_dst_ret, rr_dst_unk,
         rr_dst_rlt_fq,
         rr_fqr,
         rr_fqr_dst_bdqr_lzdr,rr_fqr_dst_bdqs_lzdr,rr_fqr_dst_bdqu_lzdr,
         rr_fqr_dst_bdqr_lzds,rr_fqr_dst_bdqs_lzds,rr_fqr_dst_bdqu_lzds,
         rr_fqr_dst_bdqr_lzdu,rr_fqr_dst_bdqs_lzdu,rr_fqr_dst_bdqu_lzdu) |>
  
  # Calculate percentage RR cases with 2nd line DST
  mutate(rrdst = (NZ(r_rlt_dst_new) + NZ(r_rlt_dst_ret)),
         indst = (NZ(dst_rlt_new) + NZ(dst_rlt_ret)),
         rrpos = (NZ(rr_dst_new) + NZ(rr_dst_ret) + NZ(rr_dst_unk)),
         fqdst = NZ(rr_dst_rlt_fq),
         prxdr = NZ(rr_fqr),
         bddst = (NZ(rr_fqr_dst_bdqr_lzdr)+NZ(rr_fqr_dst_bdqs_lzdr)+NZ(rr_fqr_dst_bdqr_lzds)+NZ(rr_fqr_dst_bdqs_lzds)+NZ(rr_fqr_dst_bdqr_lzdu)+NZ(rr_fqr_dst_bdqs_lzdu)),
         lzdst = (NZ(rr_fqr_dst_bdqr_lzdr)+NZ(rr_fqr_dst_bdqs_lzdr)+NZ(rr_fqr_dst_bdqr_lzds)+NZ(rr_fqr_dst_bdqs_lzds)+NZ(rr_fqr_dst_bdqu_lzdr)+NZ(rr_fqr_dst_bdqu_lzds)),
         bdlzt = (NZ(rr_fqr_dst_bdqr_lzdr)+NZ(rr_fqr_dst_bdqs_lzdr)+NZ(rr_fqr_dst_bdqr_lzds)+NZ(rr_fqr_dst_bdqs_lzds)+NZ(rr_fqr_dst_bdqr_lzdu)+NZ(rr_fqr_dst_bdqs_lzdu)+NZ(rr_fqr_dst_bdqu_lzdr)+NZ(rr_fqr_dst_bdqu_lzds)),
         xdr   = (NZ(rr_fqr_dst_bdqr_lzdr)+NZ(rr_fqr_dst_bdqr_lzds)+NZ(rr_fqr_dst_bdqr_lzdu)+NZ(rr_fqr_dst_bdqs_lzdr)+NZ(rr_fqr_dst_bdqu_lzdr))) |>
  arrange(iso3)

wrd_data <- notification |>
  filter(year == report_year-1) |>
  select(iso3, g_whoregion,
         c_newinc,
         new_labconf,ret_rel_labconf,new_clindx,ret_rel_clindx,
         rdx_data_available,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx#,
         # rdxsurvey_newinc,
         # rdxsurvey_newinc_rdx
         ) |>
  rowwise() |>
  mutate(new_ptb = sum(across(new_labconf:ret_rel_clindx), na.rm = T)) |>
  # rowwise() |>
  # mutate(newinc_rdx = ifelse(rdx_data_available==61, sum(across(rdxsurvey_newinc:rdxsurvey_newinc_rdx), na.rm = T), newinc_rdx)) |>
  rowwise() |>
  
  mutate(newinc_rdx = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) |>
  rowwise() |>
  mutate(newrel_pulm_conf = sum(across(new_labconf:ret_rel_labconf), na.rm = T)) |>
  select(iso3, g_whoregion,
         c_newinc,
         new_ptb,
         newinc_rdx,
         newrel_pulm_conf
         ) |>
  arrange(desc(newinc_rdx)) |>
  ungroup() 

f2.2.1_data_region <- wrd_data |>
  full_join(drs_data, by = c("iso3", "g_whoregion")) |>
  
  select(iso3, g_whoregion,rrdst:indst,c_newinc:newrel_pulm_conf) |>
  
  group_by(g_whoregion) |> 
  summarise(across(rrdst:newrel_pulm_conf, sum, na.rm = TRUE)) |>
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(!g_whoregion) |>
  ungroup()

# Calculate global aggregaes
f2.2.1_data_global <- f2.2.1_data_region |>
  
  summarise(across(rrdst:newrel_pulm_conf, sum, na.rm = TRUE)) |>
  ungroup() |>
  mutate(entity = 'Global')

f2.2.1_data <- rbind(f2.2.1_data_global, f2.2.1_data_region) |>
  
  select(entity, c_newinc:newrel_pulm_conf, everything()) |>
  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))  |>
  pivot_longer(cols = c_newinc:indst,
               names_to = "stage",
               values_to = "how_many") |>
  
  mutate(stage = factor(stage,
                        levels = c(
                          "c_newinc",
                          "new_ptb",
                          "newinc_rdx",
                          "newrel_pulm_conf",
                          "rrdst",
                          "indst"),
                          # "bddst",
                          # "lzdst"),
                        labels = c(
                          "Notified",
                          "Notified, pulmonary TB",
                          "Initially tested with a WRD\n(notified cases)",
                          "Pulmonary TB, bacteriologically confirmed",
                          "Pulmonary TB,\nbacteriologically confirmed,\ntested for rifampicin resistance",
                          "Pulmonary TB,\nbacteriologically confirmed,\ntested for isoniazid resistance")))
                          # "Tested for\nbedaquiline resistance",
                          # "Tested for\nlinezolid resistance")))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: bac confirmation ----
# Determine numerators and denominators for bacteriological confirmation and
# then use this dataframe for figures 2.2.1, 2.2.2 and 2.3.3
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bacconf_data <- notification |>
  filter(year >= 2010) |>
  select(iso3,
         country,
         year,
         g_whoregion,
         # old variables pre-2013
         new_sp,
         new_sn,
         new_su,
         # new variables
         new_labconf, new_clindx,
         ret_rel_labconf, ret_rel_clindx) |>

  #calculate % of pulmonary cases with bac confirmation
  rowwise() |>
  # a bit tricky for years before 2013, so do for new only by smear only
  mutate(bacconf_pct_numerator = ifelse(year < 2013 & g_whoregion != 'EUR',
                                        # old variables, do for new only outside EUR
                                        new_sp,
                                        # new variables
                                        sum(c_across(contains("labconf")), na.rm = TRUE)),
         bacconf_pct_denominator = ifelse(year < 2013 & g_whoregion != 'EUR',
                                          # old variables, do for new only outside EUR
                                          sum(c_across(new_sp:new_su), na.rm = TRUE),
                                          # new variables
                                          sum(c_across(new_labconf:ret_rel_clindx), na.rm = TRUE))) |>
  
  # Adjust calculation for EUR pre-2013 (applies to years 2010 - 2012)
  mutate(bacconf_pct_numerator = ifelse(between(year, 2010, 2012) & g_whoregion == 'EUR',
                                        # old variables, but using new_labconf
                                        new_labconf,
                                        # otherwise keep calculation from previous step
                                        bacconf_pct_numerator),
         bacconf_pct_denominator = ifelse(between(year, 2010, 2012) & g_whoregion == 'EUR',
                                          # old variables
                                          sum(c_across(new_sp:new_su), na.rm = TRUE),
                                          # otherwise keep calculation from previous step
                                          bacconf_pct_denominator)) |>
  
  ungroup() |>
  
  # # a bit tricky for years before 2013, so do for new only by smear only
  # mutate(bacconf_pct_numerator = sum(c_across(contains("labconf")), na.rm = TRUE),
  #        bacconf_pct_denominator = sum(c_across(new_labconf:ret_rel_clindx), na.rm = TRUE)) |>
  # 
  # ungroup() |>

  # reduce to needed variables
  select(country,
         iso3,
         year,
         g_whoregion,
         bacconf_pct_numerator,
         bacconf_pct_denominator)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.2 ----
# (Global trend of percent of TB cases tested with rapid diagnostics)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Calculate regional aggregates
f2.2.2_data <- filter(notification, year  >= 2015) |>
  select(country, year,
         g_whoregion,
         rdx_data_available,
         c_newinc,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx#,
         # rdxsurvey_newinc,
         # rdxsurvey_newinc_rdx
         ) |>
  # rowwise() |>
  # mutate(newinc_rdx = ifelse(rdx_data_available==61, sum(across(rdxsurvey_newinc:rdxsurvey_newinc_rdx), na.rm = T), newinc_rdx)) |>
  rowwise() |>
  mutate(newinc_rdx = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) |>
  mutate(newinc_rdx = ifelse(rdx_data_available==60 & year >= 2025 & if_any(
    c(newinc_pulm_labconf_rdx,
      newinc_pulm_clindx_rdx,
      newinc_ep_rdx),
    ~ !is.na(.x)
  ), sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) |>
  arrange(country) |>

  group_by(year, g_whoregion) |>
  summarise(across(c_newinc:newinc_ep_rdx, sum, na.rm=TRUE)) |>
  ungroup() |>
  mutate(wrd_pcnt = newinc_rdx * 100/c_newinc) |>
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(entity, year, newinc_rdx, wrd_pcnt) |>
  ungroup()


# global trend
f2.2.2_data_global <- filter(notification, year  >= 2015) |>
  select(year,
         g_whoregion,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx#,
         # rdxsurvey_newinc,
         # rdxsurvey_newinc_rdx
         ) |>
  # rowwise() |>
  # mutate(newinc_rdx = ifelse(rdx_data_available==61, sum(across(rdxsurvey_newinc:rdxsurvey_newinc_rdx), na.rm = T), newinc_rdx)) |>
  rowwise() |>
  mutate(newinc_rdx = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) |>
  mutate(newinc_rdx = ifelse(rdx_data_available==60 & year >= 2025 & if_any(
    c(newinc_pulm_labconf_rdx,
      newinc_pulm_clindx_rdx,
      newinc_ep_rdx),
    ~ !is.na(.x)
  ), sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) |>
  group_by(year) |>
  summarise(across(c_newinc:newinc_rdx, sum, na.rm=TRUE)) |>
  ungroup() |>
  mutate(wrd_pcnt = newinc_rdx * 100/c_newinc) |>
  select(year, newinc_rdx, wrd_pcnt) |>
  mutate(entity = "Global")

# merge global and regional trends
f2.2.2_data <- rbind(f2.2.2_data, f2.2.2_data_global) |>
  arrange(entity,year) |>
  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))


f2.2.2_txt <- filter(notification, year  >= report_year - 4) |>
  select(year,
         g_whoregion,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx#,
         # rdxsurvey_newinc,
         # rdxsurvey_newinc_rdx
         ) |>
  # rowwise() |>
  # mutate(newinc_rdx = ifelse(rdx_data_available==61, sum(across(rdxsurvey_newinc:rdxsurvey_newinc_rdx), na.rm = T), newinc_rdx)) |>
  rowwise() |>
  mutate(newinc_rdx = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) |>
  mutate(newinc_rdx = ifelse(rdx_data_available==60 & year >= 2025 & if_any(
    c(newinc_pulm_labconf_rdx,
      newinc_pulm_clindx_rdx,
      newinc_ep_rdx),
    ~ !is.na(.x)
  ), sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) |>
  group_by(year) |>
  summarise(across(c_newinc:newinc_rdx, sum, na.rm=TRUE)) |>
  ungroup() |>
  mutate(wrd_pcnt = newinc_rdx * 100/c_newinc) |>
  select( -rdx_data_available ) |>
  pivot_wider(names_from = year,
              values_from = c(c_newinc, newinc_rdx, wrd_pcnt))

# Add info about testing at least half of new cases in the high burden countries
f2.2.2_txt <- notification |>
  
  rowwise() |>
  mutate(newinc_rdx = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) |>
  
  filter(year  >= report_year - 4 &
           newinc_rdx >= 0.5 * c_newinc &
           (iso3 %in% hbc30$iso3 | iso3 %in% hbtbhiv30$iso3 | iso3 %in% hbmdr30$iso3)) |>
  select(year, country) |>
  group_by(year) |>
  summarise(hbcs = n()) |>
  ungroup() |>
  pivot_wider(names_from = year,
              names_prefix = "hbcs_",
              values_from = hbcs) |>
  cbind(f2.2.2_txt) 

f2.2.2_txt <- f2.2.2_data |>
  filter(year == report_year-1) |>
  arrange(wrd_pcnt) |>
  slice(1,5:7) |>
  select(entity,wrd_pcnt) |>
  pivot_wider(names_from = entity, values_from = wrd_pcnt) |>
  rename(searo=`South-East Asia Region`,wpro=`Western Pacific Region`,emro=`Eastern Mediterranean Region`,euro=`European Region`) |>
  cbind(f2.2.2_txt)


# country level for texts in the main report
f2.2.2_data_country <- filter(notification, year  == report_year-1) |>
  select(iso3, country,
         g_whoregion,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx#,
         # rdxsurvey_newinc,
         # rdxsurvey_newinc_rdx
         ) |>
  # rowwise() |>
  # mutate(newinc_rdx = ifelse(rdx_data_available==61, sum(across(rdxsurvey_newinc:rdxsurvey_newinc_rdx), na.rm = T), newinc_rdx)) |>
  rowwise() |>
  mutate(newinc_rdx = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) |>
  mutate(newinc_rdx = ifelse(rdx_data_available==60  & if_any(
    c(newinc_pulm_labconf_rdx,
      newinc_pulm_clindx_rdx,
      newinc_ep_rdx),
    ~ !is.na(.x)
  ), sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) |>
  group_by(iso3) |>
  summarise(across(c_newinc:newinc_rdx, sum, na.rm=TRUE)) |>
  ungroup() |>
  mutate(wrd_pcnt = newinc_rdx * 100/c_newinc) |>
  ungroup()

f2.2.2_data_country |>
  filter(c_newinc>0, wrd_pcnt>=80) |> nrow()
f2.2.2_data_country |>
  filter(c_newinc>0, wrd_pcnt<20, rdx_data_available !=0) |> nrow()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.3 ----
# (map showing percent of TB cases tested with rapid diagnostics)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f2.2.3_data <- notification |>
  filter(year  >= report_year - 2) |>
  select(iso3,
         country,
         year,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx#,
         # rdxsurvey_newinc,
         # rdxsurvey_newinc_rdx
         ) |>
  
  rowwise() |>
  mutate(newinc_rdx_type = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), NA)) |>
  
  # calculate the percentage for each country depending on data availability
  mutate(wrd_pcnt_nu = ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                              newinc_rdx,
                              ifelse(rdx_data_available == 62 & NZ(c_newinc) > 0,
                                     newinc_rdx_type,
                                     # ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                     #        rdxsurvey_newinc_rdx,
                                            NA)),
         wrd_pcnt_de = ifelse((rdx_data_available == 60|rdx_data_available == 62) & NZ(c_newinc) > 0,
                              c_newinc,
                              # ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                              #        rdxsurvey_newinc,
                                     NA),
         wrd_pcnt =ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                          newinc_rdx * 100 / c_newinc,
                          ifelse(rdx_data_available == 62 & NZ(c_newinc) > 0,
                                 newinc_rdx_type * 100 / c_newinc,
                                 # ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                 #        rdxsurvey_newinc_rdx * 100 / rdxsurvey_newinc,
                                        NA))) |>
  
  # Assign the categories for the map
  mutate(var = cut(wrd_pcnt,
                   c(0, 20, 40, 60, 80, Inf),
                   c('<20', '20\u201339', '40\u201359', '60\u201379','\u226580'),
                   right=FALSE)) |>
  
  # get rid of extra variables
  select(country,
         iso3,
         year,
         wrd_pcnt_nu,
         wrd_pcnt_de,
         wrd_pcnt,
         var)


# Find the countries with empty data for latest year and see if there are data for the previous year
wrd_prev_year_data <- f2.2.3_data |>
  filter(year == report_year - 1 & is.na(wrd_pcnt)) |>
  select(iso3) |>
  inner_join(filter(f2.2.3_data, year == report_year - 2), by = "iso3") |>
  filter(!is.na(wrd_pcnt))

# Now combine into one dataframe, with previous data used if latest year's data are not available

f2.2.3_data <- f2.2.3_data |>
  filter(year == report_year - 1) |>
  anti_join(wrd_prev_year_data, by= "iso3") |>
  rbind(wrd_prev_year_data) |>
  left_join(hbc30, by= "iso3") |>
  left_join(hbtbhiv30, by= "iso3") |>
  left_join(hbmdr30, by= "iso3") |>
  rename(hbc30=group_type.x,hbtbhiv30=group_type.y,hbmdr30=group_type)

f2.2.3_data_80 <- f2.2.3_data |>
  filter(wrd_pcnt>=80) |> nrow()

f2.2.3_data_20 <- f2.2.3_data |>
  filter(wrd_pcnt<20) |> nrow()

f2.2.3_txt_list <- f2.2.3_data |>
  filter(!is.na(hbc30), wrd_pcnt>80) |>
  inner_join(en_name, by = "iso3") |>
  arrange(country)

f2.2.3_txt_list2 <- f2.2.3_data |>
  filter(!is.na(hbc30)|!is.na(hbtbhiv30)|!is.na(hbmdr30), wrd_pcnt>=50) |>
  inner_join(en_name, by = "iso3")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.4 ----
# (30HBCs trend of percent of TB cases tested with rapid diagnostics)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# additional to see trend in HBCs
f2.2.4_data <- notification |>
  filter(year  >= 2015) |>
  select(iso3, g_whoregion,
         country,
         year,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx#,
         # rdxsurvey_newinc,
         # rdxsurvey_newinc_rdx
         ) |>
  arrange(iso3) |>
  
  rowwise() |>
  mutate(newinc_rdx_type = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), NA)) |>
  mutate(newinc_rdx_type = ifelse(rdx_data_available==60 & year >= 2025 & if_any(
    c(newinc_pulm_labconf_rdx,
      newinc_pulm_clindx_rdx,
      newinc_ep_rdx),
    ~ !is.na(.x)
  ), sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx_type)) |>
  # filter(g_whoregion == "EUR") |>
  # calculate the percentage for each country depending on data availability
  mutate(wrd_pcnt_nu = ifelse(rdx_data_available == 62 & NZ(c_newinc) > 0,
                              newinc_rdx_type,
                              if_else(rdx_data_available == 60  & NZ(c_newinc) > 0 & year >= 2025 & if_any(
                                          c(newinc_pulm_labconf_rdx,
                                            newinc_pulm_clindx_rdx,
                                            newinc_ep_rdx),
                                          ~ !is.na(.x)
                                        ), newinc_rdx_type,
                                      ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                              newinc_rdx, NA))),
         wrd_pcnt_de = ifelse((rdx_data_available == 60|rdx_data_available == 62) & NZ(c_newinc) > 0,
                              c_newinc,
                              # ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                              #        rdxsurvey_newinc,
                                     NA),
         wrd_pcnt = ifelse(rdx_data_available == 62 & NZ(c_newinc) > 0,
                           newinc_rdx_type * 100 / c_newinc,
                           
                           if_else(rdx_data_available == 60  & NZ(c_newinc) > 0 & year >= 2025 & if_any(
                             c(newinc_pulm_labconf_rdx,
                               newinc_pulm_clindx_rdx,
                               newinc_ep_rdx),
                             ~ !is.na(.x)
                           ), newinc_rdx_type * 100 / c_newinc,
                           ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                                  newinc_rdx * 100 / c_newinc,
                                 # ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                 #        rdxsurvey_newinc_rdx * 100 / rdxsurvey_newinc,
                                        NA)))) |>
  
  # get rid of extra variables
  select(entity = country,
         iso3, g_whoregion,
         year,
         rdx_data_available,
         wrd_pcnt_nu,
         wrd_pcnt_de,
         wrd_pcnt) |>
  filter(iso3 %in% iso3_hbc) |>
  mutate(wrd_pcnt = if_else((iso3 == "UGA" |iso3 == "SLE") & year == 2025, NA, wrd_pcnt))
  
f2.2.4_data <- f2.2.4_data |> 
  mutate(entity = ifelse(entity == "Democratic People's Republic of Korea", "Democratic People's\nRepublic of Korea",
                          ifelse(entity == "Democratic Republic of the Congo", "Democratic Republic\nof the Congo",
                                 entity
                          )))

f2.2.4_txt <- f2.2.4_data |>
  filter(year >= report_year-2) |>
  select(entity, iso3,  year, wrd_pcnt) |>
  pivot_wider(names_from = year,
              names_prefix = "wrd_pcnt_",
              values_from = wrd_pcnt) |>
  filter(wrd_pcnt_2025 < wrd_pcnt_2024*0.95) |>
  inner_join(en_name, by = "iso3") |>
  arrange(entity)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.5 ----
# (World map showing proportion of TB diagnostic sites with WRDs)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.5_data <- lab |>
  filter(year == report_year - 1) |>
  select(iso3,
         country,
         year,
         m_wrd,
         dx_test_sites) |>
  # Calculate the proportion
  mutate(wrd_pct = ifelse(dx_test_sites > 0,
                          m_wrd * 100 / dx_test_sites,
                          NA)) |>
  # Assign the categories for the map
  mutate(var = cut(wrd_pct,
                   c(0, 25, 50, 75, 90, Inf),
                   c('<25', '25\u201349', '50\u201375', '76\u201389','\u226590'),
                   right=FALSE)) |>
  # get rid of extra variables
  select(country,
         iso3,
         year,
         m_wrd,
         dx_test_sites,
         wrd_pct,
         var) |>
  left_join(hbc30, by= "iso3") |>
  left_join(hbtbhiv30, by= "iso3") |>
  left_join(hbmdr30, by= "iso3") |>
  rename(hbc30=group_type.x,hbtbhiv30=group_type.y,hbmdr30=group_type) 

filter(f2.2.5_data, !is.na(hbc30), wrd_pct>50) |>
  nrow()

f2.2.5_txt <- f2.2.5_data |>
  summarise(median = median(wrd_pct, na.rm=TRUE),
            quantile = list(quantile(wrd_pct, probs = seq(.25, 0.75, by = .5), na.rm = TRUE))) |>
  unnest_wider(quantile) |>
  ungroup() |>
  mutate(entity = "Global") |>
  select(entity,
         median,
         q1=`25%`,
         q3=`75%`) 

f2.2.5_country_txt <- filter(f2.2.5_data, !is.na(hbc30), wrd_pct>=50) |>
  inner_join(en_name, by = "iso3") |>
  arrange(country)

# previous year's data
f2.2.5_prev_data <- lab |>
  filter(year == report_year - 3 |year == report_year - 2) |>
  select(iso3,
         country,
         year,
         m_wrd,
         dx_test_sites) |>
  # Calculate the proportion
  mutate(wrd_pct = ifelse(dx_test_sites > 0,
                          m_wrd * 100 / dx_test_sites,
                          NA)) |>
  # get rid of extra variables
  select(country,
         iso3,
         year,
         wrd_pct)

f2.2.5_prev_txt <- f2.2.5_prev_data |>
  group_by(year) |>
  summarise(median = median(wrd_pct, na.rm=TRUE)) |>
  ungroup() |>
  pivot_wider(names_from = year,
              names_prefix = "median_",
              values_from = median)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.6 ----
# (Panel plot of TB cases with bacteriological confirmation by WHO region and globally since 2010)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# by income group for main texts
bacconf_data_income <- bacconf_data |>
  mutate(bacconf_pct = bacconf_pct_numerator * 100 / bacconf_pct_denominator) |>
  inner_join(wb_incomelist, by = "iso3") |>
  filter(year==report_year-1) |>
  group_by(income) |>
  summarise(across(bacconf_pct, median, na.rm = TRUE)) 
  

# Calculate aggregates
bacconf_data_regional <- bacconf_data |>
  group_by(year, g_whoregion) |>
  summarise(across(bacconf_pct_numerator:bacconf_pct_denominator, sum, na.rm = TRUE)) |>

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(-g_whoregion) |>
  ungroup()

bacconf_data_global <- bacconf_data |>
  group_by(year) |>
  summarise(across(bacconf_pct_numerator:bacconf_pct_denominator, sum, na.rm = TRUE)) |>
  mutate(entity = 'Global')

# Add global to the regional aggregates
f2.2.6_data <- rbind(bacconf_data_regional, bacconf_data_global) |>

  # Calculate the percentages
  mutate(bacconf_pct = bacconf_pct_numerator * 100 / bacconf_pct_denominator) |>

  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))



# summary dataset for quoting numbers in the text on bac confirmation in pulmonary TB
f2.2.6_txt <- filter(notification, year == (report_year - 1)) |>
  select(c_newinc,
         new_labconf, new_clindx,
         ret_rel_labconf, ret_rel_clindx) |>
  summarise(across(c_newinc:ret_rel_clindx, sum, na.rm=TRUE)) |>
  mutate(pulm = new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx) |>
  mutate(pulm_pct = pulm * 100/ c_newinc) |>
  select(c_newinc, pulm, pulm_pct)

# Calculate global % bac conf for the last two years and percent change and add to the summary
f2.2.6_txt <- filter(f2.2.6_data, entity == "Global" & year >= report_year-8) |>
  select(year, bacconf_pct) |>
  pivot_wider(names_from = year,
              names_prefix = "bc_pct_",
              values_from = bacconf_pct) |>
  cbind(f2.2.6_txt)

# Add regional max and min values for 2020
f2.2.6_region_txt <- filter(f2.2.6_data, year>=report_year-6 & entity %in% c("Region of the Americas", "African Region","Eastern Mediterranean Region", "Western Pacific Region")) |>
  select(year, entity, bacconf_pct) |>
  pivot_wider(names_from = entity,
              names_prefix = "bc_pct_",
              values_from = bacconf_pct) |>
  # handle spaces in column names
  select(year, 
         bc_pct_AMR = `bc_pct_Region of the Americas`,
         bc_pct_AFR = `bc_pct_African Region`,
         bc_pct_EMR = `bc_pct_Eastern Mediterranean Region`,
         bc_pct_WPR = `bc_pct_Western Pacific Region`) |>
  filter(year %in% c(2020, report_year-2, report_year-1 )) |>
  pivot_wider(names_from = year,
              values_from = bc_pct_AMR:bc_pct_WPR)

# extract absolute number of TB case notifications for the past 3 years
f2.2.6_txt <- filter(notification, year >= (report_year - 4)) |>
  select(year,c_newinc) |>
  group_by(year) |>
  summarise(across(c_newinc, sum, na.rm=TRUE)) |>
  pivot_wider(names_from = year,
              names_prefix = "c_newinc_",
              values_from = c_newinc) |>
  cbind(f2.2.6_txt)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.7 ----
# (World map showing percent of TB cases with bacteriological confirmation)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.7_data <- filter(bacconf_data, year>=report_year-2) |>
  
  # Calculate the percentages
  mutate(bacconf_pct = ifelse(bacconf_pct_denominator > 0,
                              bacconf_pct_numerator * 100 / bacconf_pct_denominator,
                              NA)) |>
  
  # Assign the categories for the map
  mutate(var = cut(bacconf_pct,
                   c(0, 50, 65, 80, Inf),
                   c('<50', '50\u201364', '65\u201379', '\u226580'),
                   right=FALSE))

# Find the countries with empty data for latest year and see if there are data for the previous year
bacconf_prev_year_data <- f2.2.7_data |>
  filter(year == report_year - 1 & is.na(bacconf_pct)) |>
  select(iso3) |>
  inner_join(filter(f2.2.7_data, year == report_year - 2), by = "iso3") |>
  filter(!is.na(bacconf_pct))

# Now combine into one data frame, with previous data used if latest year's data are not available
f2.2.7_data <- f2.2.7_data |>
  filter(year == report_year - 1) |>
  anti_join(bacconf_prev_year_data, by= "iso3") |>
  rbind(bacconf_prev_year_data)

f2.2.7_data_80 <- f2.2.7_data |>
  filter(bacconf_pct>=80) |> nrow()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.8 ----
# (Panel plot of TB cases with bacteriological confirmation for 30 countries since 2010)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.8_data <- bacconf_data |>
  inner_join(hbc30, by = "iso3") |>

  # Calculate the percentages
  mutate(bacconf_pct = ifelse(bacconf_pct_denominator > 0,
                              bacconf_pct_numerator * 100 / bacconf_pct_denominator,
                              NA)) |>

  # get rid of extra variables
  select(iso3, country,
         year,
         bacconf_pct)

f2.2.8_data <- f2.2.8_data |> 
  mutate(country = ifelse(country == "Democratic People's Republic of Korea", "Democratic People's\nRepublic of Korea",
                          ifelse(country == "Democratic Republic of the Congo", "Democratic Republic\nof the Congo",
                                 country
                          )))


# summary datasets for quoting numbers in the text
f2.2.8_txt_COD <- filter(f2.2.8_data, year %in% c(2015, report_year-1) & iso3=="COD") |>
  pivot_wider(names_from = year,
              names_prefix = "bacconf_pct_",
              values_from = bacconf_pct)

f2.2.8_txt_list_hi <- filter(f2.2.8_data, year==report_year-1 & bacconf_pct > 75)   |>
  arrange(country) |>
  select(iso3) |>
  inner_join(en_name, by = "iso3")

f2.2.8_txt_list_lo <- filter(f2.2.8_data, year==report_year-1 & bacconf_pct < 55)  |>
  arrange(country) |>
  select(iso3) |>
  inner_join(en_name, by = "iso3")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.9 ----
# (Panel plot of TB cases tested for susceptibility to rifampicin by WHO region and globally since 2009)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The calculations here are a bit messy for years prior to 2018. It is much simpler starting in 2018 when
# we switched to using routine DR surveillance records only. Prior to that we mixed and matched between
# that and the DR-TB detection section of the data collection form. Keeping this for consistency with
# previously published reports

# 1. Get DR-TB detection data

dst_notif_data <- notification |>
  filter(year >= 2010) |>
  select(year,
         iso3,
         g_whoregion,
         new_labconf,
         c_ret,
         rdst_new,
         rdst_ret) |>
  # left_join(select(dr_surveillance, year, iso3, rdst_new, rdst_ret), by = c("iso3", "year")) |>

  rowwise() |>
  mutate(

    # numerator
    dst_notif_num = ifelse(is.na(rdst_new) & is.na(rdst_ret),
                           NA,
                           sum(c_across(rdst_new:rdst_ret), na.rm = TRUE)),

    # denominator is a bit of a fudge: new_labconf + c_ret
    dst_notif_denom = ifelse(is.na(new_labconf) & is.na(c_ret),
                             NA,
                             sum(c_across(new_labconf:c_ret), na.rm = TRUE))) |>
  ungroup() |>
  arrange(iso3)


# 2. Get routine DR surveillance data
# (numerator and denominator variables are different according to the year, but
#  don't use 2015, 2016 numerator data)

dst_drs_data <- dr_surveillance |>
  filter(year >= 2010) |>
  select(year,
         iso3,
         dst_rlt_new,
         dst_rlt_ret,
         r_rlt_dst_new,
         r_rlt_dst_ret,
         r_rlt_dst_unk) |>
  arrange(iso3) |>
  full_join(select(notification, year, iso3, pulm_labconf_new, pulm_labconf_ret), by = c("iso3", "year")) |>
  filter(year >= 2010) |>
  rowwise() |>
  mutate(
    # numerator
    dst_drs_num = ifelse(year < 2015,
                         ifelse(is.na(dst_rlt_new) & is.na(dst_rlt_ret),
                                NA,
                                sum(c_across(dst_rlt_new:dst_rlt_ret), na.rm = TRUE)),
                         ifelse(year >= 2017,
                                sum(c_across(r_rlt_dst_new:r_rlt_dst_unk), na.rm = TRUE),
                                NA)),
    # denominator
    dst_drs_denom = ifelse(year >= 2017,
                           sum(c_across(pulm_labconf_new:pulm_labconf_ret), na.rm = TRUE),
                            NA)
  ) |>
  ungroup() |>
  arrange(iso3)


# Link the two data sets
dst_data <- dst_notif_data |>
  left_join(dst_drs_data, by = c("year", "iso3")) |>

  # To calculate the percentage DST coverage we need to identify the greater of the two numerators
  # Note the exception made for South Africa in 2017


  mutate(
    dst_num = ifelse(year == 2017 & iso3 == "ZAF",
                     dst_notif_num,
                     ifelse(year >= 2017,
                            dst_drs_num,
                           ifelse(NZ(dst_drs_num) >= NZ(dst_notif_num),
                                  dst_drs_num,
                                  dst_notif_num))),

    dst_denom = ifelse(year == 2017 & iso3 == "ZAF",
                       dst_notif_denom,
                       ifelse(year >= 2017,
                              dst_drs_denom,
                              dst_notif_denom))) |>

  # Set numerator to NA if the denominator is NA for a country-year
  mutate(dst_num = ifelse(is.na(dst_denom), NA, dst_num)) |>

  # Drop unwanted variables
  select(iso3,
         year,
         g_whoregion,
         dst_num,
         dst_denom) |>

  # Drop rows with empty numerators and denominator
  filter(!is.na(dst_num) & !is.na(dst_denom)) #|>
  # mutate(dst_num = if_else(iso3 == "RUS" & year == 2010, 49267, dst_num),
  #        dst_num = if_else(iso3 == "RUS" & year == 2011, 47627, dst_num),
  #        dst_num = if_else(iso3 == "RUS" & year == 2012, 44971, dst_num),
  #        dst_num = if_else(iso3 == "TUR" & year == 2011, 4823, dst_num))

f2.2.9_data <- dst_data |>
  group_by(g_whoregion, year) |>
  summarise(across(dst_num:dst_denom, sum, na.rm = TRUE)) |>

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  ungroup() |>
  select(-g_whoregion)

dst_global <- dst_data |>
  group_by(year) |>
  summarise(across(dst_num:dst_denom, sum, na.rm = TRUE)) |>
  mutate(entity = "Global") |>
  ungroup()

# Phew! Bring it all together now
# COmbine regional with global
f2.2.9_data <- rbind(f2.2.9_data, dst_global) |>

  # Calculate % tested for rifampicin resistance
  mutate(dst_pcnt = dst_num * 100 / dst_denom) |>

  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))


# summary data for text
f2.2.9_txt <- filter(f2.2.9_data, year %in% c(2015, 2020, report_year-2, report_year-1) & entity %in% c("European Region", "Global")) |>
  select(year, entity, dst_pcnt) |>
  pivot_wider(names_from = c(entity, year),
              names_prefix = "dst_pct_",
              values_from = dst_pcnt) #|>
  # Handle spaces in variable name
  # mutate(dst_pct_EUR_2021 = `dst_pct_European Region_2021`)

# Add numbers of drug-resistant cases detected
f2.2.9_txt <- filter(notification, year >= report_year - 3) |>
  
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
         c_newinc,
         conf_rr_nfqr,
         conf_rr_fqr,
         conf_rrmdr,
         rr_tx_lt12m,
         rr_tx_gt12m_lt24m,
         unconf_rr_nfqr_tx,
         conf_rr_nfqr_tx,
         conf_rr_fqr_tx) |>
  group_by(year) |>
  summarise(across(c_newinc:conf_rr_fqr_tx, sum, na.rm = TRUE)) |>
  ungroup() |>
  mutate(dr_tb = conf_rr_nfqr + conf_rr_fqr + conf_rrmdr) |>
  mutate(dr_tb_tx = rr_tx_lt12m + rr_tx_gt12m_lt24m + unconf_rr_nfqr_tx + conf_rr_nfqr_tx + conf_rr_fqr_tx) |>
  select(year, c_newinc, dr_tb, dr_tb_tx) |>
  pivot_wider(names_from = year,
              values_from = c(c_newinc,
                              dr_tb,
                              dr_tb_tx)) |>
  mutate(dr_tb_change_pct = abs(dr_tb_2025 - dr_tb_2024) * 100 / dr_tb_2024,
         dr_tb_tx_change_pct = abs(dr_tb_tx_2025 - dr_tb_tx_2024) * 100 / dr_tb_tx_2024,
         c_newinc_change_pct= abs(c_newinc_2025 - c_newinc_2024) * 100 / c_newinc_2024 ) |>
  cbind(f2.2.9_txt)


f2.2.9_txt2 <- filter(f2.2.9_data, year %in% c(report_year-1) & entity != "Global") |>
  select(year, entity, dst_pcnt) |>
  filter(dst_pcnt > 89) 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.10 ----
# (World map showing percent of TB cases tested for susceptibility to rifampicin)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.10_data <- dr_surveillance |>
  filter(year >= report_year - 2) |>
  select(year,
         country,
         iso3,
         r_rlt_dst_new,
         r_rlt_dst_ret) |>
  left_join(select(notification, year, iso3, pulm_labconf_new, pulm_labconf_ret), by = c("iso3", "year")) |>
  

  # Calculate coverage of DST percentages
  mutate(
    dst_pct = ifelse((NZ(pulm_labconf_new) + NZ(pulm_labconf_ret)) == 0 |
                       is.na(r_rlt_dst_new) & is.na(r_rlt_dst_ret), NA,
                     (NZ(r_rlt_dst_new) + NZ(r_rlt_dst_ret)) * 100 /
                       (NZ(pulm_labconf_new) + NZ(pulm_labconf_ret)))
  ) |>

  # Assign the categories for the map
  mutate(var = cut(dst_pct,
                   c(0, 20, 50, 80, Inf),
                   c('0\u201319', '20\u201349', '50\u201379', '\u226580'),
                   right=FALSE)) |>

  # get rid of extra variables
  select(country,
         iso3,
         year,
         dst_pct,
         var)


# Find the countries with empty data for latest year and see if there are data for the previous year
dst_prev_year_data <- f2.2.10_data |>
  filter(year == report_year - 1 & is.na(dst_pct)) |>
  select(iso3) |>
  inner_join(filter(f2.2.10_data, year == report_year - 2), by = "iso3") |>
  filter(!is.na(dst_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
f2.2.10_data <- f2.2.10_data |>
  filter(year == report_year - 1) |>
  anti_join(dst_prev_year_data, by= "iso3") |>
  rbind(dst_prev_year_data)


# summary numbers for the text
filter(f2.2.10_data, iso3 %in% hbmdr30$iso3) |>
  arrange(country) 

f2.2.10_txt <- filter(f2.2.10_data, dst_pct >=79.5 & iso3 %in% hbmdr30$iso3) |>
  arrange(country) |>
  select(iso3) |>
  inner_join(en_name, by = "iso3") 

f2.2.10_low_txt <- filter(f2.2.10_data, dst_pct <50 & iso3 %in% hbmdr30$iso3) |>
  arrange(country) |>
  select(iso3, dst_pct) |>
  inner_join(en_name, by = "iso3") 



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.11 ----
# (Resistance cascade)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.11_data_region <- drs_data |>
  # left_join(select(wrd_data, iso3, new_labconf), by = c("iso3")) |>
  
  select(g_whoregion,#new_labconf, 
         rrpos:prxdr, bdlzt:xdr) |>
  
  
  group_by(g_whoregion) |> 
  summarise(across(rrpos:xdr, sum, na.rm = TRUE)) |>
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(!g_whoregion) |>
  ungroup()

# Calculate global aggregaes
f2.2.11_data_global <- f2.2.11_data_region |>
  
  summarise(across(rrpos:xdr, sum, na.rm = TRUE)) |>
  ungroup() |>
  mutate(entity = 'Global')

f2.2.11_data <- rbind(f2.2.11_data_global, f2.2.11_data_region) |>
  
  select(entity, rrpos:xdr, everything()) |>
  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))  |>
  pivot_longer(cols = rrpos:xdr,
               names_to = "stage",
               values_to = "how_many") |>
  
  mutate(stage = factor(stage,
                        levels = c(
                          # "new_labconf",
                          "rrpos",
                          "fqdst",
                          "prxdr",
                          "bdlzt",
                          "xdr"),
                        labels = c(
                          # "Pulmonary TB, bacteriologically confirmed\n(new or recurrent case)",
                          "People diagnosed\nwith rifampicin resistance",
                          "Tested for\nfluoroquinolone resistance",
                          "People diagnosed\nwith pre-XDR-TB",
                          "Tested for\nbedaquiline/linezolid resistance",
                          "People diagnosed\nwith XDR-TB")))





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.12 ----
# (Panel plot of RR-TB cases tested for susceptibility to fluoroquinolones by WHO region and globally since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.12_data <- dr_surveillance |>
  filter(year >= 2015) |>
  select(iso3,
         g_whoregion,
         year,
         # denominator changed in 2017 from mdr to rr
         mdr_new,
         mdr_ret,
         xpert_dr_r_new,
         xpert_dr_r_ret,
         rr_dst_new,
         rr_dst_ret,
         # numerator changed in 2017 from mdr to rr and in 2019 from sld to fq
         mdr_dst_rlt,
         rr_dst_rlt,
         rr_dst_rlt_fq) |>

  group_by(year, g_whoregion) |>
  summarise(across(mdr_new:rr_dst_rlt_fq, sum, na.rm = TRUE)) |>
  ungroup() |>
  arrange(g_whoregion) |>

  # Calculate the numerators and denominators depending on the year
  mutate(fqdst_pct_denominator = ifelse(year < 2017,
                                        mdr_new + mdr_ret + xpert_dr_r_new + xpert_dr_r_ret,
                                        NA),
         fqdst_pct_numerator = ifelse(year < 2017,
                                      mdr_dst_rlt,
                                      NA)) |>

  mutate(fqdst_pct_denominator = ifelse(year >= 2017,
                                        rr_dst_new + rr_dst_ret,
                                        fqdst_pct_denominator),
         fqdst_pct_numerator = ifelse(year %in% c(2017, 2018),
                                      rr_dst_rlt,
                                      fqdst_pct_numerator)) |>

  mutate(fqdst_pct_numerator = ifelse(year >= 2019,
                                      rr_dst_rlt_fq,
                                      fqdst_pct_numerator)) |>

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>

  # get rid of extra variables
  select(entity,
         year,
         fqdst_pct_numerator,
         fqdst_pct_denominator)

# Calculate global aggregaes
fqdst_global <- f2.2.12_data |>
  group_by(year) |>
  summarise(across(fqdst_pct_numerator:fqdst_pct_denominator, sum, na.rm = TRUE)) |>
  ungroup() |>
  mutate(entity = 'Global')

# Add global to the regional aggregates
f2.2.12_data <- rbind(f2.2.12_data, fqdst_global) |>

  # Calculate the percentages
  mutate(fqdst_pct = fqdst_pct_numerator * 100 / fqdst_pct_denominator) |>

  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))

f2.2.12_txt <- filter(notification, year >= (report_year - 3)) |>
  select(year,
         g_whoregion,
         c_notified,
         c_newinc,
         new_labconf, new_clindx, new_ep,
         ret_rel_labconf, ret_rel_clindx, ret_rel_ep,
         newrel_hivpos,
         conf_rr_nfqr,
         conf_rr_fqr) |>
  
  # calculate regional aggregates
  group_by(year, g_whoregion) |>
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) |>
  
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  arrange(entity) |>
  ungroup() |>
  select(-g_whoregion)


# Add global summary to the regional summary
f2.2.12_txt <- f2.2.12_txt |>
  group_by(year) |>
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) |>
  mutate(entity="Global") |>
  mutate(rr_nfqr_fqr = conf_rr_nfqr+conf_rr_fqr) |>
  mutate(rr_nfqr_fqr_p = lag(rr_nfqr_fqr))|>
  mutate(rr_nfqr_pct_dif = (rr_nfqr_fqr - rr_nfqr_fqr_p)*100/rr_nfqr_fqr_p) |>
  mutate(c_newinc_p   = lag(c_newinc)) |>
  mutate(c_newinc_pct_dif = (c_newinc - c_newinc_p)*100/c_newinc_p) |>
  select(entity, year, 
         conf_rr_nfqr,
         conf_rr_fqr,
         rr_nfqr_fqr, rr_nfqr_pct_dif, c_newinc_pct_dif) |>
  pivot_wider(names_from = year,
              values_from = conf_rr_nfqr:c_newinc_pct_dif) 

f2.2.12_txt2 <- f2.2.12_data |>
  filter(year == report_year-1 & (entity=="European Region"|entity=="Eastern Mediterranean Region"|entity=="Western Pacific Region")) |>
  select(entity, fqdst_pct) |>
  pivot_wider(names_from = entity, values_from = fqdst_pct) |>
  rename(emr = 1, eur = 2, wpr = 3)

f2.2.12_txt3 <- f2.2.12_data |>
  filter(entity == "Global" & year>= report_year-3) |>
  select(year, fqdst_pct) |>
  pivot_wider(names_from = year, 
              names_prefix = "fqdst_pct_",
              values_from = fqdst_pct) 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.13 ----
# (World map showing percent of MDR/RR-TB cases tested for susceptibility to fluoroquinolones)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.13_data <- dr_surveillance |>
  filter(year >= report_year - 1) |>
  select(iso3,
         country,
         year,
         rr_dst_new,
         rr_dst_ret,
         rr_dst_rlt_fq) |>
  
  # Calculate percentage RR cases with 2nd line DST
  mutate(fqdst_pct = ifelse( (NZ(rr_dst_new) + NZ(rr_dst_ret)) > 0,
                             rr_dst_rlt_fq * 100 / (NZ(rr_dst_new) + NZ(rr_dst_ret)),
                             NA)) |>
  
  # Assign the categories for the map
  mutate(var = cut(fqdst_pct,
                   c(0, 20, 50, 80, Inf),
                   c('<20', '20\u201349', '50\u201379', '\u226580'),
                   right=FALSE)) |>
  
  # get rid of extra variables
  select(country,
         iso3,
         year,
         fqdst_pct,
         var)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.14 ----
# (World map showing percent of pre-XDR-TB cases tested for susceptibility to bedaquiline)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.14_data <- dr_surveillance |>
  filter(year >= report_year - 1) |>
  select(iso3,
         country,
         year,
         rr_fqr,
         rr_fqr_dst_bdqr_lzdr,rr_fqr_dst_bdqs_lzdr,rr_fqr_dst_bdqu_lzdr,
         rr_fqr_dst_bdqr_lzds,rr_fqr_dst_bdqs_lzds,rr_fqr_dst_bdqu_lzds,
         rr_fqr_dst_bdqr_lzdu,rr_fqr_dst_bdqs_lzdu,rr_fqr_dst_bdqu_lzdu) |>
  
  # Calculate percentage RR cases with 2nd line DST
  mutate(bddst_pct = ifelse( NZ(rr_fqr) > 0,
                             (NZ(rr_fqr_dst_bdqr_lzdr)+NZ(rr_fqr_dst_bdqs_lzdr)+NZ(rr_fqr_dst_bdqr_lzds)+NZ(rr_fqr_dst_bdqs_lzds)+NZ(rr_fqr_dst_bdqr_lzdu)+NZ(rr_fqr_dst_bdqs_lzdu)) * 100 / NZ(rr_fqr),
                             NA)) |>
  
  # Assign the categories for the map
  mutate(var = cut(bddst_pct,
                   c(0, 20, 50, 80, Inf),
                   c('<20', '20\u201349', '50\u201379', '\u226580'),
                   right=FALSE)) |>
  
  # get rid of extra variables
  select(country,
         iso3,
         year,
         bddst_pct,
         var)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.15 ----
# (World map showing percent of RR-TB cases tested for susceptibility to bedaquiline)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.15_data <- dr_surveillance |>
  filter(year >= report_year - 1) |>
  select(iso3,
         country,
         year,
         rr_dst_new,
         rr_dst_ret,
         rr_dst_rlt_bdq) |>
  
  # Calculate percentage RR cases with 2nd line DST
  mutate(rrbdq_pct = ifelse( (NZ(rr_dst_new) + NZ(rr_dst_ret)) > 0,
                             rr_dst_rlt_bdq * 100 / (NZ(rr_dst_new) + NZ(rr_dst_ret)),
                             NA)) |>
  
  # Assign the categories for the map
  mutate(var = cut(rrbdq_pct,
                   c(0, 20, 50, 80, Inf),
                   c('<20', '20\u201349', '50\u201379', '\u226580'),
                   right=FALSE)) |>
  
  # get rid of extra variables
  select(country,
         iso3,
         year,
         rrbdq_pct,
         var)


f2.2.15_txt <- f2.2.15_data |>
  filter(rrbdq_pct>80 & iso3!= "HKG") |>
  arrange(country) |>
  inner_join(en_name, by = "iso3")

f2.2.15_txt2 <- dr_surveillance |>
  filter(year >= report_year - 2) |>
  group_by(year) |>
  select(year, rr_bdqr) |>
  summarise(across(rr_bdqr, sum, na.rm = TRUE))
  

f2.2.15_txt_prev <- dr_surveillance |>
  filter(year == report_year - 2) |>
  select(iso3,
         country,
         year,
         rr_dst_new,
         rr_dst_ret,
         rr_dst_rlt_bdq) |>
  
  # Calculate percentage RR cases with 2nd line DST
  mutate(rrbdq_pct = ifelse( (NZ(rr_dst_new) + NZ(rr_dst_ret)) > 0,
                             rr_dst_rlt_bdq * 100 / (NZ(rr_dst_new) + NZ(rr_dst_ret)),
                             NA)) |>
  filter(rrbdq_pct>80 & iso3!= "HKG") |> nrow()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.16 ----
# (World map showing percent of pre-XDR-TB cases tested for susceptibility to linezolid)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.16_data <- dr_surveillance |>
  filter(year >= report_year - 1) |>
  select(iso3,
         country,
         year,
         rr_fqr,
         rr_fqr_dst_bdqr_lzdr,rr_fqr_dst_bdqs_lzdr,rr_fqr_dst_bdqu_lzdr,
         rr_fqr_dst_bdqr_lzds,rr_fqr_dst_bdqs_lzds,rr_fqr_dst_bdqu_lzds,
         rr_fqr_dst_bdqr_lzdu,rr_fqr_dst_bdqs_lzdu,rr_fqr_dst_bdqu_lzdu) |>
  
  # Calculate percentage RR cases with 2nd line DST
  mutate(lzdst_pct = ifelse( NZ(rr_fqr) > 0,
                             (NZ(rr_fqr_dst_bdqr_lzdr)+NZ(rr_fqr_dst_bdqs_lzdr)+NZ(rr_fqr_dst_bdqr_lzds)+NZ(rr_fqr_dst_bdqs_lzds)+NZ(rr_fqr_dst_bdqu_lzdr)+NZ(rr_fqr_dst_bdqu_lzds)) * 100 / NZ(rr_fqr),
                             NA)) |>
  
  # Assign the categories for the map
  mutate(var = cut(lzdst_pct,
                   c(0, 20, 50, 80, Inf),
                   c('<20', '20\u201349', '50\u201379', '\u226580'),
                   right=FALSE)) |>
  
  # get rid of extra variables
  select(country,
         iso3,
         year,
         lzdst_pct,
         var)

f2.2.16_txt <- f2.2.16_data |>
  filter(lzdst_pct>80)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.17 ----
# (Panel plot of TB cases with known HIV status by WHO region and globally since 2004)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Calculate regional aggregates
f2.2.17_data <- TBHIV_for_aggregates |>
  filter(year >= 2010) |>
  select(iso3, g_whoregion,
         year,
         hivtest_pct_numerator,
         hivtest_pct_denominator) |>
  filter(!is.na(hivtest_pct_numerator) & hivtest_pct_numerator != 0) |>
  arrange(iso3) |>
  group_by(year, g_whoregion) |>
  summarise(across(hivtest_pct_numerator:hivtest_pct_denominator, sum, na.rm = TRUE)) |>
  arrange(g_whoregion) |>
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(-g_whoregion) |>
  ungroup()

# write.csv(f2.2.17_data, here::here("local/hivtest_2026.csv"), na="", row.names = F)

# Calculate Global aggregates
hivstatus_global <- f2.2.17_data |>
  select(year,
         hivtest_pct_numerator,
         hivtest_pct_denominator) |>
  group_by(year) |>
  summarise(across(hivtest_pct_numerator:hivtest_pct_denominator, sum, na.rm = TRUE)) |>
  mutate(entity = "Global")

# COmbine regional with global
f2.2.17_data <- rbind(f2.2.17_data, hivstatus_global) |>
  
  # Calculate % with known HIV status
  mutate(hivstatus_pct = hivtest_pct_numerator * 100 / hivtest_pct_denominator) |>
  
  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))

# summary dataset for quoting numbers in the text
f2.2.17_txt <- filter(f2.2.17_data, year>=report_year-3 & entity %in% c("African Region","Region of the Americas", "European Region","South-East Asia Region", "Global")) |>
  select(year, entity, hivstatus_pct) |>
  pivot_wider(names_from = c(entity, year),
              values_from = hivstatus_pct) |>
  # handle spaces in column names
  rename(
    AFR_2023 = `African Region_2023`,
    AMR_2023 = `Region of the Americas_2023`,
    EUR_2023 = `European Region_2023`,
    SEA_2023 = `South-East Asia Region_2023`,
    AFR_2024 = `African Region_2024`,
    AMR_2024 = `Region of the Americas_2024`,
    EUR_2024 = `European Region_2024`,
    SEA_2024 = `South-East Asia Region_2024`,
    AFR_2025 = `African Region_2025`,
    AMR_2025 = `Region of the Americas_2025`,
    EUR_2025 = `European Region_2025`,
    SEA_2025 = `South-East Asia Region_2025`)

# Add the numbers that are HIV-positive
f2.2.17_txt <- filter(TBHIV_for_aggregates, year == report_year-1) |>
  summarise(across(c(hivtest_pos_pct_numerator, hivtest_pos_pct_denominator), sum, na.rm = TRUE)) |>
  mutate(hivtest_pos_pct = hivtest_pos_pct_numerator * 100 / hivtest_pos_pct_denominator) |>
  cbind(f2.2.17_txt)

f2.2.17_txt2 <- filter(TBHIV_for_aggregates, year > 2000) |>
  group_by(year) |>
  summarise(across(c(hivtest_pos_pct_numerator, hivtest_pos_pct_denominator), sum, na.rm = TRUE)) |>
  mutate(hivtest_pos_pct = hivtest_pos_pct_numerator * 100 / hivtest_pos_pct_denominator) |>
  slice_max(order_by = hivtest_pos_pct, n = 1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.18 ----
# (World map showing percent of TB cases with known HIV status)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.18_data <- TBHIV_for_aggregates |>
  filter(year >= report_year - 2) |>
  select(iso3,
         country,
         year,
         g_whoregion,
         hivtest_pct_denominator,
         hivtest_pct_numerator) |>
  
  # Calculate % with known HIV status
  mutate(hivstatus_pct = ifelse(hivtest_pct_denominator > 0,
                                hivtest_pct_numerator * 100 / hivtest_pct_denominator,
                                NA))  |>
  
  # Assign the categories for the map
  mutate(var = cut(hivstatus_pct,
                   c(0, 50, 76, 90, Inf),
                   c('0\u201349', '50\u201375', '76\u201389', "\u226590"),
                   right=FALSE)) |>
  
  # get rid of extra variables
  select(country,
         iso3,
         year,
         g_whoregion,
         hivstatus_pct,
         var)

# Find the countries with empty data for latest year and see if there are data for the previous year
hivstatus_prev_year_data <- f2.2.18_data |>
  filter(year == report_year - 1 & is.na(hivstatus_pct)) |>
  select(iso3) |>
  inner_join(filter(f2.2.18_data, year == report_year - 2)) |>
  filter(!is.na(hivstatus_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
f2.2.18_data <- f2.2.18_data |>
  filter(year == report_year - 1) |>
  anti_join(hivstatus_prev_year_data, by= "iso3") |>
  rbind(hivstatus_prev_year_data)

# Summary data for text
f2.2.18_txt <- filter(f2.2.18_data, hivstatus_pct >= 90) |>
  summarise(over_90 = n())

f2.2.18_txt_afr <- filter(f2.2.18_data, hivstatus_pct >= 90) |>
  filter(g_whoregion == "AFR") |> nrow()

f2.2.18_txt_lo_list <- filter(f2.2.18_data, hivstatus_pct <= 50) |> arrange(country) |>
  select(iso3) |>
  inner_join(en_name, by = "iso3") 
f2.2.18_txt_lo <- filter(f2.2.18_data, hivstatus_pct <= 50) |> nrow()

f2.2.18_txt_lo_prev <- TBHIV_for_aggregates |>
  filter(year == report_year - 2) |>
  select(iso3,
         country,
         year,
         g_whoregion,
         hivtest_pct_denominator,
         hivtest_pct_numerator) |>
  
  # Calculate % with known HIV status
  mutate(hivstatus_pct = ifelse(hivtest_pct_denominator > 0,
                                hivtest_pct_numerator * 100 / hivtest_pct_denominator,
                                NA))  |>
  filter(hivstatus_pct <= 50) |> nrow()

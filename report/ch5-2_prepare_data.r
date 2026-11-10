# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch5-2.rmd
# Takuya Yamanaka, June 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load chapter 5 packages, settings and data
source(here::here('report/ch5_load_data.r'))

dnk <- 3
no <- 0

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.1 ----
# (National surveys of costs faced by TB patients and their households since 2015: progress and plans)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

palatte_fig5.2.1 = c("royalblue4","dodgerblue3","steelblue1","darkviolet","maroon1"#,"violet"
                     )

load(here::here('data/pcs/pcs_progress_data.Rdata'))
  
num <- table(f5.2.1_data$var)
labs <- c(
  paste0("First survey completed (n=",num[1],")"),
  paste0("First survey ongoing (n=",num[2],")"),
  paste0("First survey planned (n=",num[3],")"),
  paste0("Repeat survey completed (n=",num[4],")"),
  paste0("Repeat survey ongoing (n=",num[5],")"),
  paste0("Repeat survey planned (n=",num[6],")")
)

f5.2.2_txt <-  f5.2.1_data %>%
  filter(var == "First survey completed"|var == "Repeat survey planned"|var == "Repeat survey ongoing"|var == "Repeat survey completed") %>%
  nrow

f5.2.2_txt <- f5.2.1_data %>%
  filter(var == "First survey completed"|var == "Repeat survey planned"|var == "Repeat survey ongoing"|var == "Repeat survey completed") %>%
  filter(iso3 %in% iso3_hbc) %>%
  nrow %>% 
  cbind.data.frame(f5.2.2_txt) %>%
  rename(hbc30 = 1, all_country = 2)

f5.2.2_txt <- f5.2.1_data %>%
  filter(var == "Repeat survey completed") %>%
  nrow %>% 
  cbind.data.frame(f5.2.2_txt) %>%
  rename(hbc30 = 2, all_country = 3, repeat_survey = 1) |>
  mutate(total_survey = all_country + repeat_survey)

# write csv for social protection related work
f5.2.1_data %>%
  write_csv(paste0(here::here('report/local/'),"/tbpcs_status_",report_year,".csv")) 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.3 ----
# (Distribution of costs faced by TB patients and their households in 25 national surveys completed since 2016)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
load(here::here('data/pcs/pcs_cost_tot_data.Rdata'))

weo_us <- weo %>% filter(iso3=="USA") %>%
  ungroup() %>%
  select(year,deflator_us)

f5.2.3_const_data <- f5.2.3_data %>%
  left_join(weo_us, by = c("year")) %>%
  mutate(across(c(value:uci), ~ . / deflator_us)) 

f5.2.3_sel_order <- 
  f5.2.3_const_data %>% 
  arrange(value) %>% 
  mutate(country_a = ifelse(iso3=="ZMB","Zambia",
                            ifelse(iso3=="NAM","Namibia",country))) %>% 
  mutate(country = factor(country),
         country_a = factor(country_a)) 

max <- max(f5.2.3_const_data$value, na.rm = T)
min <- min(f5.2.3_const_data$value, na.rm = T)

f5.2.3_txt_num <- f5.2.3_const_data %>%
  filter(!is.na(value))

f5.2.3_txt_min <- f5.2.3_const_data %>%
  filter(value == min)

f5.2.3_txt_max <- f5.2.3_const_data %>%
  filter(value == max)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.4 ----
# (Selected baseline results from national surveys^a^ of costs faced by TB patients and their households, latest year   )
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
load(here::here('data/pcs/pcs_catast_data.Rdata'))

# Subset data for All TB to estimate pooled average
notification <- notification %>%
  filter(year>2010) |>
  mutate(c.notified=c_newinc) %>%
  mutate(conf_rrmdr=ifelse(year<2020, conf_rrmdr,
                           ifelse(year<2025 & year>=2020, NZ(conf_rr_nfqr) + NZ(conf_rr_fqr), NZ(rr_tx_lt12m) + NZ(rr_tx_gt12m_lt24m) + NZ(conf_rr_nfqr_tx) + NZ(conf_rr_fqr_tx)))) |>
  mutate(c_ds=NZ(c.notified)-NZ(conf_rrmdr)) |>
  select(iso3, year, g_whoregion, c_newinc, c.notified, c_ds, conf_rrmdr)

# include the latest surveys only
f5.2.4a_data <- f5.2.4_data %>% 
  filter(grp=='overall') %>% 
  arrange(iso3, desc(year)) |>
  group_by(iso3) |>
  slice(1) |>
  ungroup() |>
  mutate(year=ifelse(year>report_year-1,report_year-1,year)) 

notification %>% select(iso3,year,g_whoregion,c.notified) %>% right_join(f5.2.4a_data,by=c('iso3','year')) -> f5.2.4a_data

fit_all <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = f5.2.4a_data, 
    weights = c.notified
  )

fit_afr <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = filter(f5.2.4a_data, g_whoregion == "AFR"),
    weights = c.notified
  )

fit_amr <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = filter(f5.2.4a_data, g_whoregion == "AMR"),
    weights = c.notified
  )

fit_sea <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = filter(f5.2.4a_data, g_whoregion == "SEA"),
    weights = c.notified
  )

fit_wpr <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = filter(f5.2.4a_data, g_whoregion == "WPR"),
    weights = c.notified
  )

# Save pooled average in the data/pcs folder for importing into the Global Database so that
# it can be included in the global TB profile shown in the mobile app and on the web
pooled_catst_costs_average_global <- data.frame(
  group_type = "global",
  group_name = "global",
  year_range = paste0("2015-", report_year),
  patient_group = "all",
  catast_pct = as.numeric(fit_all$b),
  catast_pct_lo = fit_all$ci.lb,
  catast_pct_hi = fit_all$ci.ub
)

pooled_catst_costs_average_afr <- data.frame(
  group_type = "g_whoregion",
  group_name = "AFR",
  year_range = paste0("2015-", report_year),
  patient_group = "all",
  catast_pct = as.numeric(fit_afr$b),
  catast_pct_lo = fit_afr$ci.lb,
  catast_pct_hi = fit_afr$ci.ub
)

pooled_catst_costs_average_amr <- data.frame(
  group_type = "g_whoregion",
  group_name = "AMR",
  year_range = paste0("2015-", report_year),
  patient_group = "all",
  catast_pct = as.numeric(fit_amr$b),
  catast_pct_lo = fit_amr$ci.lb,
  catast_pct_hi = fit_amr$ci.ub
)

pooled_catst_costs_average_sea <- data.frame(
  group_type = "g_whoregion",
  group_name = "SEA",
  year_range = paste0("2015-", report_year),
  patient_group = "all",
  catast_pct = as.numeric(fit_sea$b),
  catast_pct_lo = fit_sea$ci.lb,
  catast_pct_hi = fit_sea$ci.ub
)

pooled_catst_costs_average_wpr <- data.frame(
  group_type = "g_whoregion",
  group_name = "WPR",
  year_range = paste0("2015-", report_year),
  patient_group = "all",
  catast_pct = as.numeric(fit_wpr$b),
  catast_pct_lo = fit_wpr$ci.lb,
  catast_pct_hi = fit_wpr$ci.ub
)

pooled_catst_costs_average <- pooled_catst_costs_average_global |>
  rbind(pooled_catst_costs_average_afr,pooled_catst_costs_average_amr,pooled_catst_costs_average_sea,pooled_catst_costs_average_wpr)
  
save(pooled_catst_costs_average, file = here::here("data/pcs/pcs_pooled_catast_data.Rdata"))

pooled_catst_costs_average <- pooled_catst_costs_average |>
  mutate(iso3 = "AVE",
         grp = ifelse(group_name == "global", "ave1", "ave2"),
         c.notified = NA,
         g_whoregion = group_name,
         year = NA) |>
  # merge with regional names
  left_join(who_region_shortnames, by = "g_whoregion") |>
  mutate(country = ifelse(is.na(entity), "Global", as.character(entity)))
  
# include both the first and repeat surveys
f5.2.4a_data <- f5.2.4_data %>% 
  filter(grp=='overall') %>% 
  mutate(year=ifelse(year>report_year-1,report_year-1,year)) 

notification %>% select(iso3,year,g_whoregion,c.notified) %>% right_join(f5.2.4a_data,by=c('iso3','year')) -> f5.2.4a_data

f5.2.4a_data <- f5.2.4a_data |>
  rbind(select(pooled_catst_costs_average, iso3, year, g_whoregion, c.notified, country, grp, catast_pct:catast_pct_hi))

f5.2.4_sel_order <- 
  f5.2.4a_data %>% 
  arrange(catast_pct) %>% 
  arrange(grp) %>%
  mutate(country_a = ifelse(iso3=="ZMB","Zambia",
                            ifelse(iso3=="NAM","Namibia",country))) %>% 
  mutate(country = factor(country),
         country_a = factor(country_a)) 

## Subset data for DS-TB* only the latest surveys
f5.2.4b_data <- f5.2.4_data %>% 
  filter(grp=='TB (first-line treatment)'|iso3=="TLS"|iso3=="SLV"|iso3=="FJI"|iso3=="SLB") %>% 
  arrange(iso3, desc(year)) |>
  group_by(iso3) |>
  slice(1) |>
  ungroup() |>
  mutate(year=ifelse(year>report_year-1,report_year-1,year))

notification %>% select(iso3,year,c_ds) %>% inner_join(f5.2.4b_data,by=c('iso3','year')) %>% distinct() -> f5.2.4b_data 

fit_all <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = f5.2.4b_data, 
    weights = c_ds
  )

pooled_catst_costs_average <- data.frame(
  iso3="AVE",country="Pooled average", 
         year = report_year, c_ds = NA,
          grp="ave",
          catast_pct    = as.numeric(fit_all$b),
          catast_pct_lo = fit_all$ci.lb,
          catast_pct_hi = fit_all$ci.ub) 
  # mutate(country = ifelse(country=="Namibia","Namibia\u1D9C,\u1D48",country)) %>%
  # mutate(country=factor(country,levels=rev(country))) #%>%
  # add_row(iso3="NAM",grp="TB (first-line treatment)",country="Namibia\u1D9C,\u1D48") 

# all survey results incl the first and repeats
f5.2.4b_data <- f5.2.4_data %>% 
  filter(grp=='TB (first-line treatment)'|iso3=="TLS"|iso3=="SLV"|iso3=="FJI"|iso3=="SLB") %>% 
  mutate(year=ifelse(year>report_year-1,report_year-1,year))

notification %>% select(iso3,year,c_ds) %>% inner_join(f5.2.4b_data,by=c('iso3','year')) %>% distinct() -> f5.2.4b_data 
  
f5.2.4b_data <- f5.2.4b_data %>% 
  mutate(grp=ifelse(grp=="overall","TB (first-line treatment)",grp))

f5.2.4b_data <- f5.2.4b_data |>
  rbind(select(pooled_catst_costs_average, iso3, year,  c_ds, country, grp, catast_pct:catast_pct_hi))


f5.2.4_sel_order_b <- 
  f5.2.4b_data %>% 
  arrange(catast_pct) %>% 
  arrange(desc(grp)) %>%
  mutate(country_a = ifelse(iso3=="ZMB","Zambia",
                            ifelse(iso3=="NAM","Namibia",country))) %>% 
  mutate(country = factor(country),
         country_a = factor(country_a)) 

## Subset data for DR-TB 
f5.2.4c_data <- f5.2.4_data %>% 
  filter(grp=='Drug-resistant TB') %>% 
  arrange(iso3, desc(year)) |>
  group_by(iso3) |>
  slice(1) |>
  ungroup() |>
  mutate(year=ifelse(year>report_year-1,report_year-1,year))

notification %>% select(iso3,year,conf_rrmdr) %>% right_join(f5.2.4c_data,by=c('iso3','year')) %>% distinct() -> f5.2.4c_data

fit_all <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = f5.2.4c_data, 
    weights = conf_rrmdr   
  )

pooled_catst_costs_average <- data.frame(
  iso3="AVE",country="Pooled average", 
  year = report_year, conf_rrmdr = NA,
  grp="ave",
  catast_pct    = as.numeric(fit_all$b),
  catast_pct_lo = fit_all$ci.lb,
  catast_pct_hi = fit_all$ci.ub) 


# all survey results incl the first and repeats
f5.2.4c_data <- f5.2.4_data %>% 
  filter(grp=='Drug-resistant TB') %>% 
  mutate(year=ifelse(year>report_year-1,report_year-1,year))

notification %>% select(iso3,year,conf_rrmdr) %>% right_join(f5.2.4c_data,by=c('iso3','year')) %>% distinct() -> f5.2.4c_data

f5.2.4c_data <- f5.2.4c_data |>
  rbind(select(pooled_catst_costs_average, iso3, year,  conf_rrmdr, country, grp, catast_pct:catast_pct_hi))


f5.2.4c_data <- f5.2.4c_data %>% 
  mutate(country=factor(country,levels=rev(country))) %>%  # factorize in the order of rows 
  add_row(iso3="SLB",grp="Drug-resistant TB",country="Solomon Islands") %>%
  add_row(iso3="FJI",grp="Drug-resistant TB",country="Fiji") %>%
  add_row(iso3="TLS",grp="Drug-resistant TB",country="Timor-Leste") %>%
  add_row(iso3="SLV",grp="Drug-resistant TB",country="El Salvador") 
  # add_row(iso3="NAM",grp="Drug-resistant TB",country="Namibia\u1D9C,\u1D48") 


# extract pooled averages for texts
f5.2.4a_txt <- f5.2.4a_data %>%
  subset(iso3=="AVE" & grp == "ave1") %>%
  mutate(grp="Overall\n(End TB Strategy indicator)") 

f5.2.4a_txt_lo <- f5.2.4a_data %>%
  arrange(catast_pct) %>%
  slice(1)

f5.2.4a_txt_hi <- f5.2.4a_data %>%
  arrange(desc(catast_pct)) %>%
  slice(1)

f5.2.4a_txt_num <- f5.2.4a_data %>%
  filter(iso3!="AVE") |>
  mutate(country = stringr::str_remove(country, "\\s*\\(\\d{4}\\)$")) |>
  select(country) |>
  distinct()

f5.2.4c_txt <- f5.2.4c_data %>%
  subset(iso3=="AVE") %>%
  mutate(grp="Drug-resistant TB") 

f5.2.4c_txt_num <- f5.2.4c_data %>%
  filter(iso3!="AVE", !is.na(catast_pct)) |>
  mutate(country = stringr::str_remove(country, "\\s*\\(\\d{4}\\)$")) |>
  select(country) |>
  distinct()



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.5 ----
# (Distribution of costs faced by TB patients and their households in 25 national surveys completed since 2016)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
palatte_f5.2.5 = c("goldenrod2","dodgerblue1","darkblue")

load(here::here('data/pcs/pcs_cost_driver_data.Rdata'))

f5.2.5_data <- f5.2.5_data %>%
  mutate(country = as.character(country)) 

f5.2.5_sel_order <- 
  f5.2.5_data %>% 
  filter(cat == "p_med") %>% 
  arrange(value) %>% 
  mutate(country = factor(country))

f5.2.5_txt_num <- f5.2.5_data %>%
  filter(cat == "p_med")

f5.2.5_txt_med <- f5.2.5_data %>%
  filter(cat == "p_med", value > 20) %>%
  arrange(as.character(country)) |>
  inner_join(en_name, by = "iso3") 

f5.2.5_txt_med_list <- f5.2.5_txt_med %>% nrow()

f5.2.5_txt_nmed <- f5.2.5_data %>%
  filter(cat == "p_nmed", value > 50) %>%
  arrange(as.character(country)) |>
  inner_join(en_name, by = "iso3") 

f5.2.5_txt_nmed_list <- f5.2.5_txt_nmed %>% nrow()

f5.2.5_txt_indirect <- f5.2.5_data %>%
  filter(cat == "p_indirect", value > 43.6) %>% # find cutoff indirect > nmed
  arrange(as.character(country)) |>
  inner_join(en_name, by = "iso3") |>
  mutate(country_in_text_EN = if_else(iso3 == "VNM", paste0(country_in_text_EN, " (", year, ")"), country_in_text_EN))

f5.2.5_txt_indirect_list <- f5.2.5_txt_indirect %>% nrow()

# write csv for GTB Database 
f5.2.5_data %>%
  mutate(value = ftb(value)) %>%
  filter(#iso3 == "ZMB" | 
           iso3 == "NAM" ) %>% # filter countries with updates: 2023, Zambia and Namibia
  write_csv(paste0(here::here('report/local/'),"/tbpcs_cost drivers_update",report_year,".csv")) 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.6 ----
# (Model-based estimates of cost faced by TB patients and their households in 135 low- and middle-income countries, WHO regions)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# load(here::here('data/pcs/pcs_modelling_data.Rdata'))
# 
# f5.2.6_txt_global <- f5.2.6_data %>%
#   filter(entity=="All LMICs")
#   
# f5.2.6_txt_afro <- f5.2.6_data %>%
#   filter(entity=="African Region")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 5.2.6----
# (Status of social protection, pooled average weighted by tb notifications)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sp_col <- c("social_protn","free_access_tbdx","free_access_tbtx",
            "enable_tx_adherence","cash_trans",
            "food_security","income_loss",  "gov_funding_cash_trans", "gov_funding_tx_enablers")

sp1 <- sp |>
  filter(year==report_year) |> 
  select(country, iso3, g_whoregion, year, sp_col)

sp1 <- sp1 |>
  mutate(across(any_of(sp_col), ~ ifelse(. == dnk, no, .))) %>%
  mutate(across(free_access_tbdx:income_loss, ~ ifelse(social_protn == 0 & is.na(.), 0, .)))

f5.2.6_txt_sp_num <- sp1 |>
  filter(year==report_year) |> 
  filter(!is.na(social_protn)&social_protn!=0) |> nrow()

f5.2.6_data_global <- sp1 |>
  group_by(year) |>
  summarise(across(sp_col, sum, na.rm=TRUE)) |>
  ungroup() |>
  mutate(entity = "Global")

f5.2.6_data_region <- sp1 |>
  group_by(year, g_whoregion) |>
  summarise(across(sp_col, sum, na.rm=TRUE)) |>
  ungroup() |>
  arrange(g_whoregion, year) |>
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(-g_whoregion)


f5.2.6_data_hbc <- sp1 |>
  filter(iso3 %in% list_hbcs$iso3) |>
  group_by(year) |>
  summarise(across(sp_col, sum, na.rm=TRUE)) |>
  ungroup() |>
  mutate(entity = "High TB burden countries")

f5.2.6_data <- rbind(f5.2.6_data_global,f5.2.6_data_region,f5.2.6_data_hbc) |>
  select(-social_protn)

f5.2.6_txt <- f5.2.6_data |> slice(1)

col <- c("#084EA2","#0491D1","#eeb422","#ED1D24","#B92270","#91A93E")

f5.2.6_data_long <- f5.2.6_data |>
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region", "High TB burden countries")))  |>
  pivot_longer(cols = free_access_tbdx:income_loss,
               names_to = "sp",
               values_to = "value") |>
  mutate(year = as.factor(year),
         sp = factor(sp, levels = c("free_access_tbdx","free_access_tbtx",
                                    "enable_tx_adherence","cash_trans",
                                    "food_security","income_loss"),
                     labels = c("Free access to\nTB diagnosis", "Free access to\nTB treatment",
                                "Enablers to adhere to\nTB treatment", "Cash transfers",
                                "Measures to ensure\nfood security", "Measures to compensate\nfor income loss"))) |>
  mutate(sp = fct_rev(sp)) |>
  mutate(color = col[(match(sp, unique(sp)) - 1) %% length(col) + 1])

f5.2.6_txt_global <- sp1 |>
  filter(!is.na(social_protn)) |> nrow()

f5.2.6_txt_region <- sp1 %>%
  filter(!is.na(social_protn)) %>%
  count(g_whoregion)

f5.2.6_txt_hbc <- sp1 |>
  filter(iso3 %in% list_hbcs$iso3) |> filter(!is.na(social_protn)) |> nrow()

f5.2.6_txt_hbc_income <- sp1 |>
  filter(iso3 %in% list_hbcs$iso3) |> filter(income_loss == 1) |> nrow()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 5.2.7----
# (Status of social protection )
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
load(here::here('data/ilo/ilo.rda'))

f5.2.7_data <- ilo |>
  group_by(iso3) |>
  filter(!is.na(obs_value) & classif1 == "SOC_CONTIG_TOTAL" & sex == "SEX_T") |>
  slice(1) |>
  mutate(year = year(time)) |>
  select(iso3,year,obs_value, hbc) |>
  # Assign the categories for the map
  mutate(var = cut(obs_value,
                   c(0, 20, 40, 60, 80, Inf),
                   c('0\u201319','20\u201339','40\u201359','60\u201379','\u226580'),
                   right=FALSE)) |>
  ungroup()


f5.2.7_txt_lo <- f5.2.7_data |>
  arrange(year) |>
  slice(1)

f5.2.7_txt_hi <- f5.2.7_data |>
  arrange(desc(year)) |>
  slice(1)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 5.2.8----
# (Status of social protection )
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f5.2.8_data <- f5.2.7_data |>
  filter(hbc == 1 | iso3 == "X01" |iso3 == "X02" |iso3 == "X03" |iso3 == "X04" |iso3 == "X05") |>
  left_join(grpmbr |> filter(group_type == "g_hb_tb") |> select(iso3, country), by = c("iso3")) |>
  arrange(hbc, desc(obs_value), desc(iso3)) |>
  arrange(iso3 == "X01") |>
  add_row(iso3 = "  ", country = "  ", obs_value=-1, .after = 28) |>
  mutate(country = ifelse(iso3 == "X05", "High-income countries",
                          ifelse(iso3 == "X04", "Upper-middle-income countries", 
                                 ifelse(iso3 == "X03", "Lower-middle-income countries",
                                        ifelse(iso3 == "X02", "Low-income countries",
                                               ifelse(iso3 == "X01", "Global", country)))))) |>
  
  # The dataframe is in the order I want, so make entity an ordered factor based on
  # what I already have. That way ggplot will not reorder by entity name
  # But I need to reverse order for plotting
  
  mutate(country = factor(country,
                         levels = country))


f5.2.8_txt <- grpmbr |> 
  filter(group_type == "g_hb_tb") |> 
  select(iso3, country) |>
  filter(!iso3 %in% f5.2.8_data$iso3) |>
  arrange(country) |>
  inner_join(en_name, by = "iso3") 





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 5.2.x----
# (Status of social protection, pooled average weighted by tb notifications)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Subset data for All TB to estimate pooled average

f5.2.x_data_raw <- f5.2.8_data |>
  mutate(year=ifelse(year>report_year-1,report_year-1,year)) 

notification %>% select(iso3, year, g_whoregion,c_newinc) |>
  filter(year == report_year - 1) %>% right_join(f5.2.x_data_raw,by=c('iso3')) -> f5.2.x_data_raw

f5.2.x_data_raw <- f5.2.x_data_raw |>
  filter(!is.na(g_whoregion )) |> 
  select(!var)

f5.2.x_data_region <- f5.2.x_data_raw |>
  filter(!is.na(obs_value), !is.na(c_newinc)) |>
  group_by(g_whoregion) %>%
  summarise(
    ave = sum(obs_value * c_newinc ) / sum(c_newinc),
    c_newinc = sum(c_newinc)
  ) |>
  ungroup() |>
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(!g_whoregion)

f5.2.x_data_global <- f5.2.x_data_raw %>%
  filter(!is.na(obs_value), !is.na(c_newinc)) %>% 
  summarise(
    ave = sum(obs_value * c_newinc ) / sum(c_newinc),
    c_newinc = sum(c_newinc)
  ) |>
  mutate(entity = 'Global')

# Add global to the regional aggregates
f5.2.x_data <- rbind(f5.2.x_data_region, f5.2.x_data_global) |>
  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global","Western Pacific Region","Eastern Mediterranean Region", "European Region", "South-East Asia Region",
                                    "Region of the Americas", "African Region"))) |>
  arrange(rev(entity))

# f5.2.x_data_raw <- f5.2.x_data_raw |>
#   rename(year = year.x, year_sp = year.y)
# 
# write.csv(f5.2.x_data_raw, here::here("./local/weighted_sp.csv"), row.names = F)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 5.2.9 ----
# (Status of social protection: goverment expenditure )
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

load(here::here('data/ilo/sp_exp.rda'))

f5.2.9_data <- sp_exp %>%
  group_by(iso3) |>
  filter(!is.na(sp)) |>
  # filter(!is.na(CC.SP.EXP.ZS)) |>
  # slice(1) |>
  select(iso3, year, value = sp ) |>
  # Assign the categories for the map
  mutate(var = cut(value,
                   c(0, 5, 10, 15, Inf),
                   c('0\u20134','5\u20139','10\u201314','\u226515'),
                   right=FALSE)) |>
  ungroup()

f5.2.9_txt <- f5.2.9_data |>
  filter(value > 15 & !is.na(year))
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 5.2.10 ----
# (Status of social protection: goverment expenditure )
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f5.2.10_data <- sp_exp |>
  select(!country) |>
  filter(hbc == 1 | iso3 == "X01" |iso3 == "X02" |iso3 == "X03" |iso3 == "X04" |iso3 == "X05") |>
  left_join(grpmbr |> filter(group_type == "g_hb_tb") |> select(iso3, country), by = c("iso3")) |>
  arrange(hbc, desc(sp ), desc(iso3)) |>
  arrange(iso3 == "X01") |>
  add_row(iso3 = "  ", country = "  ", sp=-1, .after = 28) |>
  mutate(country = ifelse(iso3 == "X05", "High-income countries",
                          ifelse(iso3 == "X04", "Upper-middle-income countries", 
                                 ifelse(iso3 == "X03", "Lower-middle-income countries",
                                        ifelse(iso3 == "X02", "Low-income countries",
                                               ifelse(iso3 == "X01", "Global", country)))))) |>
  
  # The dataframe is in the order I want, so make entity an ordered factor based on
  # what I already have. That way ggplot will not reorder by entity name
  # But I need to reverse order for plotting
  
  mutate(country = factor(country,
                          levels = country))

f5.2.10_txt <- grpmbr |> 
  filter(group_type == "g_hb_tb") |> 
  select(iso3, country) |>
  arrange(country) |>
  filter(!iso3 %in% f5.2.10_data$iso3) |>
  inner_join(en_name, by = "iso3") 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 5.2.11 ----
# (Status of national laws and regulations against stigma)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

stigma <- read_csv(here::here("data/amend/stigma.csv"))

stigma_col <- c("protect_employment","protect_housing",
            "protect_parenting","protect_movement",
            "protect_association")

f5.2.11_country_data <- stigma |>
  filter(year==report_year-2) %>% 
  select(iso3, all_of(stigma_col))

f5.2.11_country_data <- f5.2.11_country_data %>%
  mutate(across(all_of(stigma_col), ~ ifelse(. == dnk, no, .))) 


## for map
f5.2.11_txt_num <- f5.2.11_country_data %>%
  filter(!is.na(protect_employment)&!is.na(protect_housing)&!is.na(protect_parenting)&!is.na(protect_movement)&!is.na(protect_association)) %>%
  mutate(protect_n = protect_employment+protect_housing+protect_parenting+protect_movement+protect_association) %>% 
  filter(protect_n != 0) %>%
  nrow()

f5.2.11a_data <- f5.2.11_country_data %>%
  select(iso3,protect_employment) %>%
  # Assign the categories for the map
  mutate(var = factor(protect_employment,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.11a = c("#2171B5","#C6DBEF")

f5.2.11a_txt_num <- f5.2.11a_data %>%
  filter(!is.na(protect_employment)&protect_employment!=0) %>% nrow()

f5.2.11b_data <- f5.2.11_country_data %>%
  select(iso3,protect_housing) %>%
  # Assign the categories for the map
  mutate(var = factor(protect_housing,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.11b = c("#99000D","#FCBBA1")

f5.2.11b_txt_num <- f5.2.11b_data %>%
  filter(!is.na(protect_housing)&protect_housing!=0) %>% nrow()

f5.2.11c_data <- f5.2.11_country_data %>%
  select(iso3,protect_parenting) %>%
  # Assign the categories for the map
  mutate(var = factor(protect_parenting,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.11c = c("#238B45","#C7E9C0")

f5.2.11c_txt_num <- f5.2.11c_data %>%
  filter(!is.na(protect_parenting)&protect_parenting!=0) %>% nrow()

f5.2.11d_data <- f5.2.11_country_data %>%
  select(iso3,protect_movement) %>%
  # Assign the categories for the map
  mutate(var = factor(protect_movement,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.11d = c("#D94801","#FDD0A2")

f5.2.11d_txt_num <- f5.2.11d_data %>%
  filter(!is.na(protect_movement)&protect_movement!=0) %>% nrow()

f5.2.11e_data <- f5.2.11_country_data %>%
  select(iso3,protect_association) %>%
  # Assign the categories for the map
  mutate(var = factor(protect_association,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.11e = c("#6A51A3","#DADAEB")

f5.2.11e_txt_num <- f5.2.11e_data %>%
  filter(!is.na(protect_association)&protect_association!=0) %>% nrow()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch5-1.rmd
# Mathieu Bastard, July 2026
# Takuya Yamanaka
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load chapter 5 packages, settings and data
source(here::here('report/ch5_load_data.r'))
est_date <- attr(est, which = "timestamp")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.1.1 ----
# (Trends in the UHC service coverage index in WHO regions and World Bank income groups, 2000-2019)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

palatte_fig5.1.1a = c("#6363c0","#f26829","#40bf73","#008dc9","#bd53bd","#f4a81d","navy") 

# new colour standard for WHO regions:https://apps.who.int/gho/data/design-language/design-system/colors/

# AFRO 0, 89, 88, 1   #6363c0
# AMRO 37, 7, 91, 11 #f26829
# EURO 0, 45, 88, 0 #bd53bd 
# EMRO 78, 2, 72, 2 #008dc9 
# SEARO 71, 27, 1, 0 #40bf73 
# WPRO 13, 94, 9, 8 #f4a81d 
# Global 93, 70, 0, 1 #124CFC 

f5.1.1a_data <-
  read.csv(here::here("./data/datahub/9A706FD_ALL_LATEST.csv")) %>% # the last update is 8 Jan 2024: https://data.who.int/indicators/i/3805B1E/9A706FD
  dplyr::rename(who_reg=DIM_GEO_CODE_M49,year=DIM_TIME,SCI=INDEX_N) %>%
  dplyr::select(who_reg,year,SCI) %>%
  filter(grepl("^95", who_reg)|who_reg==1) %>%
  mutate(who_reg=factor(dplyr::recode(who_reg,`953`="African Region",
                               `954`="Region of the Americas",
                               `957`="Eastern Mediterranean Region",
                               `956`="European Region",
                               `955`="South-East Asia Region",
                               `958`="Western Pacific Region",
                               `1`="GLOBAL"),
                        levels=c("African Region",
                                 "Region of the Americas",
                                 "South-East Asia Region",
                                 "European Region",
                                 "Eastern Mediterranean Region",
                                 "Western Pacific Region","GLOBAL"))) 
# (a) WHO regions
f5.1.1a_data <- 
  f5.1.1a_data %>%
  mutate(year=as.numeric(year)) %>% 
  arrange(who_reg) %>%
  group_by(who_reg) %>%
  group_modify(~ add_row(year=c(2001:2004,2006:2009,2011:2014,2016,2018,2020,2022), SCI=NA, .x)) %>% # add years for missing data points
  ungroup()

f5.1.1a_data <- subset(f5.1.1a_data, !is.na(SCI))

## text!
f5.1.1a_txt <- f5.1.1a_data %>%
  filter(who_reg == "GLOBAL", year == 2000|year == 2019|year == 2021 |year == 2022 |year == 2023) %>%
  pivot_wider(names_from = "year", values_from = "SCI") %>%
  rename(sci_2023 = `2023`, sci_2021 = `2021`, sci_2019 = `2019`, sci_2000 = `2000`)

f5.1.1a_txt <- f5.1.1a_data %>%
  filter((who_reg == "Region of the Americas"|who_reg == "African Region"|who_reg == "European Region"|who_reg == "Western Pacific Region") & year == 2023) %>%
  pivot_wider(names_from = "who_reg", values_from = "SCI") %>%
  rename(afro = 2, amro = 3, euro = 4, wpro = 5) %>%
  select(-year) %>%
  cbind(f5.1.1a_txt)

# (b) WB income classifications
palatte_fig5.1.1b = c("#66D6FF","#E63E13","#8FD314","#814550","#124CFC") 
#                
# High income 60, 16, 0, 0 #66D6FF 
# UMI 3, 74, 92, 7 #E63E13 
# LMI 37, 7, 91, 11 #8FD314 
# LI 42, 69, 64, 13 #814550 
# Global 93, 70, 0, 1 #124CFC 

f5.1.1b_data <- 
  read.csv(here::here("./data/datahub/9A706FD_ALL_LATEST.csv")) %>%
  dplyr::rename(income_group=DIM_GEO_CODE_M49,year=DIM_TIME,SCI=INDEX_N) %>%
  dplyr::select(income_group,year,SCI) %>%
  filter(grepl("^91", income_group)|income_group==1) 

f5.1.1b_data <- f5.1.1b_data %>%
  mutate(income_group=factor(dplyr::recode(income_group,`910`="High-income",
                                    `914`="Upper-middle-income",
                                    `912`="Lower-middle-income",
                                    `911`="Low-income",
                                    `1`="GLOBAL"),
    levels=c("High-income","Upper-middle-income","Lower-middle-income",
             "Low-income","GLOBAL"))) 

f5.1.1b_data <- 
  f5.1.1b_data %>%
  mutate(year=as.numeric(year)) %>% 
  arrange(income_group)  %>%
  group_by(income_group) %>%
  group_modify(~ add_row(year=c(2001:2004,2006:2009,2011:2014,2016,2018,2020,2022), SCI=NA, .x)) %>% # add years for missing data points
  ungroup()

f5.1.1b_data <- subset(f5.1.1b_data, !is.na(SCI))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.1.3 ----
# (UHC service coverage index by country, 2019)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

palatte_fig5.1.3 = c("#B11219","#ED1D24","#D2745B","#D98D93","#FDCEB8","#F8E5CF") 

f5.1.3_data <- 
  read.csv(here::here("./data/datahub/9A706FD_ALL_LATEST.csv")) %>%
  dplyr::rename(code=DIM_GEO_CODE_M49, country = GEO_NAME_SHORT, year=DIM_TIME,value=INDEX_N) %>%
  dplyr::select(code,country,year,value) %>%
  left_join(list_iso3_country) %>%
  filter(!is.na(iso3))%>%

  group_by(iso3) %>%
  slice(which.max(year)) %>%  ## select latest available year
  # slice_max(year) %>% 
  mutate(var=cut(value,breaks=c(0,40,50,60,70,80,100),right = FALSE,
                 labels=c("<40","40\u201349","50\u201359","60\u201369","70\u201379","\u226580")))  %>%
  ungroup(iso3)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.1.4 ----
# (Global and regional trends of percentage of the general population facing catastrophic health expenditure)
# Updates in 2026 with data updated on 19/11/2025
# https://www.who.int/data/gho/data/themes/topics/financial-protection
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f5.1.4_data_region <- 
  read.csv(here::here("./data/gho/fin_hardship_region.csv")) %>% 
  select(iso3=SpatialDimValueCode,value=FactValueNumeric,year=Period) %>%
  mutate(who_reg=factor(dplyr::recode(iso3,"AFR"="African Region",
                               "AMR"="Region of the Americas",
                               "EMR"="Eastern Mediterranean Region",
                               "EUR"="European Region",
                               "SEAR"="South-East Asia Region",
                               "WPR"="Western Pacific Region",
                               "GLOBAL"="GLOBAL"),
                        levels=c("African Region",
                                 "Region of the Americas",
                                 "South-East Asia Region",
                                 "European Region",
                                 "Eastern Mediterranean Region",
                                 "Western Pacific Region","GLOBAL"))) %>%
  group_by(who_reg) %>%
#  group_modify(~ add_row(year=c(2001:2004,2006:2009,2011:2014,2016,2018), value=NA, .x)) %>% # add years for missing data points
  ungroup()
  

f5.1.4_data_global <- 
  read.csv(here::here("./data/gho/fin_hardship_global.csv")) %>% 
  #  subset(Dim1=="Total") %>%
  select(iso3=SpatialDimValueCode,value=FactValueNumeric,year=Period) %>%
  mutate(who_reg=factor(dplyr::recode(iso3,"GLOBAL"="GLOBAL"),
                        levels=c("GLOBAL")))

f5.1.4_data <- rbind(f5.1.4_data_region,f5.1.4_data_global)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.1.5 ----
# (Percentage of the general population facing catastrophic health expenditure)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

palette_fig5.1.5 <- colorRampPalette(c("#E0F2FE", "#BAE6FD", "#7DD3FC", "#38BDF8", "#0284C7", "#0369A1", "#075985"))(7)
f5.1.5_data <- 
  read.csv(here::here("./data/gho/fin_hardship_country.csv")) %>%
  subset(Dim1=="Total") %>%
  select(country=Location,iso3=SpatialDimValueCode,value=FactValueNumeric,year=Period) %>%
  group_by(iso3) %>%
  slice(which.max(year)) %>%  ## select latest available year
  ungroup() %>% 
  mutate(var=cut(value,breaks=c(0,3,6,9,12,15,30,100),right = FALSE,
                 labels=c("<\u20093","3\u20135","6\u20138","9\u201311","12\u201314","15\u201329" ,"\u226530")))


f5.1.5_txt_list <- f5.1.5_data %>%
  filter(iso3 %in% iso3_hbc) %>%
  filter(value>=30)  

f5.1.5_txt <- f5.1.5_data %>%
  filter(iso3 %in% iso3_hbc) %>%
  filter(value>=30) %>% nrow() 
 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.1.6 ----
# (UHC service coverage index (SDG 3.8.1))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

label_income <- c("Low income"="Low-income","Lower-middle income"="Lower-middle-income","Upper-middle income"="Upper-middle-income")

# import raw data of 3.8.1 UHC service index
f5.1.6_data_381 <-  read.csv(here::here("./data/datahub/9A706FD_ALL_LATEST.csv")) %>%
  dplyr::rename(code=DIM_GEO_CODE_M49, country = GEO_NAME_SHORT, year=DIM_TIME,value_381=INDEX_N) %>%
  dplyr::select(country,year,value_381) %>%
  left_join(list_iso3_country) %>%
  filter(!is.na(iso3)) %>%
  select(!country) %>%
  group_by(iso3) %>% 
#  group_modify(~ add_row(year=c(2001:2004,2006:2009,2011:2014,2016,2018,2020), value_381=NA, .x)) %>% # add years for interpolation
  arrange(iso3,year)

f5.1.6_data_381 <- f5.1.6_data_381 %>%
  group_by(iso3) %>%
  mutate(value_381_interp = na.approx(value_381, na.rm=FALSE)) %>% # simple interpolation for years with missing data
  ungroup()  %>%
  filter(iso3 %in% iso3_hbc) %T>%
  write.csv(here('report/local/uhc_interpolated_gho_2023.csv'))

f5.1.6_data_381 <- subset(f5.1.6_data_381, !is.na(value_381))

# for HGF data check
# f5.1.6_data_381 <- f5.1.6_data_381  %>%
#   filter(iso3 %in% iso3_hbc) 

# uhc_index <- 
#   sdg %>% filter(indicator_id == "UHC_INDEX_REPORTED", sex=="a") %>%
#   group_by(iso3,country) %>% 
#   group_modify(~ add_row(year=c(2001:2004,2006:2009,2011:2014,2016,2018,2020), value=NA, .x)) %>% # add years for interpolation
#   arrange(iso3,year)
# 
# uhc_index <- uhc_index %>%
#   group_by(iso3) %>%
#   mutate(value_interpolated = zoo::na.approx(value, na.rm=FALSE)) %>% # simple interpolation for years with missing data
#   ungroup() %>%
#   filter(iso3 %in% iso3_hbc_plus_wl) %>%
#   select(iso3,country,year,value,value_interpolated) %T>%
#   write.csv(here('report/local/uhc_interpolated_2023.csv'))

# import raw data of 3.8.2 Catastrphic health expenditure with 10% definition
f5.1.6_data_382  <- read.csv(here::here('./data/gho/fin_hardship_country.csv')) %>%
  subset(Dim1=="Total") %>%
  rename(iso3=SpatialDimValueCode) %>%
  group_by(iso3) %>%
  filter(Period < report_year-1) %>% 
  filter(Period == max(Period)) %>%
  select(iso3,year=Period,value_382=Value) %>%
  filter(iso3 %in% iso3_hbc) 

f5.1.6_data <- list_hbcs %>%
  left_join(f5.1.6_data_382, by=c("iso3")) %>%
  left_join(f5.1.6_data_381, by=c("iso3","year"))

# import raw data of historical WB country income classification
f5.1.6_data_income_income <-  read_excel(here::here('data/wb/OGHIST.xlsx'),sheet="Country Analytical History_wide" ) %>%
  pivot_longer(cols=`1987`:`2025`, names_to = c("year"), values_to = "income_class") %>%
  filter(year>=2000) %>%
  select(!country) %>%
  mutate(year=as.numeric(year))

f5.1.6_data <- f5.1.6_data %>%
  left_join(f5.1.6_data_income_income, by=c("iso3","year")) %>%
  select(iso3,country,year,value_381,value_381_interp,value_382,income_class) %>%
  arrange(iso3)

f5.1.6_data <- f5.1.6_data %>% 
  mutate(income_class=ifelse(income_class=="L","Low-income",
                             ifelse(income_class=="LM","Lower-middle-income",
                                    ifelse(income_class=="UM","Upper-middle-income", "High-income"
                                    ))))


f5.1.6_data <- f5.1.6_data %>% 
  rename(uhc_index=value_381_interp,uhc_ce10=value_382,g_income=income_class)


f5.1.2_data <- f5.1.6_data_381 %>%
  filter(iso3 %in% iso3_hbc) %>%
  left_join(list_hbcs, by = c("iso3")) %>%
  mutate(country = ifelse(country == "Democratic People's Republic of Korea", "Democratic People's\nRepublic of Korea",
                          ifelse(country == "Democratic Republic of the Congo", "Democratic Republic\nof the Congo",
                                 country
                          )))

# Change in SCI since 2023

country_changes <- f5.1.2_data %>%
  filter(year %in% c(2000, 2023)) %>%
  select(iso3, country, year, value_381) %>%
  tidyr::pivot_wider(
    names_from = year, 
    values_from = value_381, 
    names_prefix = "yr_"
  ) %>%
  mutate(
    abs_diff = yr_2023 - yr_2000
  ) %>%
  filter(abs_diff >= 30) %>% 
  arrange(desc(abs_diff))

formatted_entries <- sprintf("%s (+%d)", country_changes$country, round(country_changes$abs_diff))

n <- length(formatted_entries)
if (n == 0) {
  result_string <- "No countries met the criteria."
} else if (n == 1) {
  result_string <- formatted_entries
} else if (n == 2) {
  result_string <- paste(formatted_entries, collapse = " and ")
} else {
  result_string <- paste0(
    paste(formatted_entries[1:(n - 1)], collapse = ", "), 
    " and ", 
    formatted_entries[n]
  )
}


cat(result_string, "\n")


  
f5.1.2_txt_list <- f5.1.2_data %>%
  select(country, value = value_381, year) %>%
  filter(year==2000 | year==2019) %>%
  pivot_wider(names_from = year,
              values_from = value, 
              names_prefix = "sci_"
              ) %>%
  mutate(prog = sci_2019 - sci_2000) %>%
  filter(prog>=30)

f5.1.2_txt_list_hi <- f5.1.2_data %>%
  select(country, value = value_381, year) %>%
  filter(year==2021) %>%
  filter(value>79)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.1.7 ----
# (Current health expenditure per capita, 30 high TB burden countries, 2000-20)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# f5.1.7_data <-
#   sdg %>% filter(iso3 %in% iso3_hbc) %>%
#   filter(indicator_id=="SH.XPD.CHEX.PP.CD") %>% # from 2023, the data source is switched to WB (GHO does not provide PPP)
#   arrange(country) %>%
#   left_join(list_hbcs_income) %>%
#   select(iso3,year,value,country,g_income)

# until sdg dataset is updated usually in July, download the data from GHED (https://apps.who.int/nha/database/Select/Indicators/en) and use variable of "che_ppp_pc" for producing the figure. 


library(readxl)
library(dplyr)

f5.1.7_data <-
  read_xlsx(here::here("./data/nhe/GHED_data.xlsx")) 

f5.1.7_data <- f5.1.7_data %>%
  dplyr::select(country=location,iso3=code,income,year,value=che_ppp_pc) %>%
  filter(iso3 %in% iso3_hbc) %>%
  mutate(g_income = ifelse(income=="Low","LIC",
                           ifelse(income=="Lower-middle","LMC",
                                  ifelse(income=="Upper-middle","UMC","HIC")))) %>%
  filter(year <= report_year-2) %>%
  mutate(country = ifelse(country == "Democratic People's Republic of Korea", "Democratic People's\nRepublic of Korea",
                          ifelse(country == "Democratic Republic of the Congo", "Democratic Republic\nof the Congo",
                                 country
                          )))

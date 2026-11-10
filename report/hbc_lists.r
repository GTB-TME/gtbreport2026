# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Generate graphs for high burden lists based on the estimates
# from the 2025 global TB report
# Hazim Timimi October 2020
# Updated by Mathieu Bastard, September 2025
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# load libraries
library(RODBC)
library(dplyr)
library(ggplot2)
library(gtbreport)
library(here)
library(ggthemes)
library(scales)


# KILL factors grrrrr
options(stringsAsFactors = FALSE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Extract historical estimates for a given report/date
# and associated variable names with the report year
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# plot_with_ui <- function(df, title, outputname, start_row, end_row){
# 
#   # plot variable with uncertainty intervals
#   # Assumes df has 4 variables called country, var, var_lo, var_hi
# 
#   p <- arrange(df, desc(var)) %>%
#         slice(start_row:end_row) %>%
#         ggplot(aes(x=reorder(country,var), y=var)) +
#           geom_point() +
#           labs(x="", y="") +
#           geom_errorbar(aes(ymin=var_lo, ymax=var_hi)) +
#           theme_gtb()+
#           coord_flip() +
#           ggtitle(title)
# 
#   print(p)
#   ggsave(filename = paste0(outputname, Sys.Date(),".png"),
#          width=8, height=11)
# 
# }

plot_with_ui<- function(df, title, outputname, start_row, end_row) {
  
  # Ensure the data has the correct columns
  required_cols <- c("country", "var", "var_lo", "var_hi")
  if (!all(required_cols %in% names(df))) {
    stop("The dataframe must contain 'country', 'var', 'var_lo', and 'var_hi' columns.")
  }
  
  p <- df %>%
    arrange(desc(var)) %>%
    slice(start_row:end_row) %>%
    ggplot(aes(x = reorder(country, var), y = var)) +
    
    theme_minimal() +
    
    geom_point(size = 3, color = "#2c7bb6") +
    
    geom_errorbar(aes(ymin = var_lo, ymax = var_hi), width = 0.2, color = "#abd9e9", size = 1) +
    
    labs(
      x = "",
      y = "Value",
      title = title,
      subtitle = "with 95% Uncertainty Intervals"
    ) +
    
    # Adjust font sizes and colors for better readability
    theme(
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle = element_text(size = 14, color = "gray50"),
      plot.caption = element_text(size = 10, color = "gray50"),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10),
      panel.grid.major.y = element_blank(), # Remove horizontal grid lines
      panel.grid.minor.y = element_blank()
    ) +
    
    # Flip coordinates for a more readable bar chart feel
    coord_flip()+
    scale_y_continuous(labels = scales::comma)
  
  print(p)
  
  ggsave(
    filename = paste0(outputname, "_", Sys.Date(), ".png"),
    plot = p,
    width = 10,
    height = 8,
    dpi = 300 # Increase resolution for a higher quality image
  )
}


source(here("import/load_gtb.R"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get data  -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load from repo
load(here("inc_mort/analysis/est.rda"))
load(here("drtb/output/db_dr_country.rda"))
cty<-load_gtb("cty",convert_dots = FALSE)


# 2024 estimates
est.2024=est[year==2024,]
db_dr_country.2024=db_dr_country[year==2024,]

# merge country names
est.2024=merge(est.2024,cty[,.(iso3,country)],by="iso3")
db_dr_country.2024=merge(db_dr_country.2024,cty[,.(iso3,country)],by="iso3")

# Incidence --------------

# incidence number
est.2024  %>%
  select(country,
         var    = inc.num,
         var_lo = inc.lo.num,
         var_hi = inc.hi.num) %>%
  plot_with_ui("Estimated  number of TB cases in 2024",
               "2024_inc_num_", 1, 30)

# make list of top 20 by number
inc_top_20 <- est.2024 %>%
  arrange(desc(inc.num)) %>%
  slice(1:20) %>%
  select(iso3, country)

# make list of additional top 10 by rate
inc_rate_top_10 <- est.2024 %>%
  filter(inc.num > 10000) %>%
  select(iso3, country, inc) %>%
  anti_join(inc_top_20, by="iso3") %>%
  arrange(desc(inc)) %>%
  slice(1:10) %>%
  select(iso3, country)


# incidence rate (threshold: >10,000 estimated incident TB cases)
# DON'T exclude countries in the top 20 by number
est.2024 %>%
  filter(inc.num > 10000) %>%
  select( country,
          iso3,
          var    = inc,
          var_lo = inc.lo,
          var_hi = inc.hi) %>%
  # # exclude countries in the top 20 by incidence number
  # anti_join(inc_top_20, by="iso3") %>%
  # plot_with_ui("Estimated incidence, 2019\nCases per 100 000 population per year\n(minimum 10 000 estimated cases per year\nand excluding countries in top 20 by number)",
  #              "2019_inc_rate_", 1, 23)
  
  plot_with_ui("Estimated incidence, 2024\nCases per 100 000 population per year\n(minimum 10 000 estimated cases per year)",
               "2024_inc_rate_", 1, 30)

# TB/HIV -----------------------------------------

# tb/hiv incidence number
est.2024  %>%
  select(country,
         var    = inc.h.num,
         var_lo = inc.h.lo.num,
         var_hi = inc.h.hi.num) %>%
  plot_with_ui("Estimated  number of TB cases in people living with HIV in 2024",
               "2024_tbhiv_num_", 1, 30)

# make list of top 20 by number
tbhiv_top_20 <- est.2024 %>%
  arrange(desc(inc.h.num)) %>%
  slice(1:20) %>%
  select(iso3, country)

# make list of additional top 10 by rate
tbhiv_rate_top_10 <- est.2024 %>%
  filter(inc.h.num > 1000) %>%
  select(iso3, country, inc.h) %>%
  anti_join(tbhiv_top_20, by="iso3") %>%
  arrange(desc(inc.h)) %>%
  slice(1:10) %>%
  select(iso3, country)

# tb/hiv incidence rate  (threshold: >1,000 estimated incident TB/HIV cases)
# DON'T exclude countries in the top 20 by number
est.2024  %>%
  filter(inc.h.num > 1000) %>%
  select(country,
         iso3,
         var    = inc.h,
         var_lo = inc.h.lo,
         var_hi = inc.h.hi) %>%
  
  plot_with_ui("Estimated TB incidence rate (/100 000 population) in people living with HIV, 2024\n(minimum 1 000 estimated cases per year)",
               "2024_tbhiv_rate_", 1, 30)


# MDR -------------------------------------------

# Estimated RR-TB number (changed in Nov 2020, before was MDR-TB number)
db_dr_country.2024  %>%
  select(country,
         var    = e_inc_rr_num,
         var_lo = e_inc_rr_num_lo,
         var_hi = e_inc_rr_num_hi) %>%
  plot_with_ui("Estimated number of people who developed MDR/RR-TB in 2024",
               "2024_rr_num_", 1, 30)

# make list of top 20 by number
dr_top_20 <- db_dr_country.2024 %>%
  arrange(desc(e_inc_rr_num)) %>%
  slice(1:20) %>%
  select(iso3, country)

# make list of additional top 10 by rate
dr_rate_top_10 <- est.2024 %>%
  select(iso3, pop) %>%
  inner_join(db_dr_country.2024, by = "iso3") %>%
  filter(e_inc_rr_num > 1000) %>%
  # exclude countries in the top 20 by rr incidence number
  anti_join(dr_top_20, by="iso3") %>%
  mutate(e_inc_rr_100k=e_inc_rr_num*1e5/pop) %>%
  select(iso3, country, e_inc_rr_100k) %>%
  arrange(desc(e_inc_rr_100k)) %>%
  slice(1:10) %>%
  select(iso3, country)



# Estimated RR-TB rate (threshold: >1,000 estimated incident RR-TB cases)
# DON'T exclude countries in the top 20 by number

est.2024 %>%
  select(iso3,pop) %>%
  inner_join(db_dr_country.2024, by = "iso3") %>%
  filter(e_inc_rr_num > 1000) %>%
  # # exclude countries in the top 20 by rr incidence number
  # anti_join(dr_top_20, by="iso3") %>%
  mutate(var    = e_inc_rr_num*1e5/pop,
         var_lo = e_inc_rr_num_lo*1e5/pop,
         var_hi = e_inc_rr_num_hi*1e5/pop) %>%
  select(country,
         iso3,
         var,
         var_lo,
         var_hi) %>%
  # plot_with_ui("Estimated MDR incidence, 2019\ncases per 100 000 population per year\n(minimum 1 000 estimated cases per year\nand excluding countries in top 20 by number)",
  #              "2019_mdr_rate_", 1, 23)
  
  
  plot_with_ui("Estimated RR/MDR-TB incidence (/100 000 population) in 2024\n(minimum 1 000 estimated cases per year)",
               "2024_rr_rate_", 1, 30)


# Create a dataframe of the 3 HBC lists to save to CSV for easy reference ----

hbc <- function(df, hbc_name){
  df %>%
    mutate(hbc_list = hbc_name) %>%
    select(hbc_list, country) %>%
    arrange(country)
}

hbcs_list <- hbc(inc_top_20, "HBC_TB_top_20_num") %>%
  rbind(hbc(inc_rate_top_10, "HBC_TB_top_10_rate")) %>%
  rbind(hbc(tbhiv_top_20, "HBC_TBHIV_top_20_num")) %>%
  rbind(hbc(tbhiv_rate_top_10, "HBC_TBHIV_top_10_rate")) %>%
  rbind(hbc(dr_top_20, "HBC_RR_top_20_num")) %>%
  rbind(hbc(dr_rate_top_10, "HBC_RR_top_10_rate"))

write.csv(hbcs_list,
          file = paste0("2025_hbcs_list_", Sys.Date(), ".csv"),
          row.names = FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Calculate the share of global burden accounted for by each HBC category ----

tot_inc <- est.2024 %>%
  group_by(year) %>%
  summarise(tot_inc=sum(inc.num))

inc_top_20_pct <- est.2024 %>%
  inner_join(inc_top_20, by = "iso3") %>%
  group_by(year) %>%
  summarise(tot_inc=sum(inc.num)) %>%
  select(tot_inc_pct = tot_inc) * 100 / tot_inc$tot_inc

inc_rate_top_10_pct <- est.2024 %>%
  inner_join(inc_rate_top_10, by = "iso3") %>%
  group_by(year) %>%
  summarise(tot_inc=sum(inc.num)) %>%
  select(tot_inc_pct = tot_inc) * 100 / tot_inc$tot_inc



tot_inc_tbhiv  <- est.2024 %>%
  group_by(year) %>%
  summarise(tot_inc_tbhiv=sum(inc.h.num, na.rm = TRUE))

tbhiv_top_20_pct <- est.2024 %>%
  inner_join(tbhiv_top_20, by = "iso3") %>%
  group_by(year) %>%
  summarise(tot_inc=sum(inc.h.num)) %>%
  select(tot_inc_pct = tot_inc) * 100 / tot_inc_tbhiv$tot_inc_tbhiv

tbhiv_rate_top_10_pct <- est.2024 %>%
  inner_join(tbhiv_rate_top_10, by = "iso3") %>%
  group_by(year) %>%
  summarise(tot_inc=sum(inc.h.num)) %>%
  select(tot_inc_pct = tot_inc) * 100 / tot_inc_tbhiv$tot_inc_tbhiv


tot_inc_rr <- db_dr_country.2024 %>%
  group_by(year) %>%
  summarise(tot_inc_rr=sum(e_inc_rr_num, na.rm = TRUE))

rr_top_20_pct <- db_dr_country.2024 %>%
  inner_join(dr_top_20, by = "iso3") %>%
  group_by(year) %>%
  summarise(tot_inc=sum(e_inc_rr_num)) %>%
  select(tot_inc_pct = tot_inc) * 100 / tot_inc_rr$tot_inc_rr

rr_rate_top_10_pct <- db_dr_country.2024 %>%
  inner_join(dr_rate_top_10, by = "iso3") %>%
  group_by(year) %>%
  summarise(tot_inc=sum(e_inc_rr_num)) %>%
  select(tot_inc_pct = tot_inc) * 100 / tot_inc_rr$tot_inc_rr

accounts_for <- cbind(inc_top_20_pct, inc_rate_top_10_pct,
                      tbhiv_top_20_pct, tbhiv_rate_top_10_pct,
                      rr_top_20_pct, rr_rate_top_10_pct)

names(accounts_for) <- c("TB_top_20_num_pct", "TB_top_10_rate_pct",
                         "TBHIV_top_20_num_pct", "TBHIV_top_10_rate_pct",
                         "RR_top_20_num_pct", "RR_top_10_rate_pct")

# write result to CSV
accounts_for %>%
  write.csv(file = paste0("2025_HBC_share_burden", Sys.Date(), ".csv"),
            row.names = FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Determine the epidemiological categorization of countries and write to CSV ----
# This is based on incidence rates:

# severely endemic: e_inc_100k >= 500
# highly endemic: e_inc_100k 300–499
# endemic: e_inc_100k 100–299
# upper-moderate incidence: e_inc_100k 50–99
# lower-moderate incidence: e_inc_100k 10-49
# low incidence: e_inc_100k < 10
# 
# epi_categories <- est %>%
#   select(g_whoregion, country, e_inc_100k, e_inc_num)
# 
# epi_categories$category <- cut(epi_categories$e_inc_100k,
#                           c(0, 9.49, 49.49, 99.49, 299.49, 499.49, Inf),
#                           c("low", "lower-moderate", "upper-moderate", "endemic", "highly endemic", "severely endemic"),
#                           right = FALSE)
# 
# # Write country lists to CSV in order of category (high to low), region and country
# epi_categories %>%
#   select(category, g_whoregion, country) %>%
#   arrange(desc(category), g_whoregion, country) %>%
#   write.csv(file = paste0("epi_categories_", Sys.Date(), ".csv"),
#           row.names = FALSE)
# 
# # Calculate share of global incidence numers accounted for by each category
# 
# epi_cat_shares <- epi_categories %>%
#   group_by(category) %>%
#   summarise(members = n(),
#             incidence = sum(e_inc_num))
# 
# inc_tot <- epi_cat_shares %>%
#   summarise(global_incidence = sum(incidence))
# 
# epi_cat_shares <- epi_cat_shares %>%
#   mutate(share = incidence * 100 / inc_tot$global_incidence)
# 
# # write result to CSV
# epi_cat_shares %>%
#   select(category, members, share) %>%
#   arrange(desc(category), members, share) %>%
#   write.csv(file = paste0("epi_categories_summary", Sys.Date(), ".csv"),
#           row.names = FALSE)

#' ---
#' title: UHC SCI index to estimate TB incidence
#'        Based on recommendations by the Task Force in 2024
#'        
#'        Group of countries HIC and low TB burden, previously relying on std adjustment
#'        
#' author: Mathieu Bastard
#' date: 10/06/2026
#' ---


# Load libraries
suppressMessages({
  library(data.table)
  library(imputeTS)
  library(zoo)
  library(propagate)
  library(here)
  library(readxl)
  library(lme4)
  library(brms)
  library(ggplot2)
  library(dplyr)
  library(ggrepel)
  library(tibble)
  library(tidyr)
  library(stringr)
})


# Prepare UHC SCI dataset
source(here('inc_mort/R code/03a-UHCSCI_calculation.R'))


# Load pre-calculated estimates and UHC SCI data
load(here("inc_mort/analysis/uhcsci_data.rda"))


# WHO region categories
wr <- c('AMR', 'AFR', 'EMR', 'EUR', 'SEA', 'WPR')


# Import UHC SCI to est
est <- merge(est,uhcsci_data,by=c("iso3","year"), all.x=T)


# --- TB incidence estimation ---
#
#  Incidence = Notification / UHC SCI
#

est[iso3 %in% uhcsci.direct.lst, `:=`(
  inc.direct.uhcsci = imp.newinc / (uhcsci_mod / 100)
  )]  


# Count total missing estimates for these countries
total_missing <- est[iso3 %in% uhcsci.direct.lst, sum(is.na(inc.direct.uhcsci))]
cat("Total missing values for inc.direct.uhcsci:", total_missing, "\n")

missing_summary <- est[iso3 %in% uhcsci.direct.lst, .(
  missing_years_count = sum(is.na(inc.direct.uhcsci))
), by = .(iso3, country)][missing_years_count > 0]

print(missing_summary[order(-missing_years_count)])


# For these countries, list UHC SCI data
missing_iso3s <- est[iso3 %in% uhcsci.direct.lst & (is.na(inc.direct.uhcsci)), unique(iso3)]
flagged_country_data <- est[iso3 %in% missing_iso3s, .(iso3, country, year, imp.newinc, uhcsci_mod, inc.direct.uhcsci)]
print(flagged_country_data[order(iso3, year)])


# All these countries have no estimate of UHC SCI
# For them, use a regional average of the UHC SCI, 
# and derive incidence from their notifications and  imputed UHC SCI


# 1. Calculate the yearly regional average using ALL countries in the dataset
est[, reg_mean_yearly := mean(uhcsci_mod, na.rm = TRUE), by = .(g.whoregion, year)]

# 2. Calculate an overall multi-year regional average as a safety fallback
est[, reg_mean_overall := mean(uhcsci_mod, na.rm = TRUE), by = .(g.whoregion)]

# 3. Apply the imputation ONLY to countries in the UHCSCI direct list that have missing values
est[iso3 %in% uhcsci.direct.lst & (is.na(uhcsci_mod)), 
    uhcsci_mod := fifelse(!is.na(reg_mean_yearly), reg_mean_yearly, reg_mean_overall)]

est[, c("reg_mean_yearly", "reg_mean_overall") := NULL]


# Check imputation
print(flagged_country_data[order(iso3, year)])

# Estimate TB incidence for these countries in missing_iso3s list
est[iso3 %in% missing_iso3s, `:=`(
  inc.direct.uhcsci = imp.newinc / (uhcsci_mod / 100)
)]

# Recheck missing estimates for these countries
total_missing <- est[iso3 %in% uhcsci.direct.lst, sum(is.na(inc.direct.uhcsci))]
cat("Total missing values for inc.direct.uhcsci:", total_missing, "\n")





# Sanity check on estimates for these countries

# Calculate YoY change and internal standard deviations per country
est[iso3 %in% uhcsci.direct.lst, `:=`(
  # Calculate the percent change from the previous year
  yoy_pct_change = (inc.direct.uhcsci - shift(inc.direct.uhcsci, type = "lag")) / shift(inc.direct.uhcsci, type = "lag"),
  
  # Calculate how many standard deviations a year is from the country's own mean
  country_z_score = (inc.direct.uhcsci - mean(inc.direct.uhcsci, na.rm = TRUE)) / sd(inc.direct.uhcsci, na.rm = TRUE)
), by = iso3]

# 2. Flag extreme anomalies (e.g., YoY change > 50% or Z-score > 2.5)
anomalies <- est[iso3 %in% uhcsci.direct.lst & (abs(yoy_pct_change) > 0.5 | abs(country_z_score) > 2.5), 
                 .(iso3, country, year, inc.direct.uhcsci, yoy_pct_change, country_z_score)]

print(anomalies[order(-abs(yoy_pct_change))])


# Check for COVID abnormalities - U shape notification in 2019-2023

# 1. Cast wide ONLY for target countries and target years
u_shape_check <- dcast(
  est[iso3 %in% uhcsci.direct.lst & year %in% c(2019, 2020, 2021, 2022, 2023)], 
  iso3 + country ~ year, 
  value.var = "newinc"
)

# Rename year columns cleanly for logical evaluation
setnames(u_shape_check, c("2019", "2020", "2021", "2022", "2023"), c("y19", "y20", "y21", "y22", "y23"))

# 2. Identify the U-Shape pattern
# Condition: Dropped by >20% in 2020/2021 vs 2019, and bounced back by >15% in 2023 vs 2021/2022
target_disrupted_countries <- u_shape_check[
  (y20 < 0.85 * y19 | y21 < 0.85 * y19) & 
    (y23 > 1.1 * y21 | y23 > 1.1 * y22 | y22 > 1.1 * y21 | y21 > 1.1 * y20 )
]

# Print out the precise table of flagged countries from your list
print(target_disrupted_countries)


#Sub plot

# Extract vector of flagged ISO codes from your list
flagged_list_iso3s <- target_disrupted_countries$iso3

# Plot full time-series trends (2010 to 2025) for visual validation
p_uhcscidirect_covid <- ggplot(est[iso3 %in% flagged_list_iso3s & year %in% 2010:yr], aes(x = year, y = c_newinc)) +
  geom_line(color = "#E53935", linewidth = 1.2) +
  geom_point(color = "#B71C1C", size = 2) +
  
  # Shaded rectangle overlaying the primary disruption window
  annotate("rect", xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "black") +
  
  facet_wrap(~ country, scales = "free_y") +
  theme_minimal(base_size = 12) +
  labs(
    title = "COVID-19 Disruptions within UHCSCI Direct Country Roster",
    subtitle = "Shaded area indicates the 2020–2022 pandemic window",
    x = "Year",
    y = "Case Notifications (c_notified)"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggsave(
  filename = here("inc_mort/analysis/graphs/uhcsci/COVID_disruptions_UHCSCI_direct.pdf"),
  plot = p_uhcscidirect_covid,
  device = "pdf",
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300
)



### For these countries identified, interpolate incidence in a given period
# Add AZE, KGZ
to_add <- c("AZE", "KGZ")
flagged_list_iso3s <- union(flagged_list_iso3s,to_add)

est[iso3 %in% flagged_list_iso3s & year %in% 2020:2022, inc.direct.uhcsci := NA]




# Apply a natural cubic spline interpolation by country

est[iso3 %in% flagged_list_iso3s, 
    inc.direct.uhcsci := na.spline(inc.direct.uhcsci, x = year, na.rm = FALSE), 
    by = iso3]


# Perou interpolation to accout for ACF

est[iso3=="PER" & year %in% 2020:2024, inc.direct.uhcsci := NA]
est[iso3=="PER", 
    inc.direct.uhcsci := na_interpolation(inc.direct.uhcsci, na.rm = FALSE)]


### Plot for these countries imputed for COVID years

plot_df <- est[iso3 %in% flagged_list_iso3s & year %in% 2010:yr]

melted_df <- melt(
  plot_df,
  id.vars = c("iso3", "country", "year"),
  measure.vars = c("inc.direct.uhcsci", "newinc"),
  variable.name = "Metric",
  value.name = "Rate_Per_100k"
)

melted_df[Metric == "inc.direct.uhcsci", Metric_Label := "Estimated Incidence Rate"]
melted_df[Metric == "newinc", Metric_Label := "Observed Notification Rate"]


p_check_imp_COVID <- ggplot(melted_df, aes(x = year, y = Rate_Per_100k, color = Metric_Label, group = Metric_Label)) +
  
  annotate("rect", xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf, 
           alpha = 0.08, fill = "grey10") +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  
  facet_wrap(~ country, scales = "free_y") +
  
  
  scale_color_manual(values = c("Estimated Incidence Rate" = "#00897B", 
                                "Observed Notification Rate" = "#E53935")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Epidemiological Rate Comparison (Shared Axis)",
    subtitle = "Comparing Estimated Burden vs. Clinical Notifications in identical units (per 100k)",
    x = "Year",
    y = "Rate per 100,000 Population",
    color = "Data Stream"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", color = "#2c3e50")
  )


ggsave(
  filename = here("inc_mort/analysis/graphs/uhcsci/COVID_disruptions_check_Imp_Inc.pdf"),
  plot = p_check_imp_COVID,
  device = "pdf",
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300
)




### Estimate of SD

# 2025 implementation:
# SD is 10% of the incidence estimate, consistent with previous method
est[iso3 %in% uhcsci.direct.lst, inc.direct.uhcsci.sd:=inc.direct.uhcsci*0.1]


# Check if any remaining missing values on inc and SD for these countries
missing_audit <- est[iso3 %in% uhcsci.direct.lst, .(
  missing_estimate = sum(is.na(inc.direct.uhcsci) | inc.direct.uhcsci == ""),
  missing_std_error = sum(is.na(inc.direct.uhcsci.sd) | inc.direct.uhcsci.sd == ""),
  missing_either    = sum(is.na(inc.direct.uhcsci) | inc.direct.uhcsci == "" | 
                            is.na(inc.direct.uhcsci.sd) | inc.direct.uhcsci.sd == "")
)]

print(missing_audit)






# UI bounds for these countries

sel <- est[, iso3 %in% uhcsci.direct.lst & year %in% 2000:yr & inc.direct.uhcsci != 0]
out <- t(vlohi(est$inc.direct.uhcsci[sel] / m, est$inc.direct.uhcsci.sd[sel] / m))

est[sel, `:=`(
  inc.direct.uhcsci.lo = out[, 1] * m,
  inc.direct.uhcsci.hi = out[, 2] * m
)]



#If incidence=0, then UIs are (0-0)
sel <- est[, iso3 %in% uhcsci.direct.lst & year %in% 2000:yr & inc.direct.uhcsci == 0]

est[sel, `:=`(
  inc.direct.uhcsci.lo = 0,
  inc.direct.uhcsci.hi = 0
)]



# Check if any missing values on inc and bounds
missing_audit <- est[iso3 %in% uhcsci.direct.lst, .(
  missing_estimate = sum(is.na(inc.direct.uhcsci) | inc.direct.uhcsci == ""),
  missing_lo = sum(is.na(inc.direct.uhcsci.lo) | inc.direct.uhcsci.lo == ""),
  missing_lo = sum(is.na(inc.direct.uhcsci.hi) | inc.direct.uhcsci.hi == "")
)]

print(missing_audit)





# Graphs 
# Plot incidence for countries with standard adjustment

plot_df <- est[iso3 %in% uhcsci.direct.lst & year %in% 2010:yr]

p_inc_UHCSCI_direct <- ggplot(plot_df, aes(x = year)) +
  
  geom_ribbon(aes(ymin = inc.direct.uhcsci.lo, ymax = inc.direct.uhcsci.hi, fill = "Incidence 95% UI"), 
              alpha = 0.2, color = NA) +
  
  geom_line(aes(y = inc.direct.uhcsci, color = "Estimated Incidence"), linewidth = 1) +

  geom_line(aes(y = newinc, color = "Observed Notification Rate"), linewidth = 1) +

  # D. Split into an optimized panel grid per country
  facet_wrap(~ country, scales = "free_y") +
  
  # Colors and Styling
  scale_color_manual(values = c("Estimated Incidence" = "#00897B", 
                                "Observed Notification Rate" = "#E53935")) +
  scale_fill_manual(values = c("Incidence 95% UI" = "#00897B")) +
  
  theme_minimal(base_size = 12) +
  labs(
    title = "TB Incidence Estimates vs. Clinical Notifications",
    x = "Year",
    y = "Rate per 100,000 Population",
    color = "Data Streams",
    fill = "Uncertainty"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", color = "#2c3e50"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



ggsave(
  filename = here("inc_mort/analysis/graphs/uhcsci/Incidence_UHSCI_direct.pdf"),
  plot = p_inc_UHCSCI_direct,
  device = "pdf",
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300
)




# Plot vs last year

old_subset <- old25[, .(iso3, year, inc_old = inc)]

plot_df <- merge(est, old_subset, by = c("iso3", "year"), all.x = TRUE)

plot_df <- plot_df[iso3 %in% uhcsci.direct.lst & year %in% 2010:yr]

p_compare_inc_lastyear <- ggplot(plot_df, aes(x = year)) +
  
  geom_line(aes(y = inc.direct.uhcsci, color = "New Estimate (inc.direct.uhcsci)"), linewidth = 1) +

  geom_line(aes(y = inc_old, color = "Old Estimate (inc - old25)"), linewidth = 1 ) +

  # Facet by country with free Y-axes to accommodate different epidemiological burdens
  facet_wrap(~ country, scales = "free_y") +
  
  # Customizing visual aesthetics
  scale_color_manual(values = c(
    "New Estimate (inc.direct.uhcsci)" = "#00897B", 
    "Old Estimate (inc - old25)" = "#D81B60"
  )) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Comparison of TB Incidence Estimates",
    x = "Year",
    y = "Incidence Rate (per 100,000 population)",
    color = "Model Version"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", color = "#2c3e50"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggsave(
  filename = here("inc_mort/analysis/graphs/uhcsci/Incidence_UHSCI_Compared_LastYear.pdf"),
  plot = p_compare_inc_lastyear,
  device = "pdf",
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300
)

#' ---
#' title: UHC SCI index calculation from DDI data
#' author: Mathieu Bastard
#' date: 27/05/2026
#' ---



# Documentation about the code


# Load libraries
suppressMessages({
  library(data.table)
  library(imputeTS)
  library(zoo)
  library(propagate)
  library(here)
  library(readxl)
#  library(ggpubr)
  library(lme4)
  library(brms)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(forecast)
})


# Load UHC SCI individual indicator 
# Data received from WHO data division on 22nd May 2026

#value
uhcsci_value <-
  as.data.table(read_excel(
    here("data/uhcsci/SDG-3.8.1--Weighted-SCI--FINAL.xlsx"),
    col_names = TRUE,
    sheet = 1
  ))


#weight
uhcsci_weight <-
  as.data.table(read_excel(
    here("data/uhcsci/SDG-3.8.1--Weighted-SCI--FINAL.xlsx"),
    col_names = TRUE,
    sheet = 2
  ))




# Merge values and weights, and clean dataset

uhcsci_value <- uhcsci_value %>% 
  dplyr::select(iso3, year, starts_with("value_"))


uhcsci_weight <- uhcsci_weight %>% 
  rename(iso3 = location_name) %>%
  dplyr::select(iso3, year, ends_with("_population_value"))

uhcsci <- merge(uhcsci_value, uhcsci_weight, by = c("iso3", "year"))



### Weighted geometric mean function

weighted_geo_mean <- function(x, w, na.rm = FALSE) {
  # Safety check: x and w must be the same length
  if (length(x) != length(w)) {
    stop("Vectors 'x' and 'w' must be of the same length.")
  }
  
  # Safety check: Geometric mean is only defined for positive numbers
  if (any(x < 0, na.rm = TRUE)) {
    stop("Geometric mean cannot be calculated for negative values.")
  }
  
  # Handle missing values if requested
  if (na.rm) {
    missing <- is.na(x) | is.na(w)
    x <- x[!missing]
    w <- w[!missing]
  }
  
  # Calculate the weighted mean of the natural logs, then exponentiate
  return(exp(sum(w * log(x)) / sum(w)))
}


# UHCSCI sub index on rmnch
uhcsci[, weighted_geo_mean_rmnch := weighted_geo_mean(
  x = c(value_fp, value_anc4,value_dtp3,value_pneumo), 
  w = c(fp_population_value, anc4_population_value,dtp3_population_value,pneumo_population_value),
  na.rm = TRUE
), 
by = .(iso3, year)]

# UHCSCI sub index on ID
uhcsci[!is.na(value_itn), weighted_geo_mean_id := weighted_geo_mean(
  x = c(value_tb_trans, value_art_trans,value_itn,value_sanit), 
  w = c(tb_trans_population_value, art_trans_population_value,itn_population_value,sanit_population_value),
  na.rm = TRUE
), 
by = .(iso3, year)]

uhcsci[is.na(value_itn), weighted_geo_mean_id := weighted_geo_mean(
  x = c(value_tb_trans, value_art_trans,value_sanit), 
  w = c(tb_trans_population_value, art_trans_population_value,sanit_population_value),
  na.rm = TRUE
), 
by = .(iso3, year)]

# UHCSCI sub index on NCD
uhcsci[, weighted_geo_mean_ncd := weighted_geo_mean(
  x = c(value_hptr, value_diab_trans,value_toba_trans), 
  w = c(hptr_population_value, diab_trans_population_value,toba_trans_population_value),
  na.rm = TRUE
), 
by = .(iso3, year)]

# UHCSCI sub index on NCD
uhcsci[, weighted_geo_mean_capacity := weighted_geo_mean(
  x = c(value_beds_trans, value_hwf_index,value_ihr), 
  w = c(beds_trans_population_value, hwf_index_population_value,ihr_population_value),
  na.rm = TRUE
), 
by = .(iso3, year)]



# Checks new vs DDI calculated indicator

p1 <- ggplot(uhcsci, aes(x = value_rmnch, y = weighted_geo_mean_rmnch)) +
  geom_point(alpha = 0.5, color = "midnightblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "RMNCH",
    x = "Original Value (value_rmnch)",
    y = "Calculated Value (weighted_geo_mean_rmnch)"
  ) +
  theme_minimal() +
  coord_equal()

p2 <- ggplot(uhcsci, aes(x = value_id, y = weighted_geo_mean_id)) +
  geom_point(alpha = 0.5, color = "midnightblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "ID",
    x = "Original Value (value_id)",
    y = "Calculated Value (weighted_geo_mean_id)"
  ) +
  theme_minimal() +
  coord_equal()

p3 <- ggplot(uhcsci, aes(x = value_ncd, y = weighted_geo_mean_ncd)) +
  geom_point(alpha = 0.5, color = "midnightblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "NCD",
    x = "Original Value (value_ncd)",
    y = "Calculated Value (weighted_geo_mean_ncd)"
  ) +
  theme_minimal() +
  coord_equal()

p4 <- ggplot(uhcsci, aes(x = value_capacity, y = weighted_geo_mean_capacity)) +
  geom_point(alpha = 0.5, color = "midnightblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Capacity",
    x = "Original Value (value_capacity)",
    y = "Calculated Value (weighted_geo_mean_capacity)"
  ) +
  theme_minimal() +
  coord_equal()



(p1 + p2) / (p3 + p4)


# Calculate the modified sub index for ID, taking out TB

# UHCSCI sub index on ID
uhcsci[!is.na(value_itn), weighted_geo_mean_id_mod := weighted_geo_mean(
  x = c(value_art_trans,value_itn,value_sanit), 
  w = c(art_trans_population_value,itn_population_value,sanit_population_value),
  na.rm = TRUE
), 
by = .(iso3, year)]

uhcsci[is.na(value_itn), weighted_geo_mean_id_mod := weighted_geo_mean(
  x = c(value_art_trans,value_sanit), 
  w = c(art_trans_population_value,sanit_population_value),
  na.rm = TRUE
), 
by = .(iso3, year)]


#Check

# View(uhcsci[,.(iso3,year,value_id, value_tb_trans, value_art_trans,value_itn,value_sanit,
#           tb_trans_population_value, art_trans_population_value,sanit_population_value)])

p_comp_ID <- ggplot(uhcsci, aes(x = year)) +
  
  # 1. Smooth line for the original value_id column
  geom_smooth(aes(y = value_id, color = "Original Value", fill = "Original Value"), 
              method = "gam", formula = y ~ s(x, bs = "cs"), size = 1.2) +
  
  # 2. Smooth line for the modified geometric mean column
  geom_smooth(aes(y = weighted_geo_mean_id_mod, color = "Weighted Geo Mean Mod", fill = "Weighted Geo Mean Mod"), 
              method = "gam", formula = y ~ s(x, bs = "cs"), size = 1.2) +
  
  # Manual scale customization to build a proper legend
  scale_color_manual(values = c("Original Value" = "midnightblue", "Weighted Geo Mean Mod" = "firebrick")) +
  scale_fill_manual(values = c("Original Value" = "midnightblue", "Weighted Geo Mean Mod" = "firebrick")) +
  
  labs(
    title = "Smoothed Trend Over Time",
    x = "Year",
    y = "Value",
    color = "Metrics",
    fill = "Metrics"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



### UHCSCI calculation (as a simple geometric mean of the 4 subindex)

geo_mean <- function(..., na.rm = TRUE) {
  data_matrix <- cbind(...)
  row_prod <- apply(data_matrix, 1, prod, na.rm = na.rm)
  row_count <- apply(data_matrix, 1, function(x) sum(!is.na(x)))
  row_count[row_count == 0] <- NA
  return(row_prod ^ (1 / row_count))
}

uhcsci[, value_sci_recal := geo_mean(weighted_geo_mean_rmnch,
                                             weighted_geo_mean_id,
                                             weighted_geo_mean_ncd,
                                             weighted_geo_mean_capacity
                                             )]

p5 <- ggplot(uhcsci, aes(x = value_sci, y = value_sci_recal)) +
  geom_point(alpha = 0.5, color = "midnightblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "RMNCH",
    x = "Original Value (value_sci)",
    y = "Calculated Value (value_sci_recal)"
  ) +
  theme_minimal() +
  coord_equal()




uhcsci[,.(iso3,year,value_sci,value_sci_recal)]


# calculate the modified UHC SCI (taking out TB)

uhcsci[, uhcsci_mod := geo_mean(weighted_geo_mean_rmnch,
                                     weighted_geo_mean_id_mod,
                                     weighted_geo_mean_ncd,
                                     weighted_geo_mean_capacity
)]




# p_comp_uhcsci <- ggplot(uhcsci, aes(x = year)) +
#   
#   # 1. Smooth line for the original value_id column
#   geom_smooth(aes(y = value_sci, color = "Original Value", fill = "Original Value"), 
#               method = "gam", formula = y ~ s(x, bs = "cs"), size = 1.2) +
#   
#   # 2. Smooth line for the modified geometric mean column
#   geom_smooth(aes(y =uhcsci_mod, color = "Weighted Geo Mean Mod", fill = "Weighted Geo Mean Mod"), 
#               method = "gam", formula = y ~ s(x, bs = "cs"), size = 1.2) +
#   
#   # Manual scale customization to build a proper legend
#   scale_color_manual(values = c("Original Value" = "midnightblue", "Weighted Geo Mean Mod" = "firebrick")) +
#   scale_fill_manual(values = c("Original Value" = "midnightblue", "Weighted Geo Mean Mod" = "firebrick")) +
#   
#   labs(
#     title = "Smoothed Trend Over Time",
#     x = "Year",
#     y = "Value",
#     color = "Metrics",
#     fill = "Metrics"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# 
# ggsave(
#   filename = here("inc_mort/analysis/graphs/uhcsci/comparing_SCI_modified.pdf"),
#   plot = p_comp_uhcsci,
#   device = "pdf", 
#   width = 11, 
#   height = 8.5, 
#   units = "in", 
#   dpi = 300
# )


# For China, one data point is non consistent. replace with official WHO value
uhcsci[abs(value_sci-value_sci_recal)>1,.(iso3,year,value_sci,value_sci_recal,uhcsci_mod)]
uhcsci[iso3=="CHN" & year == 2001,uhcsci_mod:= value_sci]



### Imputing 2024 and 2025
# New in 2026: impute using ARIMA (before LOCF)

# Expand the dataset to include 2024 and 2025
countries <- unique(uhcsci$iso3)
new_rows <- CJ(iso3 = countries, year = c(2024, 2025))

uhcsci <- rbind(uhcsci, new_rows, fill = TRUE)
setkey(uhcsci, iso3, year)


# Use ARIMA time-series forecasting
# Define UHC SCI Imputation Function
predict_uhc_arima <- function(value) {
  # 1. Isolate the historical non-NA data points
  clean_val <- na.omit(value)
  
  # Require at least 5 years of historical data to fit a trend
  if (length(clean_val) < 5) return(value)
  
  # 2. Map 0-100 index  onto a 0-1 scale,
  scaled_val <- pmax(pmin(clean_val / 100, 0.999), 0.001)
  
  # Transform to an unbounded scale (-infinity to +infinity)
  logit_val <- log(scaled_val / (1 - scaled_val))
  
  # 3. Fit ARIMA model to avoid overfitting
  # We enforce first-order differencing (d=1) to preserve long-term trend lines,
  # restrict the auto-regressive (p) and moving-average (q) steps to a maximum of 1,
  # and explicitly enable 'allowdrift' to capture ongoing structural trajectory velocity.
  
  fit <- auto.arima(logit_val, 
                    max.p = 1, 
                    max.q = 1, 
                    d = 1, 
                    allowdrift = TRUE, 
                    stepwise = FALSE) 
  
  # 4. Forecast years 2024 and 2025
  fore <- forecast(fit, h = 2)
  forecasted_logit <- fore$mean
  
  # 5. Inverse-Logit Transformation
  forecasted_clean <- 100 * (exp(forecasted_logit) / (1 + exp(forecasted_logit)))
  
  # Combine the historical records with the clean 2024 and 2025 projections
  return(c(clean_val, forecasted_clean))
}


# Apply the time-series forecasting model per country group
uhcsci[, uhcsci_mod := predict_uhc_arima(uhcsci_mod), by = iso3]



# Graph one example of imputation 

set.seed(241) 
sample_countries <- sample(unique(uhcsci$iso3), 20)
dt_sample <- uhcsci[iso3 %in% sample_countries]

# 
# p_check_imp <- ggplot(dt_sample, aes(x = year, y = uhcsci_mod, group = iso3)) +
#   
#   geom_line(data = subset(dt_sample, year <= 2023), 
#             aes(color = "Historical"), linewidth = 1) +
#   
#   geom_line(data = subset(dt_sample, year >= 2023), 
#             aes(color = "Imputed"), linewidth = 1) +
#   
#   geom_point(data = subset(dt_sample, year >= 2024), 
#              aes(color = "Imputed"), size = 2) +
#   
#   facet_wrap(~iso3, scales = "free_y", ncol = 3) + 
#   
#   expand_limits(y = 0) +
#   
#   scale_color_manual(
#     values = c("Historical" = "midnightblue", "Imputed" = "firebrick"),
#     name = "Data Status"
#   ) +
#   
#   labs(
#     title = "Time-Series Imputation Diagnostic Check",
#     x = "Year",
#     y = "UHC SCI"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     strip.background = element_rect(fill = "gray95", color = NA), 
#     strip.text = element_text(face = "bold")
#   )
# 
# 
# ggsave(
#   filename = here("inc_mort/analysis/graphs/uhcsci/imputation_plot.pdf"),
#   plot = p_check_imp,
#   device = "pdf", 
#   width = 11, 
#   height = 8.5, 
#   units = "in", 
#   dpi = 300
# )
# 
# 
# 



# Select relevant columns and years
uhcsci_data_full <- uhcsci
uhcsci_data<- uhcsci[,.(iso3,year,value_sci,uhcsci_mod)]



attr(uhcsci_data_full, "timestamp") <- Sys.Date()
save(uhcsci_data_full, file = here('inc_mort/analysis/uhcsci_data_full.rda'))

attr(uhcsci_data, "timestamp") <- Sys.Date()
save(uhcsci_data, file = here('inc_mort/analysis/uhcsci_data.rda'))


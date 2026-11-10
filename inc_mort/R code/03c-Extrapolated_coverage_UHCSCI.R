#' ---
#' title: UHC SCI index to estimate TB incidence
#'        Based on recommendations by the Task Force in 2024
#'        
#'        Group of countries previously relying on expert opinion
#'        
#' author: Mathieu Bastard
#' date: 20/06/2026
#' ---


est.train <- est


# Include a new line for 2025


# Prepare data for modeling, create variables
est.train[, time:=year-2000]
est.train[, time2:=time^2]
est.train[, time3:=time^3]
est.train[, coverage := imp.newinc / inc * 100]



# Model coverage ~ UHC SCI + f(time) + region + interaction terms

model.uhcsci <- glm(
  coverage ~ uhcsci_mod*I(time) + uhcsci_mod*I(time^2) +  
    as.factor(g.whoregion)*I(time) + as.factor(g.whoregion)*I(time^2),
  data = est.train[iso3 %in% tbps.lst & coverage<=100,]
)


summary(model.uhcsci)

# Check model fit
datafit <- est.train[iso3 %in% tbps.lst, ]
datafit[, pred:= predict(model.uhcsci, newdata = datafit, re.form = NA)]

# Plot model fit
p_uhcsci_fit <- ggplot(datafit, aes(x = year, y = coverage)) +
  geom_smooth(aes(color = "Treatment Coverage"), se = FALSE, fill = "blue") +
  geom_smooth(aes(year, pred, color = "Prediction from the model"), fill = "chartreuse3", se = FALSE) +
  geom_smooth(aes(year, uhcsci_mod, color = "Modified UHC SCI"), fill = "red", se = FALSE) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  facet_wrap(~ g.whoregion, scales = 'free_y') +
  scale_color_manual(name = "Legend", values = c("blue", "chartreuse3", "red")) +
  ylab("") +
  theme(text = element_text(size = 16))+
  theme_bw()

ggsave(
  filename = here("inc_mort/analysis/graphs/uhcsci_model/ModelFit_Cov_UHSCI.pdf"),
  plot = p_uhcsci_fit,
  device = "pdf",
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300
)



# Model prediction on countries with expert opinion
# Countries in EUR and AMR not part of this prediction as no TBPS data are available for them

preddata <- est.train[iso3 %in% uhcsci.statmd.lst & g.whoregion %ni% c("EUR", "AMR"), ]
preddata[, pred:= predict(model.uhcsci, newdata = preddata, re.form = NA)]

# Estimate incidence as notif/predicted coverage
# Impute missing values, mainly because of not notifications reported

preddata[iso3 %in% uhcsci.statmd.lst, inc.uhcsci.statmd:=imp.newinc/(pred/100)]

# any missing estimate
missing_count <- preddata[iso3 %in% uhcsci.statmd.lst, 
                          sum(is.na(inc.uhcsci.statmd) | inc.uhcsci.statmd == "")]
cat("Number of missing values for inc.uhcsci.statmd:", missing_count, "\n")


# Fix negative inc
preddata[iso3 %in% uhcsci.statmd.lst & inc.uhcsci.statmd<0,.(iso3,year,c_newinc,inc.uhcsci.statmd)]


wr.expert <- c('AFR', 'EMR', 'SEA', 'WPR')
wr.expert.nops <- c('AMR', 'EUR')


###
### Estimation of uncertainty for these countries


d=est.train[,.(iso3,year,coverage,uhcsci_mod,time,time2,g.whoregion)]
d <- subset(d, iso3 %in% tbps.lst)
d <- subset(d, !is.na(coverage) & coverage<100)

glm_family <- gaussian() 

all_regions <- unique(d$g.whoregion)
d$g.whoregion <- factor(d$g.whoregion, levels = all_regions)

# Get unique iso3 codes
unique_iso3_codes <- unique(d$iso3)

# Initialize a vector to store the LOOCV predictions
# Use NA_real_ for numeric NAs, matching the likely type of predictions
d$loocv_predictions <- NA_real_

# --- Loop through each iso3 code ---
print(paste("Starting Leave-One-Group-Out CV for", length(unique_iso3_codes), "countries"))

for (iso_to_leave_out in unique_iso3_codes) {
  
  print(paste("Processing: Leaving out country =", iso_to_leave_out))
  
  # 1. Split data: training set (all except current iso3), test set (only current iso3)
  train_data <- d[d$iso3 != iso_to_leave_out, ]
  test_data <- d[d$iso3 == iso_to_leave_out, ]
  
  # Check if sets are valid
  if (nrow(train_data) == 0) {
    warning(paste("Skipping country", iso_to_leave_out, "- No training data remaining."))
    next # Skip to the next iso3 code
  }
  if (nrow(test_data) == 0) {
    # This shouldn't happen if iso_to_leave_out comes from unique(d$iso3)
    warning(paste("Skipping country", iso_to_leave_out, "- No test data found (unexpected)."))
    next # Skip to the next iso3 code
  }
  
  # 2. Fit the GLM model on the training data
  # Use tryCatch to handle potential errors during model fitting (e.g., convergence)
  model_loocv <- tryCatch({
    glm(
      coverage ~ uhcsci_mod  * I(time) + uhcsci_mod   * I(time^2) +
        g.whoregion * I(time) + g.whoregion * I(time^2), # Use the pre-factored variable
      data = train_data,
      family = glm_family # Use the specified family
      # control = glm.control(maxit = 100)
    )
  }, error = function(e) {
    warning(paste("Model fitting failed for fold leaving out country", iso_to_leave_out, ":", e$message))
    return(NULL) # Return NULL if fitting fails
  })
  
  # 3. Predict on the test data (the left-out iso3 group)
  if (!is.null(model_loocv)) {
    # Use tryCatch for prediction errors (e.g., factor levels mismatch if not handled above)
    predictions <- tryCatch({
      predict(model_loocv, newdata = test_data, type = "response") # type="response" gives predictions on the scale of the outcome
    }, error = function(e) {
      warning(paste("Prediction failed for fold leaving out country", iso_to_leave_out, ":", e$message))
      # Return NAs of the correct length if prediction fails
      return(rep(NA_real_, nrow(test_data)))
    })
    
    # 4. Store the predictions back into the original dataframe 'd'
    # Find the row indices in 'd' that correspond to the current test_data
    original_indices <- which(d$iso3 == iso_to_leave_out)
    if(length(original_indices) == length(predictions)) {
      d$loocv_predictions[original_indices] <- predictions
    } else {
      warning(paste("Length mismatch between predictions and original indices for country", iso_to_leave_out))
      # Attempt assignment anyway if possible, but flag it
      d$loocv_predictions[original_indices] <- predictions[1:length(original_indices)]
    }
    
  } else {
    # Model fitting failed, store NAs for this group
    original_indices <- which(d$iso3 == iso_to_leave_out)
    d$loocv_predictions[original_indices] <- NA_real_
    warning(paste("Predictions for country", iso_to_leave_out, "are NA due to model fitting failure."))
  }
  
} # End of loop through iso3 codes

print("LOOCV process completed.")

# --- Evaluate the LOOCV predictions ---

# Remove rows where prediction failed or original coverage is NA
valid_preds <- !is.na(d$loocv_predictions) & !is.na(d$coverage)

if(sum(valid_preds) > 0) {
  # Calculate overall performance metrics, e.g., Root Mean Squared Error (RMSE)
  rmse <- sqrt(mean((d$coverage[valid_preds] - d$loocv_predictions[valid_preds])^2))
  mae <- mean(abs(d$coverage[valid_preds] - d$loocv_predictions[valid_preds]))
  
  print(paste("Overall LOOCV RMSE:", round(rmse, 4)))
  print(paste("Overall LOOCV MAE:", round(mae, 4)))
  
  # R-squared approximation:
  rsq <- 1 - sum((d$coverage[valid_preds] - d$loocv_predictions[valid_preds])^2) / sum((d$coverage[valid_preds] - mean(d$coverage[valid_preds]))^2)
  print(paste("Overall LOOCV Pseudo R-squared:", round(rsq, 4)))
  
  # Plot d$coverage vs d$loocv_predictions
  plot(d$coverage, d$loocv_predictions, main="LOOCV Predictions vs Actual", xlab="Actual Coverage", ylab="Predicted Coverage")
  abline(0, 1, col = "red") 
  
} else {
  print("Could not calculate performance metrics - no valid predictions found.")
}



print("--- Assessing Prediction Difference per Country ---")

# Calculate the difference for each observation where prediction is available
d <- d %>%
  mutate(prediction_diff = loocv_predictions - coverage)

# Summarize these differences grouped by country (iso3)
# Handle cases where differences might be all NA for a country
country_prediction_summary <- d %>%
  group_by(iso3) %>%
  summarise(
    n_obs = n(), # Total observations for this country over time
    n_valid_predictions = sum(!is.na(loocv_predictions)), # Number of successful LOOCV predictions
    n_valid_diff = sum(!is.na(prediction_diff)), # Number of differences calculated
    # Calculate summary stats only if there are valid differences
    mean_diff = if (n_valid_diff > 0) mean(prediction_diff, na.rm = TRUE) else NA_real_,
    median_diff = if (n_valid_diff > 0) median(prediction_diff, na.rm = TRUE) else NA_real_,
    sd_diff = if (n_valid_diff > 1) sd(prediction_diff, na.rm = TRUE) else NA_real_, # SD needs at least 2 points
    min_diff = if (n_valid_diff > 0) min(prediction_diff, na.rm = TRUE) else NA_real_,
    max_diff = if (n_valid_diff > 0) max(prediction_diff, na.rm = TRUE) else NA_real_,
    mae_diff = if (n_valid_diff > 0) mean(abs(prediction_diff), na.rm = TRUE) else NA_real_ # Mean Absolute Error
  ) %>%
  ungroup()

# Print the summary table
print("Summary of Prediction Differences (Predicted - Observed) by Country (iso3):")
print(as.data.frame(country_prediction_summary))

# Example: Show countries with the largest average positive bias (over-prediction)
print("Countries with largest mean over-prediction:")
print(head(country_prediction_summary %>% arrange(desc(mean_diff)) %>% filter(n_valid_diff > 0)))

# Example: Show countries with the largest average negative bias (under-prediction)
print("Countries with largest mean under-prediction:")
print(head(country_prediction_summary %>% arrange(mean_diff) %>% filter(n_valid_diff > 0)))

# Example: Show countries where prediction error was most variable
print("Countries with largest variability (SD) in prediction difference:")
print(head(country_prediction_summary %>% arrange(desc(sd_diff)) %>% filter(n_valid_diff > 1)))


# --- Task 2: Calculate Pooled Standard Deviation of Observed Coverage per Year ---

print("--- Calculating Pooled Standard Deviation of Observed Coverage per Year ---")

# Calculate the standard deviation of the *observed* coverage across all countries for each year
# This uses the original 'coverage' column, not the predictions.

yearly_coverage_sd <- d %>%
  group_by(year) %>%
  summarise(
    n_obs = n(), # Total observations for this country over time
    n_valid_predictions = sum(!is.na(loocv_predictions)), # Number of successful LOOCV predictions
    n_valid_diff = sum(!is.na(prediction_diff)), # Number of differences calculated
    # Calculate summary stats only if there are valid differences
    mean_diff = if (n_valid_diff > 0) mean(prediction_diff, na.rm = TRUE) else NA_real_,
    median_diff = if (n_valid_diff > 0) median(prediction_diff, na.rm = TRUE) else NA_real_,
    sd_diff = if (n_valid_diff > 1) sd(prediction_diff, na.rm = TRUE) else NA_real_, # SD needs at least 2 points
    min_diff = if (n_valid_diff > 0) min(prediction_diff, na.rm = TRUE) else NA_real_,
    max_diff = if (n_valid_diff > 0) max(prediction_diff, na.rm = TRUE) else NA_real_,
    mae_diff = if (n_valid_diff > 0) mean(abs(prediction_diff), na.rm = TRUE) else NA_real_ # Mean Absolute Error
  ) %>%
  ungroup()


# Print the yearly SD table
print("Pooled Standard Deviation of Observed Coverage by Year (time):")
print(as.data.frame(yearly_coverage_sd))



### UIs for incidence
yearly_coverage_sd=as.data.frame(yearly_coverage_sd)
preddata=merge(preddata,subset(yearly_coverage_sd, select=c("year","sd_diff")),all.x=T,by="year")

# LOCF sd_diff for current year of estimate
setorder(preddata, iso3, year)
preddata[, sd_diff := na_locf(sd_diff), by = iso3]
preddata[, coverage := na_locf(coverage), by = iso3]

preddata[,.(iso3,year,coverage,imp.newinc,inc.uhcsci.statmd,sd_diff)]

#UI bounds
out <- vlohi(preddata$coverage/1e5, preddata$sd_diff/1e5)

preddata[iso3 %in% uhcsci.statmd.lst, inc.uhcsci.statmd.lo:=imp.newinc/(out[2,]*1e5/100)]
preddata[iso3 %in% uhcsci.statmd.lst, inc.uhcsci.statmd.hi:=imp.newinc/(out[1,]*1e5/100)]


p.compare=ggplot(preddata[year>=2010], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  geom_line(aes(x = year, y = inc.uhcsci.statmd), color = "red", linetype = 2) +
  geom_ribbon(aes(ymin = inc.uhcsci.statmd.lo, ymax = inc.uhcsci.statmd.hi), fill = "red", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")

ggsave(
  filename = here("inc_mort/analysis/graphs/uhcsci_model/Inc_Compare_UHCSCIStatMod.pdf"),
  plot = p.compare,
  device = "pdf",
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300
)


# --- Countries in regions with no TBPS ---

other.uhcsci.statmd.lst <- unique(est.train$iso3[est.train$iso3 %in% uhcsci.statmd.lst & est.train$g.whoregion %in% wr.expert.nops])

# Plot current estimates
ggplot(est.train[iso3 %in% other.uhcsci.statmd.lst & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc - 1.96 * inc.sd, ymax = inc + 1.96 * inc.sd), fill = "blue", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2023)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")

# Use UHC SCI directly for these countries
preddata2 <- est.train[iso3 %in% other.uhcsci.statmd.lst, ]

preddata2[iso3 %in% other.uhcsci.statmd.lst, inc.uhcsci.statmd:=imp.newinc/(uhcsci_mod/100)]

# Fix negative inc
preddata2[inc.uhcsci.statmd<0,.(iso3,year,c_newinc,inc.uhcsci.statmd)]

# SD is 10% of the incidence estimate, consistent with previous method
preddata2[, inc.uhcsci.statmd.sd:=inc.uhcsci.statmd*0.1]

out <- vlohi(preddata2$inc.uhcsci.statmd/m, preddata2$inc.uhcsci.statmd.sd/m)
preddata2$inc.uhcsci.statmd.lo <- out[1,]*m
preddata2$inc.uhcsci.statmd.hi <- out[2,]*m


# Graph to check
ggplot(preddata2[ year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc - 1.96 * inc.sd, ymax = inc + 1.96 * inc.sd), fill = "blue", alpha = 0.4) +
  geom_line(aes(x = year, y = inc.uhcsci.statmd), color = "red") +
  geom_ribbon(aes(ymin = inc.uhcsci.statmd.lo, ymax = inc.uhcsci.statmd.hi), fill = "red", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2023)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")



# Some these countries needs adjustment due to COVID-19 pandemic fall in notifications
# Trends to be revised in 2020-2022
# Assumption that TB system is back to normal
# Impute/interpolate 2020-2022 based on 2019 and 2023

std.needsadj.lst2=c("GEO","HTI","UZB")

sel=preddata2$iso3 %in% std.needsadj.lst2 & preddata2$year %in% 2020:2022
preddata2$inc.uhcsci.statmd[sel] <- NA
preddata2$inc.uhcsci.statmd.sd[sel] <- NA

preddata2[iso3=="GEO", .(iso3,year,inc.uhcsci.statmd,inc.uhcsci.statmd.sd)]

preddata2[iso3 %in% std.needsadj.lst2, inc.uhcsci.statmd := na_kalman(inc.uhcsci.statmd), by = 'iso3']
preddata2[sel, inc.uhcsci.statmd.sd := 0.1*inc.uhcsci.statmd]

sel= (preddata2$iso3 %in% std.needsadj.lst2) & preddata2$year %in% 2000:2022
out <- vlohi(preddata2$inc.uhcsci.statmd[sel]/m, preddata2$inc.uhcsci.statmd.sd[sel]/m)
preddata2$inc.uhcsci.statmd.lo[sel] <- out[1,]*m
preddata2$inc.uhcsci.statmd.hi[sel] <- out[2,]*m

preddata2[iso3=="GEO", .(iso3,year,inc.uhcsci.statmd,inc.uhcsci.statmd.lo,inc.uhcsci.statmd.hi)]

# Graph to check
ggplot(preddata2[year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc - 1.96 * inc.sd, ymax = inc + 1.96 * inc.sd), fill = "blue", alpha = 0.4) +
  geom_line(aes(x = year, y = inc.uhcsci.statmd), color = "red") +
  geom_ribbon(aes(ymin = inc.uhcsci.statmd.lo, ymax = inc.uhcsci.statmd.hi), fill = "red", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2023)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")



# --- Compile all estimates ---
#Compile all estimates in 1 dataset

est.uhcsci.statmod=rbind(
  preddata[,.(iso3,year,inc.uhcsci.statmd,inc.uhcsci.statmd.sd=sd_diff,inc.uhcsci.statmd.lo,inc.uhcsci.statmd.hi,newinc)],
  preddata2[,.(iso3,year,inc.uhcsci.statmd,inc.uhcsci.statmd.sd,inc.uhcsci.statmd.lo,inc.uhcsci.statmd.hi,newinc)]
)



# Check all countries are there

uhcsci.statmd.lst
temp.lst=unique(est.uhcsci.statmod$iso3)
setdiff(uhcsci.statmd.lst, temp.lst)
setdiff(temp.lst,uhcsci.statmd.lst)


### Plot estimates for this group

ggplot(est.uhcsci.statmod[ year >= 2010, ], aes(x = year)) +
  geom_line(aes(x = year, y = inc.uhcsci.statmd), color = "red") +
  geom_ribbon(aes(ymin = inc.uhcsci.statmd.lo, ymax = inc.uhcsci.statmd.hi), fill = "red", alpha = 0.4) +
  
  geom_line(data=est.train[iso3 %in% uhcsci.statmd.lst & year >=2010],aes(x = year, y = newinc), color = "black") +
  
  geom_line(data=est.train[iso3 %in% uhcsci.statmd.lst & year >=2010],aes(x = year, y = inc), color = "blue") +
  geom_ribbon(data=est.train[iso3 %in% uhcsci.statmd.lst & year >=2010],aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")




### Smooth trends to avoid the bumps

library(mgcv)

smooth_with_bounds_robust <- function(y, y_lo, y_hi, year_vec) {
  
  logit <- function(p) log(p / (1 - p))
  inv_logit <- function(l) exp(l) / (1 + exp(l))
  
  # 1. Clamp data to avoid extreme boundary effects
  y_prop  <- pmin(pmax(y / 1e5, 1e-4), 1 - 1e-4)
  lo_prop <- pmin(pmax(y_lo / 1e5, 1e-5), y_prop) # Ensure lo <= y
  hi_prop <- pmax(pmin(y_hi / 1e5, 1 - 1e-5), y_prop) # Ensure hi >= y
  
  y_logit <- logit(y_prop)
  wlo_logit <- y_logit - logit(lo_prop)
  whi_logit <- logit(hi_prop) - y_logit
  
  # 2. Use a simpler, more robust smoother for widths
  # We use a weighted mean approach or a very stiff GAM (k=2)
  # This prevents the bounds from "wiggling" wildly
  m_mean <- gam(y_logit ~ s(year_vec, k = 3), method = "REML")
  
  # Modeling the log of the width prevents bounds from ever crossing 
  # and forces the width to remain positive
  m_wlo <- gam(log(wlo_logit + 1e-6) ~ s(year_vec, k = 2), method = "REML")
  m_whi <- gam(log(whi_logit + 1e-6) ~ s(year_vec, k = 2), method = "REML")
  
  # 3. Predict and Apply Anchor
  pred_mean <- predict(m_mean, newdata = data.frame(year_vec = year_vec))
  
  # Soft landing for mean
  idx_2025 <- which(year_vec == 2025)
  if(length(idx_2025) > 0) {
    pred_mean[(idx_2025-2):idx_2025] <- seq(pred_mean[idx_2025-3], y_logit[idx_2025], length.out = 4)[2:4]
  }
  
  pred_wlo <- exp(predict(m_wlo, newdata = data.frame(year_vec = year_vec)))
  pred_whi <- exp(predict(m_whi, newdata = data.frame(year_vec = year_vec)))
  
  # 4. Final reconstruction
  final_mean <- inv_logit(pred_mean) * 1e5
  final_lo   <- inv_logit(pred_mean - pred_wlo) * 1e5
  final_hi   <- inv_logit(pred_mean + pred_whi) * 1e5
  
  return(data.frame(mean = final_mean, lo = final_lo, hi = final_hi))
}


# Now apply this function without dividing inside the lapply
# 1. Define the columns we are creating
out_cols <- c("inc.uhcsci.statmd.smooth", 
              "inc.uhcsci.statmd.lo.smooth", 
              "inc.uhcsci.statmd.hi.smooth")

# 2. Perform the assignment
# We pass the full data columns to the function and assign the result to the new columns
est.uhcsci.statmod[, (out_cols) := smooth_with_bounds_robust(
  inc.uhcsci.statmd, 
  inc.uhcsci.statmd.lo, 
  inc.uhcsci.statmd.hi, 
  year
), by = iso3]




### Plot check

plot_dt <- est.uhcsci.statmod[, 
                              .(iso3, year, 
                                Raw = inc.uhcsci.statmd, 
                                Smooth = inc.uhcsci.statmd.smooth,
                                Notif = newinc)]

plot_long <- melt(plot_dt, 
                  id.vars = c("iso3", "year"), 
                  measure.vars = c("Raw", "Smooth", "Notif"), 
                  variable.name = "Type", 
                  value.name = "Incidence")

ggplot(plot_long[year>=2010], aes(x = year, y = Incidence, color = Type)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = plot_long[Type == "Raw" & year>=2010], alpha = 0.4, size = 1) + # Keep raw points visible
  geom_point(data = plot_long[Type == "Notif" & year>=2010], alpha = 0.4, size = 1) + # Keep raw points visible
  facet_wrap(~ iso3, scales = "free_y") +
  scale_color_manual(values = c("Raw" = "blue", "Smooth" = "#e41a1c", "Notif" = "black")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Incidence: Raw vs. Logit-Smoothed Trends",
    subtitle = "Comparing raw estimates against the logit-transformed loess smoothing",
    x = "Year",
    y = "Incidence Rate",
    color = "Trend Type"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )




### Plot estimates for this group

p_compare_all_UHCSCI_StatMod <- ggplot(est.uhcsci.statmod[ year >= 2010, ], aes(x = year)) +
  geom_line(aes(x = year, y = inc.uhcsci.statmd.smooth), color = "red") +
  geom_ribbon(aes(ymin = inc.uhcsci.statmd.lo.smooth, ymax = inc.uhcsci.statmd.hi.smooth), fill = "red", alpha = 0.4) +
  
  geom_line(data=est.train[iso3 %in% uhcsci.statmd.lst & year >=2010],aes(x = year, y = newinc), color = "black") +
  
  geom_line(data=est.train[iso3 %in% uhcsci.statmd.lst & year >=2010],aes(x = year, y = inc), color = "blue") +
  geom_ribbon(data=est.train[iso3 %in% uhcsci.statmd.lst & year >=2010],aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")



ggsave(
  filename = here("inc_mort/analysis/graphs/uhcsci_model/Inc_UHSCI_StatMod.pdf"),
  plot = p_compare_all_UHCSCI_StatMod,
  device = "pdf",
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300
)


### Check missing estimates for these countries 

target_cols <- c("inc.uhcsci.statmd.smooth", 
                 "inc.uhcsci.statmd.lo.smooth", 
                 "inc.uhcsci.statmd.hi.smooth",
                 "inc.uhcsci.statmd.sd")

missing_summary <- est.uhcsci.statmod[, lapply(.SD, function(x) sum(is.na(x))), 
                                      .SDcols = target_cols]

print("Total missing values per smoothed variable:")
print(missing_summary)



missing_details <- est.uhcsci.statmod[is.na(inc.uhcsci.statmd.smooth) | 
                                        is.na(inc.uhcsci.statmd.lo.smooth) | 
                                        is.na(inc.uhcsci.statmd.hi.smooth), 
                                      .(iso3, year, inc.uhcsci.statmd.smooth, 
                                        inc.uhcsci.statmd.lo.smooth, 
                                        inc.uhcsci.statmd.hi.smooth)]

cat("\nNumber of rows with at least one missing smoothed value:", nrow(missing_details), "\n")



# Check inc and bounds consistency

# 1. Define violations
# Rule A: Incidence should be >= notifications (assuming detection rate <= 100%)
# Rule B: Lower bound must be <= incidence
# Rule C: Upper bound must be >= incidence
est.uhcsci.statmod[, `:=`(
  check_vs_notif = inc.uhcsci.statmd.smooth < newinc, 
  check_lo_bound = inc.uhcsci.statmd.lo.smooth > inc.uhcsci.statmd.smooth,
  check_hi_bound = inc.uhcsci.statmd.hi.smooth < inc.uhcsci.statmd.smooth
)]

# 2. Summarize violations
violations <- est.uhcsci.statmod[check_vs_notif == TRUE | 
                                   check_lo_bound == TRUE | 
                                   check_hi_bound == TRUE]

cat("Number of records violating integrity rules:", nrow(violations), "\n")

# 3. View the specific rows that fail
if(nrow(violations) > 0) {
  print(violations[, .(iso3, year, newinc, 
                       inc.uhcsci.statmd.smooth, 
                       inc.uhcsci.statmd.lo.smooth, 
                       inc.uhcsci.statmd.hi.smooth)])
}



# Final data with only the smoothed final values

est.uhcsci.statmod <- est.uhcsci.statmod[,.(iso3,year,inc.uhcsci.statmd=inc.uhcsci.statmd.smooth,
                                            inc.uhcsci.statmd.sd=inc.uhcsci.statmd.sd,
                                            inc.uhcsci.statmd.lo=inc.uhcsci.statmd.lo.smooth,
                                            inc.uhcsci.statmd.hi=inc.uhcsci.statmd.hi.smooth)]



attr(est.uhcsci.statmod, "timestamp") <- Sys.Date() #set date
save(est.uhcsci.statmod, file = here('inc_mort/analysis/est.uhcsci.statmod.rda'))

# fwrite(est.uhcsci, file = here(paste0('inc_mort/analysis/csv/est.uhcsci_', Sys.Date(), '.csv')))


#' 
#' ###
#' ### Specific customization where UHC SCI approach does not work
#' ###
#' 
#' custom.lst=c("COD","BFA","CAF","GNQ","SLE","SSD")
#' 
#' testdata=tb[iso3 %in% custom.lst & year>1999, .(iso3,year,newinc,c.newinc)]
#' testdata[,imp.newinc := na_kalman(newinc, type = 'trend'), by = iso3]
#' 
#' 
#' # Average increase in notif rate
#' 
#' calculate_average_relative_increase <- function(data, as_percentage = TRUE) {
#'   
#'   # Filter data for the years 2010 to 2025
#'   filtered_data <- data[data$year >= 2010 & data$year <= 2025, ]
#'   
#'   # Order the data by year to ensure correct sequence for difference calculation
#'   ordered_data <- filtered_data[order(filtered_data$year), ]
#'   
#'   # Calculate the year-over-year relative increases
#'   # We need at least two data points to calculate an increase
#'   if (nrow(ordered_data) < 2) {
#'     warning("Not enough data points (less than 2) to calculate relative increases for the specified period.")
#'     return(0)
#'   }
#'   
#'   # Use `diff` for the numerator (new_value - old_value)
#'   # Use `head` to get all but the last value for the denominator (old_value)
#'   absolute_increases <- diff(ordered_data$imp.newinc)
#'   old_values <- head(ordered_data$imp.newinc, -1) # All values except the last one
#'   
#'   # Handle cases where old_value might be zero, which would lead to division by zero
#'   if (any(old_values == 0)) {
#'     stop("Cannot calculate relative increase: one or more 'c.newinc' values in the denominator are zero.")
#'   }
#'   
#'   relative_increases <- absolute_increases / old_values
#'   
#'   # Calculate the average of these relative increases
#'   if (length(relative_increases) > 0) {
#'     average_relative_increase <- mean(relative_increases)
#'   } else {
#'     average_relative_increase <- 0 # No relative increases to calculate
#'   }
#'   
#'   if (as_percentage) {
#'     return(average_relative_increase * 100)
#'   } else {
#'     return(average_relative_increase)
#'   }
#' }
#' 
#' increase_rates=list()
#' 
#' for (i in unique(testdata$iso3)) {
#'   #calculate_average_relative_increase(testdata[i])
#'   increase_rates[[i]]=calculate_average_relative_increase(testdata[i])
#' }
#' 
#' #Mean increase rate
#' (mean_rate<- mean(unlist(increase_rates)))
#' 
#' #weighted mean increase weight
#' dta=testdata[year==2024,.(iso3,c.newinc)]
#' dta$increase_rate=c(increase_rates[[1]],increase_rates[[2]],increase_rates[[3]],increase_rates[[4]],
#'                     increase_rates[[5]],increase_rates[[6]])
#' 
#' (wmean_rate=weighted.mean(dta$increase_rate,w=dta$c.newinc))
#' 
#' 
#' backcalculate_inc <- function(df,rate) {
#'   #' Backcalculates 'inc', 'inc.lo', and 'inc.hi' values in a DataFrame.
#'   #'
#'   #' From 2010 to 2024, each variable declines by 4% each year.
#'   #' From 2000 to 2010, each variable remains constant at its respective 2010 value.
#'   #'
#'   #' @param df A data frame with columns 'group', 'iso3', 'year', 'inc', 'inc.lo', and 'inc.hi'.
#'   #'           Assumes 'inc', 'inc.lo', and 'inc.hi' for 2024 are already present and correct.
#'   #'
#'   #' @return A data frame with backcalculated 'inc', 'inc.lo', and 'inc.hi' values.
#'   
#'   df_copy <- df # Work with a copy to avoid modifying the original
#'   
#'   # Ensure required columns exist; if not, you might need to handle how 2024 values are provided.
#'   required_cols <- c("inc", "inc.lo", "inc.hi")
#'   for (col in required_cols) {
#'     if (!(col %in% colnames(df_copy))) {
#'       stop(paste0("Error: Column '", col, "' missing from the input DataFrame. Ensure 2024 values are present."))
#'     }
#'   }
#'   
#'   # Group by 'group' and 'iso3' and apply the backcalculation
#'   df_result <- df_copy %>%
#'     group_by(iso3) %>%
#'     mutate(
#'       # Get the 2024 values for inc, inc.lo, and inc.hi for the current group/iso3
#'       inc_2024 = inc[year == 2024],
#'       inc_lo_2024 = inc.lo[year == 2024],
#'       inc_hi_2024 = inc.hi[year == 2024],
#'       
#'       # Initialize temporary columns for calculations
#'       inc_temp = NA_real_,
#'       inc_lo_temp = NA_real_,
#'       inc_hi_temp = NA_real_
#'     ) %>%
#'     rowwise() %>% # Operate row by row for the backcalculation logic
#'     mutate(
#'       # Calculation for 'inc'
#'       inc_temp = {
#'         if (year == 2024) {
#'           inc_2024
#'         } else if (year >= 2000 && year < 2024) {
#'           inc_2024 / (1 - rate/100)^(2024 - year)
#'         } else if (year >= 1999 && year < 2000) {
#'           inc_2024 / (1 - rate/100)^(2024 - 2010) # Value constant at 2010 level
#'         } else {
#'           NA_real_
#'         }
#'       },
#'       # Calculation for 'inc.lo' (same logic)
#'       inc_lo_temp = {
#'         if (year == 2024) {
#'           inc_lo_2024
#'         } else if (year >= 2000 && year < 2024) {
#'           inc_lo_2024 / (1 - rate/100)^(2024 - year)
#'         } else if (year >= 1999 && year < 2000) {
#'           inc_lo_2024 / (1 - rate/100)^(2024 - 2010)
#'         } else {
#'           NA_real_
#'         }
#'       },
#'       # Calculation for 'inc.hi' (same logic)
#'       inc_hi_temp = {
#'         if (year == 2024) {
#'           inc_hi_2024
#'         } else if (year >= 2000 && year < 2024) {
#'           inc_hi_2024 / (1 - rate/100)^(2024 - year)
#'         } else if (year >= 1999 && year < 2000) {
#'           inc_hi_2024 / (1 - rate/100)^(2024 - 2010)
#'         } else {
#'           NA_real_
#'         }
#'       }
#'     ) %>%
#'     ungroup() %>%
#'     # Remove temporary helper columns for 2024 values
#'     dplyr:: select(-inc_2024, -inc_lo_2024, -inc_hi_2024) %>%
#'     # Overwrite the original columns with the backcalculated temporary values
#'     mutate(
#'       inc = inc_temp,
#'       inc.lo = inc_lo_temp,
#'       inc.hi = inc_hi_temp
#'     ) %>%
#'     # Remove the temporary calculation columns
#'     dplyr:: select(-inc_temp, -inc_lo_temp, -inc_hi_temp)
#'   
#'   return(df_result)
#' }
#' 
#' 
#' dta2=est.uhcsci[iso3 %in% custom.lst,.(iso3,year,inc=inc.uhcsci,inc.lo=inc.uhcsci.lo,inc.hi=inc.uhcsci.hi)]
#' names(dta2)[names(dta2) == "inc.uhcsci"] <- "inc"
#' names(dta2)[names(dta2) == "inc.uhcsci.lo"] <- "inc.lo"
#' names(dta2)[names(dta2) == "inc.uhcsci.hi"] <- "inc.hi"
#' 
#' # 2. Call the function
#' df_with_inc <- as.data.frame(backcalculate_inc(dta2,wmean_rate/2))
#' 
#' names(df_with_inc)[names(df_with_inc) == "inc"] <- "inc.uhcsci.cust"
#' names(df_with_inc)[names(df_with_inc) == "inc.lo"] <- "inc.uhcsci.cust.lo"
#' names(df_with_inc)[names(df_with_inc) == "inc.hi"] <- "inc.uhcsci.cust.hi"
#' 
#' est.uhcsci=merge(est.uhcsci,df_with_inc,by=c("iso3","year"), all.x=T)
#' 
#' 
#' ### Replace new incidence for these countries
#' 
#' est.uhcsci[ iso3 %in% custom.lst, inc.uhcsci := inc.uhcsci.cust]
#' est.uhcsci[ iso3 %in% custom.lst, inc.uhcsci.lo := inc.uhcsci.cust.lo]
#' est.uhcsci[ iso3 %in% custom.lst, inc.uhcsci.hi := inc.uhcsci.cust.hi]
#' est.uhcsci[ iso3 %in% custom.lst, inc.uhcsci.sd := (inc.uhcsci.cust.hi-inc.uhcsci.cust.lo)/3.92]
#' 
#' 
#' ggplot(est.uhcsci[year >= 2010 & iso3 %in% custom.lst , ], aes(x = year, y = inc.uhcsci)) +
#'   geom_line(color = "blue") +
#'   geom_ribbon(aes(ymin = inc.uhcsci.lo, ymax = inc.uhcsci.hi), fill = "blue", alpha = 0.4) +
#'   scale_x_continuous(breaks = c(2010, 2015, 2020, yr)) +
#'   geom_line(aes(year, newinc)) +
#'   facet_wrap(~ iso3, scales = 'free_y') +
#'   xlab("") +
#'   ylab("Incidence rate per 100k/yr")
#' 
#' ### Save files
#' 
#' attr(est.uhcsci, "timestamp") <- Sys.Date() #set date
#' save(est.uhcsci, file = here('inc_mort/analysis/est.uhcsci.rda'))
#' fwrite(est.uhcsci, file = here(paste0('inc_mort/analysis/csv/est.uhcsci_', Sys.Date(), '.csv')))
#' 
#' 
#' 

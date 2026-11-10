#' ---
#' title: Force of infection and IRR for HIV
#'        
#'        
#' author: Mathieu Bastard
#' date: 16/06/2026
#' ---



# --- Assumed Functions ---
# 
# divXY 
# na_interpolation 

#  1. Calculate Force of Infection (FI) and Incidence Rate Ratio (IRR) ---
#  Calculate FI for HIV+ and HIV- populations and then the IRR.

est[, c("fi.h", "fi.h.sd") := divXY(inc.h / m, hiv, inc.h.sd / m, hiv.sd)]
est[, c("fi.nh", "fi.nh.sd") := divXY(inc.nh / m, (1 - hiv), (inc.nh.sd / m), hiv.sd)]
est[, c("irr", "irr.sd") := divXY(fi.h, fi.nh, fi.h.sd, fi.nh.sd)]


# 2. Clean IRR Data and Interpolate ---
# Nullify biologically implausible IRR values and those with zero standard deviation.
est[irr > 1e3, `:=`(irr = NA, irr.sd = NA)]

# # Identify countries with new NAs and interpolate to fill the gaps.
# countries_to_interpolate <- est[irr.sd == 0 & hiv > 0, `:=`(irr = NA, irr.sd = NA)][, unique(iso3)]
# if (length(countries_to_interpolate) > 0) {
#   est[iso3 %in% countries_to_interpolate, `:=`(
#     irr    = na_interpolation(irr),
#     irr.sd = na_interpolation(irr.sd)
#   ), by = iso3]
# }
# 

# 3. Prepare for Imputation ---
# Create an imputation map based on strata (year, HIV prevalence group, income status).

est[, `:=`(
  ghiv    = fifelse(is.na(hiv), FALSE, hiv > 0.1),
  hincome = fifelse(is.na(g.income), TRUE, g.income == 'HIC')
)]

imputation_map <- est[, .(e.irr = weighted.mean(irr, w = pop, na.rm = TRUE)),
                      by = .(year, ghiv, hincome)]

imputation_map[is.infinite(e.irr) | is.nan(e.irr), e.irr := NA]
imputation_map[, e.irr := na_interpolation(e.irr), by = .(ghiv, hincome)]

# Merge the estimated IRR back into the main dataset.
est <- merge(est, imputation_map, by = c("year", "ghiv", "hincome"), all.x = TRUE)


# 4. Unified Imputation of Missing IRR Values ---
# Impute missing IRR using a country-specific adjustment factor (f.irr).
# This single block replaces the three separate cases (n.irr=0, n.irr=1, n.irr>1).
est[, `:=`(
  # Calculate a country-specific adjustment factor and SD ratio.
  # This is calculated once per country and broadcast to all its rows.
  f.irr = {

    n_irr_obs <- sum(!is.na(.SD$irr) & .SD$irr > 0)
    if (n_irr_obs == 0) 1.0 else mean(.SD$irr, na.rm = TRUE) / mean(.SD$e.irr, na.rm = TRUE)
  },
  f.irr.sd_ratio = {
    n_irr_obs <- sum(!is.na(.SD$irr) & .SD$irr > 0)
    if (n_irr_obs == 0) 0.25 else mean(.SD$irr.sd, na.rm = TRUE) / mean(.SD$irr, na.rm = TRUE)
  }
), by = iso3]

# Apply the imputation only where IRR is missing.
est[is.na(irr), `:=`(
  irr    = e.irr * f.irr,
  irr.sd = (e.irr * f.irr) * f.irr.sd_ratio
)]


# 5. Finalize and Clean Up ---
# Run final data validation checks.
est[!is.na(tbhiv), test.isbinom(tbhiv)]
est[!is.na(irr), test.ispos(irr)]

# Clean up temporary columns and fix potential edge cases.
est[is.infinite(irr.sd), irr.sd := irr * 0.25]
est[is.na(hiv.sd) & !is.na(hiv), hiv.sd := hiv * 0.25]
est[, `:=`(f.irr = NULL, f.irr.sd_ratio = NULL, e.irr = NULL, ghiv = NULL, hincome = NULL)]
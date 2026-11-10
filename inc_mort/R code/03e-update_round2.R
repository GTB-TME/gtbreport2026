#' ---
#' title: Round 2 - Update in the estimate of TB incidence
#' author: Mathieu Bastard
#' date: 24/07/2026
#'
#' 
#' 
#'  Update to round 1 TB incidence estimate based on country feedback
#'  To be run just before Step 2 of the 03-incidence.R script and the work on disaggregation by HIV
#' 
#'  Updated estimates from the model are fixed separately by importing the latest model outputs directly 
#' 


### UHC SCI and statistical model
### Smooth showing an odd trend

### Niger

ggplot(est[iso3=="NER" & year >= 2000, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  xlab("") +
  ylab("Incidence rate per 100k/yr") +
  facet_wrap(~ iso3, scales = 'free_y')


# Interpolate 2010-2025
est[iso3 == "NER" & year >= 2011 & year <= 2024, c("inc", "inc.lo", "inc.hi") := .(NA, NA, NA)]

est[iso3 == "NER", `:=`(
  inc    = na.approx(inc, x = year, rule = 2, na.rm = FALSE),
  inc.lo = na.approx(inc.lo, x = year, rule = 2, na.rm = FALSE),
  inc.hi = na.approx(inc.hi, x = year, rule = 2, na.rm = FALSE)
), by = iso3]


### Same for AGO

ggplot(est[iso3=="AGO" & year >= 2000, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  xlab("") +
  ylab("Incidence rate per 100k/yr") +
  facet_wrap(~ iso3, scales = 'free_y')


# Interpolate 2010-2025
est[iso3 == "AGO" & year >= 2016 & year <= 2024, c("inc", "inc.lo", "inc.hi") := .(NA, NA, NA)]

est[iso3 == "AGO", `:=`(
  inc    = na.approx(inc, x = year, rule = 2, na.rm = FALSE),
  inc.lo = na.approx(inc.lo, x = year, rule = 2, na.rm = FALSE),
  inc.hi = na.approx(inc.hi, x = year, rule = 2, na.rm = FALSE)
), by = iso3]



### TLS

ggplot(est[iso3=="TLS" & year >= 2000, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  xlab("") +
  ylab("Incidence rate per 100k/yr") +
  facet_wrap(~ iso3, scales = 'free_y')


# Interpolate 2010-2025
est[iso3 == "TLS" & year >= 2011 & year <= 2024, c("inc", "inc.lo", "inc.hi") := .(NA, NA, NA)]

est[iso3 == "TLS", `:=`(
  inc    = na.approx(inc, x = year, rule = 2, na.rm = FALSE),
  inc.lo = na.approx(inc.lo, x = year, rule = 2, na.rm = FALSE),
  inc.hi = na.approx(inc.hi, x = year, rule = 2, na.rm = FALSE)
), by = iso3]



### MDA (inc < notif) post COVID, use 2019 coverage as correction

psg <- est$iso3 =="MDA" & est$year %in% 2023:2025

# 2019cov
cov_mda_19 <- est$newinc[est$iso3=="MDA" & est$year==2019] / est$inc[est$iso3=="MDA" & est$year==2019]
est[psg, inc:=newinc/cov_mda_19]


### MRT, LOCF 2023 in 2024 and 2025

ggplot(est[iso3=="MRT" & year >= 2000, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  xlab("") +
  ylab("Incidence rate per 100k/yr") +
  facet_wrap(~ iso3, scales = 'free_y')

est[iso3 == "MRT" & year >= 2024 & year <= 2025, c("inc", "inc.lo", "inc.hi") := .(NA, NA, NA)]

est[iso3 == "MRT", `:=`(
  inc    = na_locf(inc),
  inc.lo = na_locf(inc.lo),
  inc.hi = na_locf(inc.hi)
), by = iso3]


### EGY, last minute bilateral call, inc aas last year except 2025 LOCF
ggplot(est[iso3=="EGY" & year >= 2000, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  xlab("") +
  ylab("Incidence rate per 100k/yr") +
  facet_wrap(~ iso3, scales = 'free_y')

est[old25[iso3 == "EGY"], 
    on = .(iso3, year), 
    `:=` (
      inc = i.inc, 
      inc.sd = i.inc.sd, 
      inc.lo = i.inc.lo, 
      inc.hi = i.inc.hi
    )]


est[iso3 == "EGY" & year == 2025, `:=` (
  inc = NA,
  inc.sd = NA,
  inc.lo = NA,
  inc.hi = NA
)]
est[iso3 == "EGY", `:=` (
  inc = na_locf(inc),
  inc.sd = na_locf(inc.sd),
  inc.lo = na_locf(inc.lo),
  inc.hi = na_locf(inc.hi)
)]





### Yemen, LOCF inc in 2025 since most probably TB cases largley under reported
ggplot(est[iso3=="YEM" & year >= 2000, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  xlab("") +
  ylab("Incidence rate per 100k/yr") +
  facet_wrap(~ iso3, scales = 'free_y')

est[iso3 == "YEM" & year == 2025, `:=` (
  inc = NA,
  inc.sd = NA,
  inc.lo = NA,
  inc.hi = NA
)]
est[iso3 == "YEM", `:=` (
  inc = na_locf(inc),
  inc.sd = na_locf(inc.sd),
  inc.lo = na_locf(inc.lo),
  inc.hi = na_locf(inc.hi)
)]










### End of round 2 updates


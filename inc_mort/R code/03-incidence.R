
#' ---
#' title: Estimation of TB incidence
#' author: Mathieu Bastard
#'  date: 10/06/2026
#'  update: 27/07/2026
#'
#' 
#' 
#' Update 2026:
#' - Update estimation approach according to last year final methods used (est.method.rda) based on 
#'          country bilaterral discussion during second round
#'          
#' - New method for2 countries : Thailand and Mongolia, Markov model using VR data
#' 

#'  
#' Estimate TB incidence (inc), disaggregate it by HIV (inc.h and inc.nh for HIV-pos and HIV-neg,
#' respectively).
#'
#' The incidence of TB can be estimated using a variety of methods, including:
#' - Surveillance data: Health authorities can track the number of new TB cases 
#'   that are reported through surveillance systems. This can help to estimate 
#'   the overall incidence of TB in a population.
#' - Mathematical modeling: Researchers can use mathematical models to estimate 
#'   the incidence of TB based on data on factors such as population size, 
#'   TB prevalence, and the effectiveness of TB prevention and treatment programs.
#' - Epidemiological (cohort) studies: Researchers can also conduct epidemiological 
#'   studies to estimate the incidence of TB in a specific population. These studies 
#'   can involve collecting data on TB cases and using statistical methods to estimate 
#'   the overall incidence of TB. No national representative cohort study of TB incidence
#'   as ever been conducted, all past studies were either geographically limited (e.g. selected 
#'   districts in India) or population-limited (e.g. civil servants in Rep of Korea).
#'   
#' This script generates TB and TB/HIV incidence estimates based on the above 2 approaches, 
#' specifically using:
#'
#' - the pre-covid 2000-2019 estimates taken from previous report, stored in 
#'    ~/data/gtb/old.rda
#' - UNAIDS HIV estimates, stored in ~/data/gtb/unaids
#' - notifications, stored in ~/data/gtb/tb 
#' - HIV prevalence in TB, stored in ~/inc_mort/analysis/est (created by 02-tbhiv_prevalence.R)
#' - Mathematical model outputs for 2020 onwards, stored in ~/dynamic/output
#'    
#'  
#' Key ancillary variables used to generate inc, inc.h, inc.nh:
#'
#' - *.hat variables: projections of 2017-2019 trends into 2020 onwards using logistic regression
#' - imp.newinc: imputed notifications when reported numbers are missing
#' - inc.md.*: incidence from country dynamic model
#' - inc.rmd.*: incidence from regional model
#'
#'
#'
#' # Deps:
#' - output from all previous scripts in the sequence
#' - helper functions in fun.R
#' - libraries data.table", imputeTS (imputation of time series data), propagate (propagation of 
#'     errors through calculus using the delta-method or based on a second order expansion. 
#'     The propagate library can be a little difficult to install on MacOS due to dependences 
#'     on specific FORTRAN libraries (an install using brew may help). 
#'     Installation of propagate on windows OS is straightforward. 
#'
#'
#' Input: 
#'  - output from the previous scripts in the sequence
#'  - model.rda and extra.rda (Dynamic models output)
#'  - ~/inc_mort/input_inc/rules_inc.csv set of low/high uncertainty bounds (with specified time period and
#'      briefly documented source -- see comment above)
#'  
#'
#' # Output:
#' - est:  dated csv file (est_04inc_TIMESTAMP.csv) and rda file
#'
#'
#'


rm(list=ls())


# Load and suppress messages for all core packages
pacman::p_load(data.table, 
               imputeTS, 
               zoo, 
               propagate, 
               here, 
               readxl
               )

#Run Functions script
source(here('inc_mort/R code/fun.R'))

#Function to import db from GTB databases
source(here("import/load_gtb.R"))


# Load data 
data_names <- c("tb", "cty", "pop", "grpmbr")
lapply(data_names, function(x) assign(x, load_gtb(x, convert_dashes = TRUE), envir =.GlobalEnv))

#Estimation method

load(here("inc_mort/estimates2025/est.method.rda"))




#Nim results: Update to the most recent one

model=fread("dynamic/output/260706_Estimates.csv",
            col.names=c("iso3","hiv","measure","year","best","lo","hi"))
setkey(model, iso3)



# load previous years estimates (2024 and 2025 report)
load(here('inc_mort/estimates2025/est.rda'))
old25=copy(est)
rm(est)
old25[iso3=="IDN", g.whoregion:="WPR"]

load(here('inc_mort/estimates2024/est.rda'))
old24=copy(est)
rm(est)
old24[iso3=="IDN", g.whoregion:="WPR"]


#load UNDAIS data
load(here('inc_mort/analysis/unaids.rda'))


#' Load the estimate skeleton dataset prepared in codes 01c and 02
#' 
#' This data set contains:
#'  - All 215 countries and territories for years 2000 to 2025
#'  - Total reported notifications in c_newinc and newinc
#'  - Imputed notifications in imp_c_newinc and imp.newinc
#'  - HIV prevalence in TB in variable tbhiv and imp.tbhiv
#'  
#'  


load(here('inc_mort/analysis/est.rda'))



# Vectorize function lohi for 95% UI bounds
vlohi <- Vectorize(lohi, c('ev', 'sd'))

#Year of estimate
yr <- 2025

#Unit of rate (/100000pop)
m <- 1e5




# Import method to estimate data set

est <- merge(est,est.method,by="iso3",all.x=T)

# Filter for 2025 where either method variable is missing
missing_2025 <- est[year == 2025 & (
  is.na(method.inc) | method.inc == "" | 
    is.na(method.mort) | method.mort == ""
), .(iso3, country, g.whoregion, method.inc, method.mort)]

print(missing_2025)



# Import previous incidence estimates variables (inc, inc.sd, inc.lo, inc.hi)

est <- merge(est,old25[,.(iso3,
                          year,
                          inc, inc.lo, inc.hi,
                          inc.sd)],
             by = c("iso3","year"),
             all.x = T)



# check missing values, TRUE: only year==yr inc values are missing
missing_inc_2025 <- est[year < yr & (is.na(inc)), .(iso3, country, g.whoregion)]
print(missing_inc_2025)




#' # add country groupings: HIC, High TB burden hbc
#' 
#' 
income <- grpmbr[group.type == 'g_income']
setnames(income, 'group.name', 'g.income')
est <-  merge(est, income[, .(iso3, g.income)], by = 'iso3', all.x = TRUE)
est$g.income <- as.character(est$g.income)

gbd <- grpmbr[group.type == 'g_gbd']
setnames(gbd, 'group.name', 'g.gbd')
est <- merge(est, gbd[, .(iso3, g.gbd)], by = 'iso3', all.x = TRUE)

hbc <- as.character(grpmbr[group.type == 'g_hb_tb']$iso3)
est$g.hbc <- est$iso3 %in% hbc
hbmdr <- as.character(grpmbr[group.type == 'g_hb_mdr']$iso3)
est$g.hbmdr <- est$iso3 %in% hbmdr
hbtbhiv <- as.character(grpmbr[group.type == 'g_hb_tbhiv']$iso3)
est$g.hbtbhiv <- est$iso3 %in% hbtbhiv


### Create list of countries according to method for estimating TB incidence
# based on last year est.method dataset


# New IS communicated by ECDC in Croatia, Denmark, Finland, Portugal, The Netherlands and Slovenia
# New IS communicated by country : Poland

is.new.lst <- c("HRV","DNK","FIN","PRT","NLD","SVN","POL")

# UHC SCI direct adjustment (mostly HIC)
# method.inc = 1, 2, 3 : remove Djibouti as agreed to be adhoc early 2026
# Remove also th EURO countries that have a new IS
uhcsci.direct.lst <- unique(est.method[method.inc %in% c(1, 2, 3), iso3])
uhcsci.direct.lst <- setdiff(uhcsci.direct.lst, "DJI")
uhcsci.direct.lst <- setdiff(uhcsci.direct.lst, is.new.lst)


# UHC SCI stat model adjustment (previous expert opinion)
# method.inc = 4, 5, 6
uhcsci.statmd.lst <- unique(est.method[method.inc %in% c(4, 5, 6), iso3])

# Prevalence Survey
# method.inc = 7, 8, 9
tbps.lst <- unique(est.method[method.inc %in% c(7, 8, 9), iso3])

# Inventory Study
# method.inc = 10, 11, 12
is.lst <- unique(est.method[method.inc %in% c(10, 11, 12), iso3])
is.lst <- union(is.lst,is.new.lst)

# AdHoc method (following bilateral discussion, an ad-hoc method was used for these countries)
# method.inc = 99 + Djibouti
adhoc.lst <- unique(est.method[method.inc %in% c(99), iso3])
adhoc.lst <- union(adhoc.lst, "DJI")

# Length of each list
sapply(list(uhcsci.direct.lst, uhcsci.statmd.lst, tbps.lst, is.lst, adhoc.lst), length)
total_length <- sum(sapply(list(uhcsci.direct.lst, uhcsci.statmd.lst, tbps.lst, is.lst, adhoc.lst), length))
print(total_length)

### Model countries for 2020 onwards

# Countries using country model estimates for 2020 onwards
countrymod.lst <- unique(est.method[method.inc %in% c(2, 5, 8, 11), iso3])
countrymod.lst <- union(countrymod.lst,"BRA")

# Countries using regional model estimates for 2020 onwards
regmod.lst <- unique(est.method[method.inc %in% c(3, 6, 9, 12), iso3])


# Model exempted from the model where recent notifications seem to suggest that 
# COVID disruptions weren't as great as initially modeled 
# (countries where recent, reported notifications are systematically lower than those being modeled)

exemptmod.lst <- c(
  "AFG", "AGO", "ALB", "AZE", "BGD", 
  "BLZ", "KGZ", "LSO", "MNG", "PER", 
  "SWZ", "TJK", "VNM", "ZWE",
  "CRI", "MNE", "NAM", "VEN"
)

countrymod.lst <- setdiff(countrymod.lst, exemptmod.lst)
cat("Number of countries remaining in countrymod.lst:", length(countrymod.lst), "\n")

regmod.lst <- setdiff(regmod.lst, exemptmod.lst)
cat("Number of countries remaining in regmod.lst:", length(regmod.lst), "\n")



# Length of each list
sapply(list(countrymod.lst, regmod.lst), length)



bckup.est1= copy(est)


###
### Step 1 - Estimation of TB incidence
###

###
### 1. HIC countries with low TB burden using UHC SCI as a proxy of treatment coverage
###

# Run code to estimate TB incidence in countries in uhcsci.direct.lst
#
# This creates the new incidence estimates variables in the est dataset:
# inc.direct.uhcsci, inc.direct.uhcsci.lo, inc.direct.uhcsci.hi, inc.direct.uhcsci.sd

source(here('inc_mort/R code/03b-HIC_UHCSCI.R'))


# update trends in incidence + bounds

psg <- est$iso3 %in% uhcsci.direct.lst
est[psg, c("inc", "inc.sd", "inc.lo", "inc.hi") := 
      .(inc.direct.uhcsci, inc.direct.uhcsci.sd, inc.direct.uhcsci.lo, inc.direct.uhcsci.hi)]



bckup.est2= copy(est)



###
### 2. Countries previously relying on notifications and expert opinion ---
###    Use the data from countries with TBPS to train  the statistical model coverage~UHC SCI
###    Predict treatment coverage from the model in countries without TBPS and previously relying on expert opinion
###    Estimate uncertainty using LOO CV 
###    Creates est.uhcsci.statmod dataset for countries in uhcsci.statmd.lst

source(here('inc_mort/R code/03c-Extrapolated_coverage_UHCSCI.R'))


### Merge estimates in est.uhcsci.statmod to the main dataset est

dim(est)

est=merge(est,est.uhcsci.statmod, by=c("iso3","year"), all.x = T)
dim(est)


# update trends in incidence + bounds

psg <- est$iso3 %in% uhcsci.statmd.lst


est[psg, c("inc", "inc.sd", "inc.lo", "inc.hi") := 
      .(inc.uhcsci.statmd, inc.uhcsci.statmd.sd, inc.uhcsci.statmd.lo, inc.uhcsci.statmd.hi)]


# Backup of est dataset

bckup.est3= copy(est)



###
### 3. Countries with TBPS, not modeled, continue trends until better...
###

tbps.nomod.lst <- setdiff(tbps.lst,countrymod.lst)
tbps.nomod.lst <- setdiff(tbps.nomod.lst,regmod.lst)

# Graph to check
ggplot(est[iso3 %in% tbps.nomod.lst & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo , ymax = inc.hi), fill = "blue", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, yr)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")


### Continue with current trends for those not modeled last year
temp.lst=setdiff(tbps.nomod.lst,exemptmod.lst)

# Function to project incidence based one previous years
project <- function(y, time = 2021:(yr-1), target = yr) {
  
  valid_idx <- !is.na(y)
  stopifnot(sum(valid_idx) >= 2)
  
  # Fit model
  df_fit <- data.frame(y = y[valid_idx], time = time[valid_idx])
  model <- glm(y ~ time, data = df_fit, family = quasibinomial)
  
  # Predict only for the target year
  yhat <- predict(model, newdata = data.frame(time = target), type = "response")
  
  # Safety check
  if (any(y == 0 & valid_idx)) yhat <- mean(y[valid_idx], na.rm = TRUE)
  
  return(yhat)
}

# Apply  using the target year
trends <- est[year %in% 2021:yr & iso3 %in% temp.lst, .(iso3, year, inc, newinc)]

trends[, inc.hat := {
  y_history <- inc[year %in% 2021:(yr-1)] / 1e5
  time_history <- 2021:(yr-1)
  
  project(y_history, time = time_history, target = yr) * 1e5
}, by = iso3]


# keep onl< the current yr of estimate
trends <- trends[year==yr,.(iso3,year,inc.hat)]

# merge trends with main est dataset
(dim(est))
est <- merge(est, trends, by = c('iso3', 'year'), all.x = TRUE)
(dim(est))



# SD of inc, similar proportion of SD/inc as previous year (only for year yr in inc.hat.sd)
est[iso3 %in% temp.lst, prop.sd := inc.sd/inc]
est[iso3 %in% temp.lst, prop.sd := na_locf(prop.sd)]

sel<-est$iso3 %in% temp.lst & est$year == yr
est[sel, inc.hat.sd := inc.hat * prop.sd]
est[iso3 %in% temp.lst & year==yr, .(iso3,year,inc,inc.sd,inc.hat, inc.hat.sd)]


# Bounds for these countries
sel = est$iso3 %in% temp.lst & est$year == yr

out <- vlohi(est$inc.hat[sel]/m, est$inc.hat.sd[sel]/m)
est$inc.hat.lo[sel] <- out[1,]*m
est$inc.hat.hi[sel] <- out[2,]*m

est[iso3 %in% temp.lst & year==yr, .(iso3,year,inc,inc.sd,inc.hat, inc.hat.sd, inc.hat.lo,inc.hat.hi)]



# Plot

# Graph to check
ggplot(est[iso3 %in% temp.lst & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo , ymax = inc.hi), fill = "blue", alpha = 0.4) +
  geom_point(aes(y=inc.hat),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, yr)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")



# update trends in incidence + bounds (only current year)

psg <- est$iso3 %in% temp.lst & est$year == yr
est[psg, c("inc", "inc.sd", "inc.lo", "inc.hi") := 
      .(inc.hat, inc.hat.sd, inc.hat.lo, inc.hat.hi)]




### Treat countries with TBPS that were previously modeled last year but 
#'   exempted this year for the period 2020-yr
#'   For them, re-estimate the trend in 2020-yr, as projection of the trend in 2017-2019
#'   Create a new inc.exempt.hat estimate for the period 2020-yr
#'   Also estimate SD and bounds using previous year proportion of SD/inc


exemptmod.lst


# Graph to check
ggplot(est[iso3 %in% exemptmod.lst & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")




# 1. For countries using method on UHC SCI before COVID
# incidence estimates are already created on the steps before


# 2. For countries using TBPS results
# Predict incidence in 2020-yr in inc.hat

tbps.exemptmod.lst <- intersect(tbps.lst,exemptmod.lst)



# Tweak Mongolia to keep it flat in 2020 (remove artfect decrease)
est[iso3=="MNG" & year > 2015,.(year,inc,inc.lo,inc.hi)]
inc_vars <- c("inc", "inc.lo", "inc.hi", "inc.sd")
est[iso3 == "MNG" & year == 2020, (inc_vars) := est[iso3 == "MNG" & year == 2019, .SD, .SDcols = inc_vars]]


# function to project incidence on logit model
project_multi <- function(y, time_hist = 2017:2020, target_years = 2021:yr) {
  
  valid_idx <- !is.na(y)
  stopifnot(sum(valid_idx) >= 2)
  
  # Fit model
  df_fit <- data.frame(y = y[valid_idx], time = time_hist[valid_idx])
  model <- glm(y ~ time, data = df_fit, family = quasibinomial)
  
  # Predict for all target years at once
  yhats <- predict(model, 
                   newdata = data.frame(time = target_years), 
                   type = "response")
  
  # Safety check
  if (any(y == 0 & valid_idx)) yhats[] <- mean(y[valid_idx], na.rm = TRUE)
  
  return(yhats)
}

# Apply to the dataset
target_yrs <- 2021:yr

# Prepare target table
projection_results <- est[iso3 %in% tbps.exemptmod.lst, {
  # Extract history
  y_hist <- inc[year %in% 2017:2020] / 1e5
  
  # Predict
  preds <- project_multi(y_hist, time_hist = 2017:2020, target_years = target_yrs)
  
  # Return as a table aligned with target years
  list(year = target_yrs, inc.hat = preds * 1e5)
}, by = iso3]

# Merge these back into main estimation dataset
est[projection_results, on = .(iso3, year), inc.hat := i.inc.hat]


est[iso3 %in% tbps.exemptmod.lst & year>2018, .(iso3,year,inc,inc.hat)]



# SD of inc, similar proportion of SD/inc as previous year (only for year yr in inc.hat.sd)
est[iso3 %in% tbps.exemptmod.lst & year <= 2020, prop.sd := inc.sd/inc]
est[iso3 %in% tbps.exemptmod.lst, prop.sd := na_locf(prop.sd)]

sel<-est$iso3 %in% tbps.exemptmod.lst & est$year >2020
est[sel, inc.hat.sd := inc.hat * prop.sd]
est[iso3 %in% tbps.exemptmod.lst & year >2020, .(iso3,year,inc,inc.sd,inc.hat, inc.hat.sd)]


# Bounds for these countries
sel = est$iso3 %in% tbps.exemptmod.lst & est$year >2020
out <- vlohi(est$inc.hat[sel]/m, est$inc.hat.sd[sel]/m)
est$inc.hat.lo[sel] <- out[1,]*m
est$inc.hat.hi[sel] <- out[2,]*m

est[iso3 %in% tbps.exemptmod.lst & year >=2020, .(iso3,year,inc,inc.sd,inc.hat, inc.hat.sd, inc.hat.lo,inc.hat.hi)]


# Graph to check updates
ggplot(est[iso3 %in% tbps.exemptmod.lst & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  geom_line(aes(y=inc.hat),color = "red") +
  geom_ribbon(aes(ymin = inc.hat.lo, ymax = inc.hat.hi), fill = "red", alpha = 0.4) +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")



# update trends in incidence + bounds for 2021:yr

psg <- est$iso3 %in% tbps.exemptmod.lst & est$year %in% 2021:yr
est[psg, c("inc", "inc.sd", "inc.lo", "inc.hi") := 
      .(inc.hat, inc.hat.sd, inc.hat.lo, inc.hat.hi)]


# Countries having notif/inc > 0.85 in selected TPBS countries
tbps.to.check.lst <- c("LAO","MWI","SDN","TZA","ZAF","ZMB","LSO","NAM","SWZ","ZWE","VNM")

flagged_records <- est[
  iso3 %in% tbps.to.check.lst & (newinc / inc) > 0.85 & year==yr, 
  .(iso3, year, newinc, inc, ratio = newinc / inc)
]
# Display the anomalies
print(flagged_records)

# Identify and correct 2025 incidence
est[, inc_corrected_flag := FALSE]

# Flag countries that violate the condition in 2025
countries_to_interpolate <- est[
  iso3 %in% tbps.to.check.lst & (newinc / inc) > 0.85 & year == yr, 
  unique(iso3)
]

# Apply the 0.85 floor correction to 2025 for these countries
est[iso3 %in% countries_to_interpolate & year == yr, `:=`(
  inc = newinc / 0.85,
  inc_corrected_flag = TRUE
)]

# Wipe out 2020:2024 and Interpolate
# We loop only through the countries that triggered the correction
est[iso3 %in% countries_to_interpolate, inc := {
  current_inc <- inc
  current_inc[year %in% 2020:2024] <- NA
  zoo::na.approx(current_inc, x = year)
  }, by = iso3]


# Update Bounds (vlohi) for the entire modified block (2020:2025)

out <- vlohi(
  est[iso3 %in% countries_to_interpolate & year %in% 2020:yr, inc / m], 
  est[iso3 %in% countries_to_interpolate & year %in% 2020:yr, inc.sd / m]
)

est[iso3 %in% countries_to_interpolate & year %in% 2020:yr, `:=`(
  inc.lo = out[1, ] * m,
  inc.hi = out[2, ] * m
)]


est[iso3 %in% countries_to_interpolate & year %in% 2020:yr
    , .(iso3,year,inc,newinc, newinc/inc)]


ggplot(est[iso3 %in% countries_to_interpolate & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +

  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")





# Backup of est dataset

bckup.est4= copy(est)



###
### 4. Countries Inventory Studies
###


# China, carrying on 2023 case detection rate based on preCOVID 
# To be updated if new data on CDR are available

sel<-est$iso3=="CHN"
est[sel, coverage:= newinc/inc]
est[sel, coverage:= na_locf(coverage)]
est[sel & year==yr, inc.is:= newinc/coverage]

#Fix SD at 10% of central estimate as in HIC
est[sel & year==yr, inc.is.sd:=0.1*inc.is]

out <- vlohi(est$inc.is[sel & est$year==yr]/m, est$inc.is.sd[sel & est$year==yr]/m)
est$inc.is.lo[sel & est$year==yr] <- out[1,]*m
est$inc.is.hi[sel & est$year==yr] <- out[2,]*m

# update China trends in incidence + bounds for current year yr

psg <- est$iso3 == "CHN" & est$year==yr
est[psg, c("inc", "inc.sd", "inc.lo", "inc.hi") := 
      .(inc.is, inc.is.sd, inc.is.lo, inc.is.hi)]



# For the remaining countries
temp.lst <- setdiff(is.lst,"CHN")


# CDR estimated from under reporting in inventory study (from the previous rules.csv from PG)
est[iso3=="DEU", ratio.inc.notif:= mean(c(1,1.2))]
est[iso3=="EGY", ratio.inc.notif:= mean(c(1.22,1.66))]
est[iso3=="FRA", ratio.inc.notif:= mean(c(1.17,1.25))]
est[iso3=="GBR", ratio.inc.notif:= mean(c(1.05,1.2))]
est[iso3=="IRQ", ratio.inc.notif:= mean(c(1.31,1.72))]
est[iso3=="KOR", ratio.inc.notif:= mean(c(1,1.139))]
est[iso3=="YEM", ratio.inc.notif:= mean(c(1.19,1.51))]

est[iso3=="DEU", ratio.inc.notif.sd:= (1.2-1)/3.92]
est[iso3=="EGY", ratio.inc.notif.sd:= (1.66-1.22)/3.92]
est[iso3=="FRA", ratio.inc.notif.sd:= (1.25-1.17)/3.92]
est[iso3=="GBR", ratio.inc.notif.sd:= (1.2-1.05)/3.92]
est[iso3=="IRQ", ratio.inc.notif.sd:= (1.72-1.31)/3.92]
est[iso3=="KOR", ratio.inc.notif.sd:= (1.139-1)/3.92]
est[iso3=="YEM", ratio.inc.notif.sd:= (1.51-1.19)/3.92]



# New in 2026, countries in is.new.lst

calculate_adjustment_factor <- function(x, n, conf = 0.95) {
  p <- x / n
  alpha <- x + 1
  beta_val <- (n - x) + 1
  
  # Uncertainty Interval (Quantiles from Beta distribution)
  alpha_tail <- (1 - conf) / 2
  p_lo <- qbeta(alpha_tail, alpha, beta_val)
  p_hi <- qbeta(1 - alpha_tail, alpha, beta_val)
  
  # Estimate F and its bounds
  f_est <- 1 / p
  f_lo  <- 1 / p_hi
  f_hi  <- 1 / p_lo
  
  # Calculate SD of F using the Delta Method
  # Variance of Beta distribution p: (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
  var_p <- (alpha * beta_val) / (((alpha + beta_val)^2) * (alpha + beta_val + 1))
  
  # Delta method: Var(f(p)) approx [f'(p)]^2 * Var(p)
  # f'(p) = -1 / p^2
  var_f <- (1 / p^4) * var_p
  sd_f  <- sqrt(var_f)
  
  return(list(F = f_est, F_lo = f_lo, F_hi = f_hi, F_sd = sd_f))
}


est[iso3 == "HRV", ratio.inc.notif := calculate_adjustment_factor(489, 662)$F]
est[iso3 == "DNK", ratio.inc.notif := calculate_adjustment_factor(379, 384)$F]
est[iso3 == "FIN", ratio.inc.notif := calculate_adjustment_factor(260, 311)$F]
est[iso3 == "PRT", ratio.inc.notif := calculate_adjustment_factor(1997,2328)$F]
est[iso3 == "NLD", ratio.inc.notif := calculate_adjustment_factor(814, 998)$F]
est[iso3 == "SVN", ratio.inc.notif := calculate_adjustment_factor(118, 118)$F]
est[iso3 == "POL", ratio.inc.notif := calculate_adjustment_factor(4075, 4153)$F]

est[iso3 == "HRV", ratio.inc.notif.sd := calculate_adjustment_factor(489, 662)$F_sd]
est[iso3 == "DNK", ratio.inc.notif.sd := calculate_adjustment_factor(379, 384)$F_sd]
est[iso3 == "FIN", ratio.inc.notif.sd := calculate_adjustment_factor(260, 311)$F_sd]
est[iso3 == "PRT", ratio.inc.notif.sd := calculate_adjustment_factor(1997,2328)$F_sd]
est[iso3 == "NLD", ratio.inc.notif.sd := calculate_adjustment_factor(814, 998)$F_sd]
est[iso3 == "SVN", ratio.inc.notif.sd := calculate_adjustment_factor(118, 118)$F_sd]
est[iso3 == "POL", ratio.inc.notif.sd := calculate_adjustment_factor(4075, 4153)$F_sd]



sel<-est$iso3 %in% temp.lst

est[sel, inc.is:=imp.newinc*ratio.inc.notif]
est[sel, inc.is.sd:=inc.is*ratio.inc.notif.sd]


#uncertainty at minimum 10% of inc estimate in case its less for reliability
est[sel & inc.is.sd < 0.1*inc.is, inc.is.sd:=inc.is*0.1]


est[iso3 %in% temp.lst & year>2020, .(iso3,year,imp.newinc,newinc,inc,inc.sd,inc.is,inc.is.sd)]


# Bounds for IS non modeled countries - CHN 

sel<-est$iso3 %in% temp.lst & !is.na(est$inc.is)

out <- vlohi(est$inc.is[sel]/m, est$inc.is.sd[sel]/m)
est$inc.is.lo[sel] <- out[1,]*m
est$inc.is.hi[sel] <- out[2,]*m


# Treat IDN as it is a IS country but with a model then, not captured here
# Replace by last year estimate
psg <- est$iso3=="IDN" & est$year %in% 2000:(yr-1)

est[psg, c("inc.is", "inc.is.sd", "inc.is.lo", "inc.is.hi") := 
      .(inc, inc.sd, inc.lo, inc.hi)]


# update IS trends in incidence + bounds for current year yr

psg <- est$iso3 %in% temp.lst

est[psg, c("inc", "inc.sd", "inc.lo", "inc.hi") := 
      .(inc.is, inc.is.sd, inc.is.lo, inc.is.hi)]


# Handle Egypt (IS but adhoc method after bilateral discussion in 2025)

# IS in 2007 to estimate level
# Keep coverage of last year, and interpolate 2007-2025

cov_egy <- old25[iso3 == "EGY" & year == 2024, newinc / inc]

est[iso3 == "EGY" & year %in% yr, 
    inc.is := newinc / cov_egy]

est[iso3 == "EGY" & year %in% 2008:2024, 
    inc.is := NA]

est[iso3 == "EGY" & year %in% 2007:2025, 
    inc.is := approx(year, inc.is, xout = year)$y]

est[iso3 == "EGY" & year >= 2007, .(iso3, year, inc, newinc, inc.is)]

# Bounds for Egypt
sel = est$iso3 == "EGY" & est$year %in% 2008:yr
prop_sd_egy <- est[iso3 == "EGY" & year == 2007, inc.sd / inc]
est[sel, inc.is.sd := inc.is * prop_sd_egy]

out <- vlohi(est$inc.is[sel]/m, est$inc.is.sd[sel]/m)
est$inc.is.lo[sel] <- out[1,]*m
est$inc.is.hi[sel] <- out[2,]*m



# update EGY trends in incidence + bounds for 2008:yr

psg <- est$iso3=="EGY" & est$year %in% 2008:yr

est[psg, c("inc", "inc.sd", "inc.lo", "inc.hi") := 
      .(inc.is, inc.is.sd, inc.is.lo, inc.is.hi)]



# Graph to check updates
ggplot(est[iso3 %in% c(is.lst) & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  
  geom_line(data=old25[iso3 %in% c(is.lst) & year >= 2010],aes(x=year, y=inc),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")

# Backup of est dataset

bckup.est5= copy(est)




###
### 5. Countries using Ad-Hoc methods
###

adhoc.lst

ggplot(est[iso3 %in% adhoc.lst & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")


# Use inc/notif ratio of last year
# A mix of previous method (std adjustment), for small countries (<10 cases) inc=notif etc..
# Other bilateral discussions (Russia, Moldova, Mar, Djibouti, Saoudi Arabia ...)
# Use 2023 coverage (post COVID)

est[iso3 %in% adhoc.lst, coverage:= newinc/inc]

#Correct for DJI
est[iso3=="DJI", coverage:= 0.8]

#set 2024 to NA
est[iso3 %in% adhoc.lst  & year==2024, coverage:= NA]

est[iso3 %in% adhoc.lst, coverage:= na_locf(coverage)]
est[iso3 %in% adhoc.lst , inc.adhoc:= imp.newinc/coverage]
est[iso3 %in% adhoc.lst & inc.adhoc<0, inc.adhoc:= 0]


#Fix SD at 10% of central estimate as in HIC
est[iso3 %in% adhoc.lst, inc.adhoc.sd:=0.1*inc.adhoc]


# Bounds (For non 0)
sel<-est$iso3 %in% adhoc.lst & !is.na(est$inc.adhoc) & est$inc.adhoc!=0
out <- vlohi(est$inc.adhoc[sel]/m, est$inc.adhoc.sd[sel]/m)
est$inc.adhoc.lo[sel] <- out[1,]*m
est$inc.adhoc.hi[sel] <- out[2,]*m

# Bounds (For 0)
sel<-est$iso3 %in% adhoc.lst & !is.na(est$inc.adhoc) &  est$inc.adhoc==0
est$inc.adhoc.lo[sel] <- 0
est$inc.adhoc.hi[sel] <- 0



#Plot

ggplot(est[iso3 %in% adhoc.lst & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  geom_point(aes(y=inc.adhoc),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")



# For the 6 African Countries
# Continue trend

temp.lst <- c("BFA","CAF","COD","GNQ","SLE","SSD")

# Apply  using the target year
trends <- est[year %in% 2021:yr & iso3 %in% temp.lst, .(iso3, year, inc, newinc)]

trends[, inc.hat := {
  y_history <- inc[year %in% 2021:(yr-1)] / 1e5
  time_history <- 2021:(yr-1)
  
  project(y_history, time = time_history, target = yr) * 1e5
}, by = iso3]


# keep only the current yr of estimate
trends <- trends[year==yr,.(iso3,year,inc.adhoc.bis=inc.hat)]

# merge trends with main est dataset
(dim(est))
est <- merge(est, trends, by = c('iso3', 'year'), all.x = TRUE)
(dim(est))


# SD of inc, similar proportion of SD/inc as previous year (only for year yr in inc.hat.sd)
est[iso3 %in% temp.lst , prop.sd := inc.sd/inc]
est[iso3 %in% temp.lst, prop.sd := na_locf(prop.sd)]

sel<-est$iso3 %in% temp.lst & est$year == yr
est[sel, inc.adhoc.bis.sd := inc.adhoc.bis * prop.sd]
est[sel, .(iso3,year,inc,inc.sd,inc.adhoc.bis, inc.adhoc.bis.sd)]



# Complete their adhoc trend : before this year, historical inc, in 2025 inc.adhoc.bis
sel = est$iso3 %in% temp.lst & est$year %in% 2000:(yr-1)
est[sel, c("inc.adhoc", "inc.adhoc.sd") := 
      .(inc, inc.sd)]

sel = est$iso3 %in% temp.lst & est$year %in% yr
est[sel, c("inc.adhoc", "inc.adhoc.sd") := 
      .(inc.adhoc.bis, inc.adhoc.bis.sd)]


# Bounds for these countries
sel = est$iso3 %in% temp.lst 
out <- vlohi(est$inc.adhoc[sel]/m, est$inc.adhoc.sd[sel]/m)
est$inc.adhoc.lo[sel] <- out[1,]*m
est$inc.adhoc.hi[sel] <- out[2,]*m





# GNQ is bumping during a missing notification year 2012-2013: impute
sel = est$iso3=="GNQ" & est$year %in% 2012:2013
est[sel, c("inc.adhoc", "inc.adhoc.sd", "inc.adhoc.lo", "inc.adhoc.hi") := NA_real_]

cols_to_interp <- c("inc.adhoc", "inc.adhoc.sd", "inc.adhoc.lo", "inc.adhoc.hi")
est[iso3=="GNQ", (cols_to_interp) := lapply(.SD, function(x) na.approx(x, na.rm = FALSE)), 
    by = iso3, 
    .SDcols = cols_to_interp]

est[iso3 =="GNQ",.(iso3,year,inc.adhoc, inc.adhoc.sd, inc.adhoc.lo, inc.adhoc.hi)]

## Correct Albania, previously country model in 2020-yr but adhoc before
# Keep the 2019 coverage in 2023 onwards
# interpolate 2020-2022

cov_2019 <- est[iso3 == "ALB" & year == 2019, newinc / inc]

est[iso3 == "ALB" & year %in% 2025, 
    inc.adhoc := newinc / cov_2019]

est[iso3 == "ALB" & year %in% 2020:2024, 
    inc.adhoc := NA]

est[iso3 == "ALB" & year %in% 2019:2025, 
    inc.adhoc := approx(year, inc.adhoc, xout = year)$y]

est[iso3 == "ALB" & year >= 2019, .(iso3, year, inc, newinc, inc.adhoc)]

# Bounds for these countries
sel = est$iso3 == "ALB" & est$year %in% 2020:yr
est[sel, inc.adhoc.sd := inc.adhoc * 0.1]

out <- vlohi(est$inc.adhoc[sel]/m, est$inc.adhoc.sd[sel]/m)
est$inc.adhoc.lo[sel] <- out[1,]*m
est$inc.adhoc.hi[sel] <- out[2,]*m


# AIA, complete with previous estimates

est[iso3 == "AIA" & year >= 2019, .(iso3, year, inc, newinc, inc.adhoc)]
sel = est$iso3 == "AIA" & est$year < yr
est[sel, c("inc.adhoc", "inc.adhoc.sd", "inc.adhoc.lo", "inc.adhoc.hi") := 
      .(inc, inc.sd, inc.lo, inc.hi)]


# ATG and GRD missing some year (NaN)
est[iso3 %in% adhoc.lst & is.na(inc.adhoc),.(iso3,year,newinc,inc.adhoc)]
est[iso3 %in% adhoc.lst & is.na(inc.adhoc) & newinc==0,
    c("inc.adhoc", "inc.adhoc.sd", "inc.adhoc.lo", "inc.adhoc.hi") := 
      .(0, 0, 0, 0)]









ggplot(est[iso3 %in% adhoc.lst & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  geom_point(aes(y=inc.adhoc),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")



# update AdHoc trends for all but the 6 AFR countries in incidence + bounds for current year yr

psg <- est$iso3 %in% adhoc.lst

est[psg, c("inc", "inc.sd", "inc.lo", "inc.hi") := 
      .(inc.adhoc, inc.adhoc.sd, inc.adhoc.lo, inc.adhoc.hi)]




### Plot by Methods

#UHC SCI direct
p_inc_UHCSCI_direct <- ggplot(est[iso3 %in% c(uhcsci.direct.lst) & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  
  geom_line(data=old25[iso3 %in% c(uhcsci.direct.lst) & year >= 2010],aes(x=year, y=inc),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")


#UHC SCI stat mod
p_inc_UHCSCI_statmd <- ggplot(est[iso3 %in% c(uhcsci.statmd.lst) & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  
  geom_line(data=old25[iso3 %in% c(uhcsci.statmd.lst) & year >= 2010],aes(x=year, y=inc),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")


#TBPS
p_inc_TBPS <- ggplot(est[iso3 %in% c(tbps.lst) & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  
  geom_line(data=old25[iso3 %in% c(tbps.lst) & year >= 2010],aes(x=year, y=inc),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")

#IS
p_inc_IS <- ggplot(est[iso3 %in% c(is.lst) & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  
  geom_line(data=old25[iso3 %in% c(is.lst) & year >= 2010],aes(x=year, y=inc),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")

#AdHoc
p_inc_AdHoc <- ggplot(est[iso3 %in% c(adhoc.lst) & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  
  geom_line(data=old25[iso3 %in% c(adhoc.lst) & year >= 2010],aes(x=year, y=inc),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")




### Check missing country/years

missing_inc_records <- est[is.na(inc), .(iso3, country, year)]
cat("Number of country-years with missing incidence data:", nrow(missing_inc_records), "\n")
head(missing_inc_records)

unique_iso3_missing <- unique(missing_inc_records$iso3)

ggplot(est[iso3 %in% c(unique_iso3_missing) & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  
  geom_line(data=old25[iso3 %in% c(unique_iso3_missing) & year >= 2010],aes(x=year, y=inc),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")





### For Now, LOCF for these countries
# 
# # Define the columns to apply the function LOCF
# cols_to_fill <- c("inc", "inc.sd", "inc.lo", "inc.hi")
# 
# # Apply nafill to all specified columns at once, by group
# est[iso3 %in% unique_iso3_missing, (cols_to_fill) := lapply(.SD, nafill, type = "locf"), by = iso3, .SDcols = cols_to_fill]
# 
# 
# ggplot(est[iso3 %in% c(unique_iso3_missing) & year >= 2010, ], aes(x = year, y = inc)) +
#   geom_line(color = "blue") +
#   geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
#   
#   geom_line(data=old25[iso3 %in% c(unique_iso3_missing) & year >= 2010],aes(x=year, y=inc),color = "red") +
#   
#   scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
#   geom_line(aes(year, newinc)) +
#   facet_wrap(~ iso3, scales = 'free_y') +
#   xlab("") +
#   ylab("Incidence rate per 100k/yr")
# 

bckup.est6 <- copy(est)



###
### 6. Dynamic models, for a set of countries that accounted for COVID disruptions
###                    for which it was decided to continue with the modelling
###
###       (a) Country Specific model
###       
###       
###       Estimates in these countries are for 2020-2025 and may erase previous estimates 
###       using one of the previous method if applicable
###

# Replace with model estimate on for countries retained for model

model_bis <- model[iso3 %in% countrymod.lst]
model_inc <- model_bis[measure == "inc" & hiv =="a" & year >=2020]
model_inc_iso3 <- unique(model_inc$iso3)


est[model_inc, 
    on = .(iso3, year), 
    `:=`(
      inc    = i.best,
      inc.lo = i.lo,
      inc.hi = i.hi
    )]

est[iso3 %in% model_inc_iso3 & year>=2020, `:=`(
  inc.sd  = (inc.hi-inc.lo) / 3.92
)]


bckup.est7 <- copy(est)


ggplot(est[iso3 %in% c(model_inc_iso3) & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  
  geom_line(data=old25[iso3 %in% c(unique_iso3_missing) & year >= 2010],aes(x=year, y=inc),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")




###
### 6. Countries with the new approach using Markov model, VR data based : Mongolia and Thailand
###

markov_iso3 <- c("MNG","THA")

est_thailand <- fread(here("inc_mort/markovmodel/estimates_thailand_MarkovModel_2026-07-27.csv"))

est[est_thailand[iso3 == "THA"], 
    on = .(iso3, year), 
    `:=` (
      inc    = i.inc, 
      inc.lo = i.inc.lo, 
      inc.hi = i.inc.hi
    )]

est_mongolia <- fread(here("inc_mort/markovmodel/estimates_mongolia_MarkovModel_2026-07-27.csv"))

est[est_mongolia[iso3 == "MNG"], 
    on = .(iso3, year), 
    `:=` (
      inc    = i.inc, 
      inc.lo = i.inc.lo, 
      inc.hi = i.inc.hi
    )]



est[iso3 %in% markov_iso3, `:=`(
  inc.sd  = (inc.hi-inc.lo) / 3.92
)]



ggplot(est[iso3 %in% c(markov_iso3) & year >= 2000, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  
  geom_line(data=old25[iso3 %in% markov_iso3 & year >= 2000],aes(x=year, y=inc),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")




# Update country model listto remove Thailand
countrymod.lst <- setdiff(countrymod.lst,c("THA","MNG"))

###
### 7. Incorporate estimates for India
###

india <- fread(file=here("dynamic/output/India_Burden Estimation 2026 update Sept26.csv"))

year_cols <- names(india)[3:ncol(india)]
years <- as.character(as.numeric(unlist(india[1, ..year_cols])))
setnames(india, old = year_cols, new = years)

india_clean <- india[-1]
india_long <- melt(
  india_clean,
  id.vars = c("V1", "V2"),
  measure.vars = years,
  variable.name = "year",
  value.name = "value"
)

india_long[, prefix := fifelse(V1 %like% "Incidence", "inc", "mort")]
india_long[, var_name := paste0(prefix, ".", V2)]

india_final <- dcast(india_long, year ~ var_name, value.var = "value")

india_final[, year := as.integer(as.character(year))]

save(india_final,file=here("inc_mort/analysis/india_final.rda"))

print(india_final)


#Update 2025 value

est[est[iso3 == "IND" & year == 2025, 
        `:=` (
          inc    = india_final[year == 2025, inc.mid],
          inc.lo = india_final[year == 2025, inc.lo],
          inc.hi = india_final[year == 2025, inc.hi]
        )]]

est[iso3=="IND" & year == 2025, inc.sd:= (inc.hi-inc.lo) / 3.92]

est[iso3=="IND" & year>2010,.(year,inc,inc.lo,inc.hi,inc.sd)]

ggplot(est[iso3 == "IND" & year >= 2010, ], aes(x = year, y = inc)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
  
  geom_line(data=old25[iso3 == "IND" & year >= 2010],aes(x=year, y=inc),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Incidence rate per 100k/yr")



bckup.est8 <- copy(est)

### 7. Incorporate updates from feedback after round 1
### All updates made in 03e-update_round2.R on inc and inc.sd
### This code generate new estimates for specific countries and update inc, inc.sd, inc.lo, inc.hi
### Update specific to incidence in HIV+ (if needed) should be made after the disaggregation process
### 

source(here("inc_mort/R code/03e-update_round2.R"))


bckup.est9 <- copy(est)




### Check missing country/years

missing_inc_records <- est[is.na(inc), .(iso3, country, year)]
cat("Number of country-years with missing incidence data:", nrow(missing_inc_records), "\n")
head(missing_inc_records)

unique_iso3_missing <- unique(missing_inc_records$iso3)

# ggplot(est[iso3 %in% c(unique_iso3_missing) & year >= 2010, ], aes(x = year, y = inc)) +
#   geom_line(color = "blue") +
#   geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
#   
#   geom_line(data=old25[iso3 %in% c(unique_iso3_missing) & year >= 2010],aes(x=year, y=inc),color = "red") +
#   
#   scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
#   geom_line(aes(year, newinc)) +
#   facet_wrap(~ iso3, scales = 'free_y') +
#   xlab("") +
#   ylab("Incidence rate per 100k/yr")
# 


### For Now, LOCF for these countries
# 
# # Define the columns to apply the function LOCF
# cols_to_fill <- c("inc", "inc.sd", "inc.lo", "inc.hi")
# 
# # Apply nafill to all specified columns at once, by group
# est[iso3 %in% unique_iso3_missing, (cols_to_fill) := lapply(.SD, nafill, type = "locf"), by = iso3, .SDcols = cols_to_fill]
# 
# 
# ggplot(est[iso3 %in% c(unique_iso3_missing) & year >= 2010, ], aes(x = year, y = inc)) +
#   geom_line(color = "blue") +
#   geom_ribbon(aes(ymin = inc.lo, ymax = inc.hi), fill = "blue", alpha = 0.4) +
# 
#   geom_line(data=old25[iso3 %in% c(unique_iso3_missing) & year >= 2010],aes(x=year, y=inc),color = "red") +
# 
#   scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
#   geom_line(aes(year, newinc)) +
#   facet_wrap(~ iso3, scales = 'free_y') +
#   xlab("") +
#   ylab("Incidence rate per 100k/yr")
# 




#================================================================================#

### Step 2

###
### Dis-aggregate incidence by HIV status
### Using tbhiv variable : HIV prevalence in TB, estimated in previous script
###
#================================================================================#

# Incorporate UNAIDS estimates

(dim(est))
est <-
  merge(
    est,
    unaids[, .(
      iso3,
      year,
      hiv.num,
      hiv.lo.num,
      hiv.hi.num,
      mort.hiv.num,
      mort.hiv.lo.num,
      mort.hiv.hi.num
    )],
    by = c('iso3', 'year'),
    all.x = T,
    all.y = F
  )

(dim(est))

# Define the columns to be created and the columns to be calculated
hiv_cols <- c("hiv", "hiv.lo", "hiv.hi")
mort_cols <- c("mort.hiv", "mort.hiv.lo", "mort.hiv.hi")
hiv_num_cols <- paste0(hiv_cols, ".num")
mort_num_cols <- paste0(mort_cols, ".num")

# Perform the calculations on rates
est[, (hiv_cols) := lapply(.SD, \(x) x / pop), .SDcols = hiv_num_cols]
est[, (mort_cols) := lapply(.SD, \(x) x / pop * m), .SDcols = mort_num_cols]
est[, `:=`(hiv.sd = (hiv.hi - hiv.lo) / 3.92,
           mort.hiv.sd = (mort.hiv.hi - mort.hiv.lo) / 3.92)]


### Impute UNAIDS estimate for 2025 since not yet available for 1st round, impute: LOCF
# To comment the following lines once the new dataset is available

# sum(is.na(est$hiv[est$year==yr]))
# 
# # Define the columns to apply the function LOCF
# cols_to_fill <- c("hiv", "hiv.lo", "hiv.hi", "hiv.sd", 
#                   "mort.hiv", "mort.hiv.lo", "mort.hiv.hi", "mort.hiv.sd")
# 
# # Apply nafill to all specified columns at once, by group
# est[, (cols_to_fill) := lapply(.SD, nafill, type = "locf"), by = iso3, .SDcols = cols_to_fill]




### use imputed tbhiv if tbhiv missing (mostly for current year waiting for new snapshot)
# Impute sd as 20% of estimate
est[is.na(tbhiv), tbhiv:=imp.tbhiv]
est[is.na(tbhiv.sd), tbhiv.sd:=0.2*tbhiv]

### SD of TBHIV un-plausibly low when < 5% of estimates
est[tbhiv.sd/tbhiv < 0.1,.(iso3,year,tbhiv,tbhiv.sd,tbhiv.sd/tbhiv)]

# Fix SD = 5% of central estimates if too low

est[tbhiv.sd/tbhiv < 0.1, tbhiv.sd:=0.1*tbhiv]
est[, tbhiv.sd.2:=0.2*tbhiv]


###
### Derive inc in HIV pos and inc in HIV neg
###



# inc.h: incidence in HIV positive

inch <- with(est, prodXY(inc, tbhiv, inc.sd, tbhiv.sd))
est[, inc.h := inch[[1]]]
est[, inc.h.sd := inch[[2]]]

est[inc.h<0, inc.h:=0]
est[inc.h.sd<0, inc.h.sd:=0]


#Check missing data in recent year
est[year>=2019, sum(is.na(inc.h)), by=year]
est[year>=2019, sum(is.na(inc.h.sd)), by=year]

#Fix SD=0 if inc.h=0
est[inc.h==0, inc.h.sd:=0]

est[is.na(inc.h),.(iso3,year,inc,inc.h,tbhiv)]

# Bounds for inc.h
sel = est$inc.h>0 & !is.na(est$inc.h)
out <- vlohi(est$inc.h[sel]/m, est$inc.h.sd[sel]/m)
est$inc.h.lo[sel] <- out[1,]*m
est$inc.h.hi[sel] <- out[2,]*m
est[inc.h==0, inc.h.lo:=0]
est[inc.h==0, inc.h.hi:=0]




# inc.nh: incidence in HIV negative

incnh <-with(est, prodXY(inc, (1 - tbhiv), inc.sd, tbhiv.sd))
est[, inc.nh := incnh[[1]]]
est[, inc.nh.sd := incnh[[2]]]
est[year>=2019, sum(is.na(inc.nh)), by=year]
est[year>=2019, sum(is.na(inc.nh.sd)), by=year]

# Bounds for inc.nh

sel = est$inc.nh>0 & !is.na(est$inc.nh)
out <- vlohi(est$inc.nh[sel]/m, est$inc.nh.sd[sel]/m)
est$inc.nh.lo[sel] <- out[1,]*m
est$inc.nh.hi[sel] <- out[2,]*m
est[inc.nh==0, inc.nh.lo:=0]
est[inc.nh==0, inc.nh.hi:=0]




### Check missing country/years for inc.h
# Define the columns to check for missing values
check_cols <- c("inc.h", "inc.h.sd", "inc.h.lo", "inc.h.hi")

missing_records <- est[
  (is.na(inc.h) | is.na(inc.h.sd) | is.na(inc.h.lo) | is.na(inc.h.hi)) & 
    !is.na(inc) & 
    !is.na(tbhiv), 
  .(iso3, year, inc.h, inc.h.sd, inc.h.lo, inc.h.hi)
]

# Print the list of affected country-years
print("Country-years (with valid inc & tbhiv) having missing data in incidence variables:")
print(missing_records)

### Check missing country/years for inc.nh
# Define the non-HIV incidence columns to check
check_cols_nh <- c("inc.nh", "inc.nh.sd", "inc.nh.lo", "inc.nh.hi")

missing_records_nh <- est[
  (is.na(inc.nh) | is.na(inc.nh.sd) | is.na(inc.nh.lo) | is.na(inc.nh.hi)) & 
    !is.na(inc) & 
    !is.na(tbhiv), 
  .(iso3, year, inc.nh, inc.nh.sd, inc.nh.lo, inc.nh.hi)
]

# Print the results
print("Country-years (with valid inc & tbhiv) having missing data in non-HIV variables:")
print(missing_records_nh)


### backup

bckup.est10 = copy(est)

# 
# ###
# ### Incorporate results from country model that had HIV component
# ###
# 

# Replace with model estimate on for countries retained for model

model_qua <- model[iso3 %in% countrymod.lst]
model_inc_h <- model_qua[measure == "inc" & hiv =="pos" & year >=2020]

est[model_inc_h, 
    on = .(iso3, year), 
    `:=`(
      inc.h    = i.best,
      inc.h.lo = i.lo,
      inc.h.hi = i.hi
    )]



# For these countries, re estimate inc.nh as inc-inc.h and SD from 2020 onwards

model_inc_h_iso3 <- unique(model_inc_h$iso3)

est[iso3 %in% model_inc_h_iso3 & year>=2020, `:=`(
  inc.nh    = inc - inc.h,
  inc.h.sd  = (inc.h.hi-inc.h.lo) / 3.92,
  inc.nh.sd = sqrt(inc.sd^2 + inc.h.sd^2)
)]

# re-estimate bounds

psg <- est$iso3 %in% model_inc_h_iso3 & est$year>=2020
out <- vlohi(est$inc.nh[psg]/m, est$inc.nh.sd[psg]/m)
est$inc.nh.lo[psg] <- out[1,]*m
est$inc.nh.hi[psg] <- out[2,]*m



### backup

bckup.est11 = copy(est)


###
### Estimate force of infection and IRR
###

source(here('inc_mort/R code/03d-IRR.R'))



# Check global aggregates

#
# global TBHIV
#
(est[, weighted.mean(tbhiv, w = inc * pop / 1e5, na.rm = T), by = year])

# global aggregates, quick check

(est[, .(inc.num = as.integer(sum(inc * pop / 1e5, na.rm=T))), by = year])
(est[, .(inc.h.num = as.integer(sums(inc.h * pop / 1e5, na.rm=T))), by = year])

est[, temp.incnumh:=inc.h * pop / 1e5]

countries_inc_higher_with_diff <- est %>%
  filter(year %in% c(2024, 2025)) %>%
  pivot_wider(
    id_cols = iso3,
    names_from = year,
    values_from = temp.incnumh,
    names_prefix = "temp.incnumh_"
  ) %>%
  mutate(difference = `temp.incnumh_2025` - `temp.incnumh_2024`) %>%
  dplyr::filter(difference > 0) %>% 
  dplyr::arrange(desc(difference)) %>%
  dplyr::select(iso3, `temp.incnumh_2024`, `temp.incnumh_2025`, difference) 



# Print the result
print(countries_inc_higher_with_diff)


### Countries where estimated incidence < notified TB case rate
(inc.low.lst=unique(est[year==yr & (inc+0.01)<=newinc,iso3]))
est[iso3 %in% inc.low.lst , .(iso3,year,inc,newinc,c_newinc)]




### Countries not HIC, not modeled, (and RUS) where estimated incidence < 1.10 * notified TB case rate (10% as applied for HIC)

(inc.low.lst.10=unique(est[year==yr & inc<=1.1*newinc,iso3]))
tocheck.country=data.table(est[iso3 %in% inc.low.lst.10 & year>2019, .(iso3,year,inc,newinc,c_newinc)])



# Plot for these countries

p=qplot(year,
        inc,
        data = subset(est, iso3 %in% inc.low.lst.10  & year >= 2010),
        geom = 'line',
        colour = I('blue')) +
  geom_ribbon(
    aes(
      year,
      ymin = inc.lo,
      ymax = inc.hi
    ),
    fill = I('blue'),
    alpha = I(.4)
  ) +
  geom_line(
    aes(year, inc),
    data = subset(old25, iso3 %in% inc.low.lst.10 & year >= 2010),
    colour = I('red'),
    linetype = I(2))+
  scale_x_continuous(breaks=c(2010,2015,2020,2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')

suppressWarnings(ggsave(
  here(paste('inc_mort/analysis/graphs/incidence compare/Inc_tocheck_countries.pdf', sep = '')),
  plot = p,
  width = 14,
  height = 8
))




# check that inc>0 & rate<100000 for all incidence estimates

est[, test.isbinom(inc / m)]
est[!is.na(inc.nh), test.isbinom(inc.nh / m)]
est[!is.na(tbhiv), test.isbinom(tbhiv)]
est[!is.na(inc.h), test.isbinom(inc.h / m)]
est[!is.na(inc.h), sum(abs(inc.h + inc.nh - inc) > 1) == 0]
est[!is.na(inc.h), test.ispos(inc.h)]
est[!is.na(inc.h), test.ispos(inc.h.sd)]
est[!is.na(inc.h), test.ispos(inc.nh)]
est[!is.na(inc.h), test.ispos(inc.nh.sd)]
est[!is.na(irr), test.ispos(irr)]



### Missing data on incidence data
print(paste("Missing data on incidence estimate:", sum(is.na(est$inc))))
print(paste("Missing data on incidence SE estimate:", sum(is.na(est$inc.sd))))
print(paste("Missing data on incidence HIV+ estimate:", sum(is.na(est$inc.h) & !is.na(est$tbhiv))))
print(paste("Missing data on incidence HIV+ SD estimate:", sum(is.na(est$inc.h.sd) & !is.na(est$tbhiv))))
print(paste("Missing data on incidence HIV- estimate:", sum(is.na(est$inc.nh) & !is.na(est$tbhiv))))
print(paste("Missing data on incidence HIV- SD estimate:", sum(is.na(est$inc.nh.sd) & !is.na(est$tbhiv))))





###
#### PLOTS 
###
### Comparison plots with last year's report, focus on recent trends
###


for (i in wr) {
  p <- qplot(
    year,
    inc,
    data = subset(est, g.whoregion == i & year >= 2010),
    geom = 'line',
    colour = I('blue')
  ) +
    geom_ribbon(
      aes(
        year,
        ymin = inc.lo,
        ymax = inc.hi
      ),
      fill = I('blue'),
      alpha = I(.4)
    ) +
    geom_line(
      aes(year, inc),
      data = subset(old25, g.whoregion == i & year >= 2010),
      colour = I('red'),
      linetype = I(2)
    ) +
    scale_x_continuous(breaks=c(2010,2015,2020,2025)) +
    geom_line(aes(year, newinc)) +
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')
  
  suppressWarnings(ggsave(
    here(paste(
      'inc_mort/analysis/graphs/incidence compare/Incidence_', i, '_compare_last_year.pdf', sep = ''
    )),
    plot = p,
    width = 14,
    height = 8
  ))
}


# Incidence of HBC
hbc.lst <- est[g.hbc == T, unique(iso3)]


p <- qplot(year,
           inc,
           data = subset(est, iso3 %in% hbc.lst  & year >= 2010),
           geom = 'line',
           colour = I('blue')) +
  geom_ribbon(
    aes(
      year,
      ymin = inc.lo,
      ymax = inc.hi
    ),
    fill = I('blue'),
    alpha = I(.4)
  ) +
  geom_line(
    aes(year, inc),
    data = subset(old25, iso3 %in% hbc.lst & year >= 2010),
    colour = I('red'),
    linetype = I(2))+
  scale_x_continuous(breaks=c(2010,2015,2020,2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')

suppressWarnings(ggsave(
  here(paste('inc_mort/analysis/graphs/incidence compare/Incidence_HBCs.pdf', sep = '')),
  plot = p,
  width = 14,
  height = 8
))



### Incidence for countries exempted 

# Incidence of HBC

p <- qplot(year,
           inc,
           data = subset(est, iso3 %in% exemptmod.lst  & year >= 2010),
           geom = 'line',
           colour = I('blue')) +
  geom_ribbon(
    aes(
      year,
      ymin = inc.lo,
      ymax = inc.hi
    ),
    fill = I('blue'),
    alpha = I(.4)
  ) +
  geom_line(
    aes(year, inc),
    data = subset(old25, iso3 %in% exemptmod.lst & year >= 2010),
    colour = I('red'),
    linetype = I(2))+
  scale_x_continuous(breaks=c(2010,2015,2020,2025)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')

suppressWarnings(ggsave(
  here(paste('inc_mort/analysis/graphs/incidence compare/Incidence_ExemptedModel.pdf', sep = '')),
  plot = p,
  width = 14,
  height = 8
))


# # Comparison plots with focus on recent trends, HIV+ incidence

for (i in wr) {
  p <- qplot(
    year,
    inc.h,
    data = subset(est, g.whoregion == i & year >= 2010),
    geom = 'line',
    colour = I('blue')
  ) +
    geom_ribbon(
      aes(
        year,
        ymin = inc.h.lo,
        ymax = inc.h.hi
      ),
      fill = I('blue'),
      alpha = I(.4)
    ) +
    geom_line(
      aes(year, inc.h),
      data = subset(old25, g.whoregion == i & year >= 2010),
      colour = I('red'),
      linetype = I(2)
    ) +
    scale_x_continuous(breaks=c(2010,2015,2020,2025)) +
    facet_wrap( ~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate in HIV+ per 100k/yr')
  suppressWarnings(print(p))
  suppressWarnings(ggsave(here(
    paste('inc_mort/analysis/graphs/incidence compare/Inc_HIVpos_', i, '_compare.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}




# For modeled countries (country specific model)

p <- qplot(year,
           inc,
           data = subset(est, iso3 %in% countrymod.lst & year >= 2010),
           geom = 'line',
           colour = I('blue')) +
  geom_ribbon(
    aes(
      year,
      ymin = inc.lo,
      ymax = inc.hi
    ),
    fill = I('blue'),
    alpha = I(.4)
  ) +
  scale_x_continuous(breaks=c(2010,2015,2020,2025)) +
  geom_line(
    aes(year, inc),
    data = subset(old25, iso3 %in% countrymod.lst  & year >= 2010),
    colour = I('red'),
    linetype = I(2)
  ) +
  
  geom_line(
    aes(year, newinc),
    colour = I('black'),
    linetype = I(2)
  ) +
  
  
  facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Mortality rate per 100k/yr')

suppressWarnings(ggsave(
  here(paste('inc_mort/analysis/graphs/incidence compare/Incidence_CountryModel_compare.pdf', sep = '')),
  plot = p,
  width = 14,
  height = 8
))













# Backup
bckup.est9=copy(est)




### Methodology used to estimate TB incidence
### Combination of historical method until 2019 and methods used in 2020-2024


# Updates on list

# Mongolia and Thailand now using the new approach ; Cambodia one full model
countrymod.lst <- setdiff(countrymod.lst,c("THA","MNG","KHM"))

# New approach
markov.lst <- c("THA","MNG")


est.method$method.inc<- NULL

### UHC SCI direct adjustement
# Full period
est.method[iso3 %in% uhcsci.direct.lst, method.inc:=1]

# 2000-2019 then country model for 2020 onwards
est.method[iso3 %in% uhcsci.direct.lst & iso3 %in% countrymod.lst, method.inc:=2]

# 2000-2019 then regional model for 2020 onwards
est.method[iso3 %in% uhcsci.direct.lst & iso3 %in% regmod.lst, method.inc:=3]


### UHC SCI + statistical model
# Full period
est.method[iso3 %in% uhcsci.statmd.lst, method.inc:=4]

# 2000-2019 then country model for 2020 onwards
est.method[iso3 %in% uhcsci.statmd.lst & iso3 %in% countrymod.lst, method.inc:=5]

# 2000-2019 then regional model for 2020 onwards
est.method[iso3 %in% uhcsci.statmd.lst & iso3 %in% regmod.lst, method.inc:=6]


### TPBS
# Full period
est.method[iso3 %in% tbps.lst, method.inc:=7]

# 2000-2019 then country model for 2020 onwards
est.method[iso3 %in% tbps.lst  & iso3 %in% countrymod.lst, method.inc:=8]

# 2000-2019 then regional model for 2020 onwards
est.method[iso3 %in% tbps.lst & iso3 %in% regmod.lst, method.inc:=9]


### Inventory Study
# Full period
est.method[iso3 %in% is.lst, method.inc:=10]

# 2000-2019 then country model for 2020 onwards
est.method[iso3 %in% is.lst & iso3 %in% countrymod.lst, method.inc:=11]

# 2000-2019 then regional model for 2020 onwards
est.method[iso3 %in% is.lst & iso3 %in% regmod.lst, method.inc:=12]


### Markov model (VR based)
est.method[iso3 %in% markov.lst, method.inc:=13]


### Country customization, not a previous method

# Incorporating round 2 updates
est.method[iso3 %in% adhoc.lst, method.inc:=99]

# Brazil, VR data based replacing UHCSCI
est.method[iso3=="BRA", method.inc:=991]

# 6 African countries for which UHCSCI+Stat not working
est.method[iso3 %in% c("COD","BFA","CAF","GNQ","SLE","SSD"), method.inc:=992]

# Countries with < 10 cases ; no adjustment inc=notif
compact.lst=est[c_newinc<10, unique(iso3)]
est.method[iso3 %in% compact.lst, method.inc:=993]

# Case notification with no adjustment for some countries
est.method[iso3 %in% c("RUS","MDA","KAZ"), method.inc:=994]

# Case notification with Std adjustment (as before)
est.method[iso3 %in% c("ALB","GTM","MAR","SAU","DJI"), method.inc:=995]

### Check
table(est.method$method.inc[est.method$year==2025])
total_sum <- sum(table(est.method$method.inc))
print(total_sum)



# Combine all method lists into a single named list or vector
all_method_lists <- list(
  uhcsci.direct = uhcsci.direct.lst,
  uhcsci.statmd = uhcsci.statmd.lst,
  tbps          = tbps.lst,
  is            = is.lst,
  markov        = markov.lst,
  adhoc         = adhoc.lst
)

all_elements <- unlist(all_method_lists)
duplicated_countries <- unique(all_elements[duplicated(all_elements)])

print("Countries appearing in more than one list:")
print(duplicated_countries)

overlap_check <- lapply(all_method_lists, function(lst) intersect(lst, duplicated_countries))
overlap_df <- data.frame(
  iso3 = duplicated_countries,
  lists = sapply(duplicated_countries, function(iso) {
    paste(names(all_method_lists)[sapply(all_method_lists, function(l) iso %in% l)], collapse = ", ")
  })
)

print(overlap_df)




attr(est.method, "timestamp") <- Sys.Date() #set date
save(est.method, file = here('inc_mort/analysis/est.method.rda'))
fwrite(est.method, file = here(paste0('inc_mort/analysis/csv/est_method_', Sys.Date(), '.csv')))







### Save estimate dataset


#attr(est, "timestamp") <- Sys.Date() #set date
#save(est, file = here('inc_mort/analysis/est.rda'))
fwrite(est, file = here(paste0('inc_mort/analysis/csv/est_inc_', Sys.Date(), '.csv')))





### END Incidence estimation



### Check aggregates 

est <- within(est, {
  inc.num <- inc * pop / m
  inc.lo.num <- inc.lo * pop / m
  inc.hi.num <- inc.hi * pop / m
  inc.nh.num <- inc.nh * pop / m
  inc.nh.lo.num <- inc.nh.lo * pop / m
  inc.nh.hi.num <- inc.nh.hi * pop / m
  inc.h.num <- inc.h * pop / m
  inc.h.lo.num <- inc.h.lo * pop / m
  inc.h.hi.num <- inc.h.hi * pop / m
})


est[, .(
  sums(inc.num),
  sums(inc.nh.num),
  sums(inc.h.num),
  sums(c_newinc)
), by = year]









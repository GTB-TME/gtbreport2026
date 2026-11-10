#' ---
#' title: Global TB Report 2026
#'        Estimate TB mortality
#' author: Mathieu Bastard
#' date: 16/06/2026
#' Update: 27/07/2026



#' # Preamble
#'
#'
#' - Code entirely re-written in 2025 according to the new methods
#'
#' The mortality rate due to TB can be estimated using a variety of methods, including:
#' - Surveillance data: Health authorities can track the number of deaths caused by TB 
#'   that are reported through surveillance systems. This can help to estimate the 
#'   overall TB mortality rate in a population.
#' - Mathematical modeling: Researchers can use mathematical models to estimate TB 
#'   mortality based on data on factors such as population size, TB prevalence, 
#'   and the effectiveness of TB prevention and treatment programs.
#' - Epidemiological (cohort or retrospective cohort) studies: Researchers can 
#'   also conduct epidemiological studies to estimate TB mortality in a specific 
#'   population. These studies can involve collecting data on TB cases and deaths 
#'   and using statistical methods to estimate the overall TB mortality rate. This 
#'   appraoch is not currently used due to the lack of nationally representative 
#'   empirical data. Data from the cohort analysis of TB treatment outcomes suffer from
#'   the following limitations:
#'      - undocumented causes of deaths (all deaths occuring during TB treatment are counted,
#'      including deaths from causes other than TB)
#'      - incomplete reporting (deaths occuring among patients classified as unevaluated or 
#'      lost to follow-up (defaulters) are not accounted for)
#'      - TB deaths occuring among people not registered for TB treatment are not accounted 
#'      for. 
#'
#'
#' This script generates TB mortality estimates with HIV disaggregation using:
#' - vr.rda updated VR data stored in the WHO mortality database, with country-year
#'    data points selected based on data quality criteria as documented in the 
#'    technical appendix and adjusted to account for incomplete coverage and garbage
#'    codes
#' - old.rda previously published mortality estimates, including those originating
#'    from IHME for the period 2000-2019
#' - model.rda, extra.rda output from dynamic models over the period 2020-2024
#' - unaids.rda UNAIDS estimates of AIDS deaths (ensuring that HIV+ TB deaths do 
#'    not exceed AIDS deaths)
#' - R object est as obtained from the previous script in the sequence
#' 
#' Estimates over 2000-2019 are kept from the previously published series, except 
#' in countries with updated VR data in the WHO mortality database (many countries 
#' report with a delay of several years, particularly in E Europe). 
#' 
#' Estimated from IHME were updated as per GBD 2021 study results
#' 
#' GHE estimates of mortality provided by WHO were updated to 2021 estimates
#' 
#' For the period 2020-2026, the approach consists in using one of the following 3 sources:
#' 
#' - Mathematical models output in countries with covid disruptions
#'   
#' - updated VR data from the WHO database
#' 
#' - updated VR data directly obtained from NTP (RUS, CHN, and others in vrtgb) with the usual statistical
#'    adjustments for incomplete coverage and garbage codes, with values hard-coded 
#'    in this script
#' 
#' Deps: 
#'  - libraries data.table and here, 
#'    imputeTS and propagate (as for the previous script in the sequence)
#'    
#'    
#' Input: 
#'  - output from the previous scripts in the sequence
#'  - vr.rda (VR data in the WHO mortality database)
#'  - model.rda and extra.rda (mathematical models output)
#'  - unaids.rda (UNAIDS estimates of HIV burden)
#'  
#'  
#' Output:
#'  - est.rda and dated csv file 
#'  
#'  2026 updates: 
#'  GHE estimates update up to 2023
#'  Imputation after VR data using CFR-based recalibrated approach
#'  Markov Model for countries with good VR data
#'  




rm(list=ls())

# Load libraries and data

library(data.table)
library(imputeTS)
library(zoo)
library(propagate)
library(here)
library(readxl)
library(plotly)
library(htmlwidgets)
library(whomap)
library(gtbreport)
library(RColorBrewer)

#Run Functions script
source(here('inc_mort/R code/fun.R'))

#Function to import db from GTB databases
source(here("import/load_gtb.R"))


# Load data 
data_names <- c("tb", "cty", "pop", "grpmbr")
lapply(data_names, function(x) assign(x, load_gtb(x, convert_dots = FALSE), envir =.GlobalEnv))


# Mathematical model
#Nim results: Update to the most recent one

model=fread("dynamic/output/260706_Estimates.csv",
            col.names=c("iso3","hiv","measure","year","best","lo","hi"))
setkey(model, iso3)





# load old VR data
load(here('inc_mort/estimates2025/vr.rda'))   
ovr=copy(vr)
rm(vr)

#Load new VR data updated
load(here("inc_mort/analysis/vr.rda"))

#load IHME tb mortality estimates
load(here("inc_mort/analysis/ihmetb.rda"))


#load last year estimates data
load(here('inc_mort/estimates2025/est.rda'))
old25=copy(est)
rm(est)

load(here('inc_mort/estimates2024/est.rda'))
old24=copy(est)
rm(est)
old24[iso3=="IDN", g.whoregion:="WPR"]


#load UNAIDS data
load(here('inc_mort/analysis/unaids.rda'))


# Load last inc estimates (from code 3.)
source(here("inc_mort/input_inc/load_last_est_inc.R"))
setkey(est, iso3, year)

# Load lst data with country listings
load(here('inc_mort/estimates2025/est.method.rda'))

# m for rate generation
m <- 1e5

# Current year of estimates
yr <- 2025


vlohi <- Vectorize(lohi, c('ev', 'sd'))


### List of countries for method

# VR based
vr.lst <- unique(est[method.mort %in% c(1, 2, 3), iso3])

# 2026: add Mongolia
vr.lst <- union(vr.lst,"MNG")

# IHME based
ihme.lst <- unique(est[method.mort %in% c(4, 5, 6), iso3])

# 2026: drop Mongolia
ihme.lst <- setdiff(ihme.lst,"MNG")


# CFR based
cfr.lst <- unique(est[method.mort %in% c(7, 8, 9), iso3])

# AdHoc based (none)
adhoc.lst <- unique(est[method.mort %in% c(99), iso3])


length(c(vr.lst,ihme.lst,cfr.lst,adhoc.lst))==215





### List of countries that used model-based mortality in 2024

countrymod.mort24.lst <- unique(est[method.mort %in% c(2, 5, 8), iso3])
regmod.mort24.lst <-  unique(est[method.mort %in% c(3, 6, 9), iso3])

exemptmod.lst <- c(
  "AFG", "AGO", "ALB", "AZE", "BGD", 
  "BLZ", "KGZ", "LSO", "MNG", "PER", 
  "SWZ", "TJK", "VNM", "ZWE",
  "CRI", "MNE", "NAM", "VEN","PHL"
)


countrymod.mort24.lst <- setdiff(countrymod.mort24.lst, exemptmod.lst)
cat("Number of countries remaining in countrymod.lst:", length(countrymod.mort24.lst), "\n")

regmod.mort24.lst <- setdiff(regmod.mort24.lst, exemptmod.lst)
cat("Number of countries remaining in regmod.lst:", length(regmod.mort24.lst), "\n")



### Harmonize model listings: eg, if incidence not using model. mortality should also not


###
### 1. Mortality based on VR data
###    Import updated VR data to the estimates dataset
###    Update with most recent VR data available
###


dim(est)
est <-
  merge(est, vr[, .(
    iso3,
    year,
    vr.keep = keep.vr,
    vr.garbage = garbage,
    vr.coverage,
    vr.quality = codqual,
    vr.env = env,
    # ghe.env, ghe.env.lo, ghe.env.hi,
    vr.mort.nh = tb.adj * m / pop,
    vr.raw = tb * m / pop,
    vr.mort.nh.sd = tb.adj.sd * m / pop
  )],
  by = c('iso3', 'year'), all.x = TRUE)
dim(est)


# incorporate old values of mortality (2000 - 2024)

est <-
  merge(est, old25[, .(iso3,
                     year,
                     mort.nh,
                     mort.nh.sd,
                     mort.h,
                     mort.h.sd,
                     mort,
                     mort.sd)], 
        by = c('iso3', 'year'), all.x = TRUE)
dim(est)



### Update DOM Sept26, drop 2023-2025 import, it will be then imputed
vars_to_na <- c("mort.nh", "mort.nh.sd", "mort.h", "mort.h.sd", "mort", "mort.sd")
est[iso3 == "DOM" & year %in% 2023:2025, (vars_to_na) := NA]


### Update VR estimates in recent year
#   if available and if different from previous time series

psg <- !is.na(est$vr.mort.nh) & est$vr.keep==TRUE & 
       (est$mort.nh!=est$vr.mort.nh | (is.na(est$mort.nh) & !is.na(est$vr.mort.nh))) &
       est$iso3 %in% vr.lst &
       est$year >2019

table(psg)


temp <- est[psg, .(iso3,year,mort.nh,vr.mort.nh)]

est[psg, mort.nh := vr.mort.nh]
est[psg, mort.nh.sd := vr.mort.nh.sd]

# Update PHL stand alone this year
# what about THA

psg <- est$iso3=="PHL"
est[psg, mort.nh := vr.mort.nh]
est[psg, mort.nh.sd := vr.mort.nh.sd]



### For current year, think ... ARIMA?

est[psg, mort.nh := na_locf(mort.nh)]
est[psg, mort.nh.sd := na_locf(mort.nh.sd)]


# check missing values, TRUE: only year==yr
sum(is.na(est$mort.nh) & est$year < yr) == 0  




###
###  2. Markov Model estimates
###

markov_iso3 <- c("MNG","THA")

est_thailand <- fread(here("inc_mort/markovmodel/estimates_thailand_MarkovModel_2026-07-27.csv"))

est[est_thailand[iso3 == "THA"], 
    on = .(iso3, year), 
    `:=` (
      mort    = i.mort, 
      mort.lo = i.mort.lo, 
      mort.hi = i.mort.hi
    )]

est_mongolia <- fread(here("inc_mort/markovmodel/estimates_mongolia_MarkovModel_2026-07-27.csv"))

est[est_mongolia[iso3 == "MNG"], 
    on = .(iso3, year), 
    `:=` (
      mort    = i.mort, 
      mort.lo = i.mort.lo, 
      mort.hi = i.mort.hi
    )]



est[iso3 %in% markov_iso3, `:=`(
  mort.sd  = (mort.hi-mort.lo) / 3.92
)]



ggplot(est[iso3 %in% c(markov_iso3) & year >= 2000, ], aes(x = year, y = mort)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = mort.lo, ymax = mort.hi), fill = "blue", alpha = 0.4) +
  
  geom_line(data=old25[iso3 %in% markov_iso3 & year >= 2000],aes(x=year, y=mort),color = "red") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  facet_wrap(~ iso3, scales = 'free_y') +
  xlab("") +
  ylab("Mortality rate per 100k/yr")








###
### 2. Mortality based on CFR (no VR data)
###    Use inc2mort function to estimate TB mortality based on CFR
###    Both for HIV-neg and HIV-pos mortality
###

#

# Manage ANT and SCG (tweak as these were re imported from older database)
est[iso3 %in% c("ANT","SCG"), imp.newinc := inc]
est[iso3 %in% c("ANT","SCG"), newinc := inc]

est[imp.newinc<0, imp.newinc := 0]

# HIVneg mortality
out1 <-
  est[, {
    tmp = inc2mort(inc, inc.sd, imp.newinc, tbhiv, tbhiv.sd, noHIV =
                     T)$prop

    list(mort.nh = tmp[2],
         mort.nh.sd = tmp[4])
  },
  by = .(iso3, year)]

# HIVpos mortality
out2 <-
  est[, {
    tmp = inc2mort(inc, inc.sd, imp.newinc, tbhiv, tbhiv.sd, noHIV =
                     F)$prop

    list(mort.h = tmp[2],
         mort.h.sd = tmp[4])
  },
  by = .(iso3, year)]


# Store the estimates in variables e*

est[, `:=`(
  e.mort.nh    = out1$mort.nh,
  e.mort.nh.sd = out1$mort.nh.sd,
  e.mort.h     = out2$mort.h,
  e.mort.h.sd  = out2$mort.h.sd,
  e.mort       = out1$mort.nh + out2$mort.h,
  e.mort.sd    = sqrt(out2$mort.h.sd^2 + out1$mort.nh.sd^2)
)]


### For countries using CFR based estimates, replace mort by these estimates
### Due to the update in UHC SCI methods, this impacts also mortality 


psg=est$iso3 %in% cfr.lst |est$iso3 %in% markov_iso3

est[psg, mort.nh := e.mort.nh]
est[psg, mort.nh.sd := e.mort.nh.sd]



###
### 3. Mortality in HIV+, relies only on CFR based approach for ALL countries
###

# For all countries, mort.h=e.mort.h
# Except ZAF, keep previous estimates


est[iso3!="ZAF", `:=`(
  mort.h    = e.mort.h,
  mort.h.sd = e.mort.h.sd
)]


est[iso3!="ZAF" & iso3 %ni% markov_iso3 , `:=`(
  mort      = mort.nh + e.mort.h,
  mort.sd   = sqrt(e.mort.h.sd^2 + mort.nh.sd^2)
)]

# treat THA and MNG

est[iso3 %in% markov_iso3 , `:=`(
  mort.nh      = mort - mort.h,
  mort.nh.sd   = sqrt(mort.sd^2 + mort.h.sd^2)
)]



### Backup data

bckup.est1= copy(est)



###
### 4. Dynamic models, for a set of countries that accounted for COVID disruptions
###                    for which it was decided to continue with the modelling
###
###       (a) Country Specific model, including India model outputs from Sandip Mandall
###       
###       
###       Estimates in these countries are for 2020-2025 and may erase previous estimates 
###       using one of the previous method if applicable
###

countrymod.mort24.lst <- setdiff(countrymod.mort24.lst,c("MNG","THA"))

model_bis <- model[iso3 %in% countrymod.mort24.lst]
model_mort <- model_bis[measure == "mort" & hiv =="a" & year >=2020]
model_mort_iso3 <- unique(model_mort$iso3)

est[model_mort, 
    on = .(iso3, year), 
    `:=`(
      mort    = i.best,
      mort.lo = i.lo,
      mort.hi = i.hi
    )]

est[iso3 %in% model_mort_iso3 & year>=2020, `:=`(
  mort.sd  = (mort.hi-mort.lo) / 3.92
)]


est[iso3 %in% model_mort_iso3 & year>=2020, `:=`(
  mort.nh    = mort - mort.h,
  mort.nh.sd = sqrt(mort.sd^2 + mort.h.sd^2)
)]


# mort hiv+
model_mort_h <- model_bis[measure == "mort" & hiv =="pos" & year >=2020]

est[model_mort_h, 
    on = .(iso3, year), 
    `:=`(
      mort.h    = i.best,
      mort.h.lo = i.lo,
      mort.h.hi = i.hi
    )]



# For these countries, re estimate mort.nh as mort-mort.h and SD from 2020 onwards

model_mort_h_iso3 <- unique(model_mort_h$iso3)

est[iso3 %in% model_mort_h_iso3 & year>=2020, `:=`(
  mort.nh    = mort - mort.h,
  mort.h.sd  = (mort.h.hi-mort.h.lo) / 3.92,
  mort.nh.sd = sqrt(mort.sd^2 + mort.h.sd^2)
)]

# re-estimate bounds

psg <- est$iso3 %in% model_mort_h_iso3 & est$year>=2020
out <- vlohi(est$mort.nh[psg]/m, est$mort.nh.sd[psg]/m)
est$mort.nh.lo[psg] <- out[1,]*m
est$mort.nh.hi[psg] <- out[2,]*m


### Backup data

bckup.est2= copy(est)



###
### 5. Missing estimates in current year 
###

missing_mort_records <- est[year==yr & (
  is.na(mort) | is.na(mort.nh) | is.na(mort.h)
  ), 
  .(iso3, year, mort, mort.nh, mort.h, e.mort.nh, e.mort.h)
]

print("Country-years with missing data in mortality variables:")
print(missing_mort_records)

unique_iso3_missing_mort <- sort(unique(na.omit(missing_mort_records$iso3)))

cat("\nUnique ISO3 codes with missing mortality data:\n")
cat(paste(unique_iso3_missing_mort, collapse = ", "), "\n")


# If based on VR ONLY (not modeled)
# 
# psg <- est$iso3 %in% intersect(vr.lst,unique_iso3_missing_mort)
# 
# est[psg, `:=`(
#   mort.nh    = na_kalman(mort.nh),
#   mort.nh.sd = na_locf(mort.nh.sd)
# ), by = iso3]  
# 
# est[psg & year==yr, `:=`(  
#   mort       = mort.nh + mort.h,
#   mort.sd    = sqrt(mort.nh.sd^2 + mort.h.sd^2)
# ), by = iso3]

setorder(est, iso3, year)

est[, has_enough_data := sum(!is.na(vr.mort.nh)) >= 3, by = iso3]
est[, imputed_vr := vr.mort.nh]


# Specific countries to not change anything due to previous discussions
no.imputectry.lst <- c("SAU",markov_iso3)
no.impute.lst <- union(countrymod.mort24.lst,no.imputectry.lst)



est[iso3 %in% vr.lst & iso3 %ni% no.impute.lst & has_enough_data == TRUE, imputed_vr := {
  
  # A. Linear interpolation for internal gaps
  v <- na.approx(vr.mort.nh, na.rm = FALSE)
  
  # B. Kalman for the head
  v <- na_kalman(v, model = "StructTS")
  
  # C. Tail Projection
  last_idx <- max(which(!is.na(vr.mort.nh)))
  e_ratio <- e.mort.nh / (shift(e.mort.nh)+0.01)
  
  if (last_idx < length(v)) {
    for (i in (last_idx + 1):length(v)) {
      if (!is.na(e_ratio[i])) v[i] <- v[i - 1] * e_ratio[i]
    }
  }
  
  # Return the result as a vector
  v
}, by = iso3]

# Replace mort.nh by the new imputed TS
est[iso3 %in% vr.lst & iso3 %ni% no.impute.lst & has_enough_data ==T, mort.nh := imputed_vr]
est[iso3 %in% vr.lst & iso3 %ni% no.impute.lst & has_enough_data ==T, mort.nh.sd := na_locf(mort.nh.sd)]


# Re estimate mort = mort.nh+mort.h
est[iso3 %in% vr.lst & iso3 %ni% no.impute.lst & has_enough_data ==T, `:=`(  
  mort       = mort.nh + mort.h,
  mort.sd    = sqrt(mort.nh.sd^2 + mort.h.sd^2)
), by = iso3]


# Tweak PHL 2025 SD; 2* 2024
est[iso3=="PHL" & year>2020,.(year,mort,mort.sd)]
est[iso3=="PHL" & year==2025,mort.sd := est$mort.sd[est$iso3=="PHL" & est$year==2024]*2]


# For the others, na kalman

psg <- est$iso3 %in% no.imputectry.lst |
       (est$iso3 %in% vr.lst & est$iso3 %ni% countrymod.mort24.lst & est$has_enough_data ==F)

est[psg, `:=`(
  mort.nh    = na_kalman(mort.nh),
  mort.nh.sd = na_locf(mort.nh.sd)
), by = iso3]

est[psg & year==yr, `:=`(
  mort       = mort.nh + mort.h,
  mort.sd    = sqrt(mort.nh.sd^2 + mort.h.sd^2)
), by = iso3]




# If based on IHME, using NA KALMAN

psg <- est$iso3 %in% intersect(ihme.lst,unique_iso3_missing_mort)

est[psg, `:=`(
  mort.nh    = na_kalman(mort.nh),
  mort.nh.sd = na_locf(mort.nh.sd)
), by = iso3]  

est[psg & year==yr, `:=`(  
  mort       = mort.nh + mort.h,
  mort.sd    = sqrt(mort.nh.sd^2 + mort.h.sd^2)
), by = iso3]


# If using CFR based estimates, complete with e.mort.nh

psg <- est$iso3 %in% intersect(cfr.lst,unique_iso3_missing_mort) & est$year==yr

est[psg, `:=`(
  mort.nh    = e.mort.nh,
  mort.nh.sd = e.mort.nh.sd,
  mort       = mort.nh + mort.h,
  mort.sd    = sqrt(mort.nh.sd^2 + mort.h.sd^2)
), by = iso3]  



# Those still missing
missing_mort_records <- est[year==yr & (
  is.na(mort) | is.na(mort.nh) | is.na(mort.h)
), 
.(iso3, year, mort, mort.nh, mort.h, e.mort.nh, e.mort.h)
]

print("Country-years with missing data in mortality variables:")
print(missing_mort_records)

# ZAF, impute mort.h and derive mort.nh
psg <- est$iso3 == "ZAF"

est[psg, `:=`(
  mort.h    = na_kalman(mort.h),
  mort.h.sd = na_kalman(mort.h.sd)
), by = iso3]  

est[psg & year==yr, `:=`(  
  mort       = mort.nh + mort.h,
  mort.sd    = sqrt(mort.nh.sd^2 + mort.h.sd^2)
), by = iso3]


est[iso3=="ZAF",.(year,mort,mort.nh,mort.h,mort.sd,mort.nh.sd,mort.h.sd)]





### Check missing data on mortality globally :

vars_to_check <- c("mort", "mort.sd", "mort.nh", "mort.nh.sd", "mort.h", "mort.h.sd")

missing_summary_year <- est[, lapply(.SD, function(x) sum(is.na(x))), 
                            .SDcols = vars_to_check, 
                            by = year]

missing_iso3_list <- est[, lapply(.SD, function(x) paste(unique(iso3[is.na(x)]), collapse = ", ")), 
                         .SDcols = vars_to_check, 
                         by = year]

print("Missing counts per year:")
print(missing_summary_year)

print("ISO3s with missing data per year:")
print(missing_iso3_list)



p <- est %>%
  filter(year >= 2010, iso3 %in% vr.lst ) %>%
  ggplot(aes(x = year)) +
  # Ribbon for uncertainty
  geom_ribbon(aes(ymin = mort - 1.96 * mort.sd, 
                  ymax = mort + 1.96 * mort.sd),
              fill = "blue", alpha = 0.4) +
  # Main line (est)
  geom_line(aes(y = mort), color = "blue", linewidth = 1) +
  # Old comparison line (old25)
  geom_line(data = old25 %>% filter(year >= 2010, iso3 %in% vr.lst) ,
            aes(y = mort), color = "red", linetype = "dashed") +
  # Aesthetics
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  facet_wrap(~ iso3, scales = "free_y") +
  labs(x = "", y = "Rate per 100,000/year") +
  theme_minimal()

# Print and Save
print(p)

ggsave(here(paste0('inc_mort/analysis/graphs/mortality/Mortality_VR_countries.pdf')),
       plot = p, width = 14, height = 8)






###
###  6. Country specific updates
###

### Mongolia, use VR data for the full time series as replacement of IHME
# Impute before 2010
# est[iso3 == "MNG", vr.mort.nh:= na_kalman(vr.mort.nh)]
# est[iso3 == "MNG" , vr.mort.nh.sd := na_locf(vr.mort.nh.sd)]
# est[iso3 == "MNG",.(iso3,year,mort,vr.mort.nh,vr.mort.nh.sd,mort.h,e.mort.h)]
# 
# est[iso3 == "MNG", mort.nh:= vr.mort.nh]
# est[iso3 == "MNG", mort.nh.sd:= vr.mort.nh.sd]
# 
# est[iso3 == "MNG", mort:= mort.nh+mort.h]
# est[iso3 == "MNG", mort.sd:= sqrt(mort.nh.sd^2 + mort.h.sd^2)]
# 
# est[iso3 == "MNG",.(iso3,year,mort,mort.sd, mort.nh,mort.nh.sd,mort.h,mort.h.sd)]



### VietNam, use VR data for the full time series as replacement of IHME

est[iso3 == "VNM",.(iso3,year,mort,mort.sd, mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort.h.sd,e.mort.nh)]
old25[iso3 == "VNM",.(iso3,year,mort,mort.sd, mort.nh,mort.nh.sd,mort.h,mort.h.sd,e.mort.nh)]


ggplot(data=est[iso3=="VNM" & year>2009], aes(x=year, y=mort))+
  geom_line(color="blue") +
  geom_line(data=old25[iso3=="VNM" & year>2009], aes(x=year, y=mort), color="red")

#Very big difference in mortality, back to previous estimate until we find a way...
# rescaled using 2019 as the multiplier

multiplier <- old25[iso3 == "VNM" & year == 2019, mort] / 
  est[iso3 == "VNM" & year == 2019, mort]

cols_to_update <- c("mort", "mort.sd", "mort.nh", "mort.nh.sd", 
                    "mort.h", "mort.h.sd")

est[iso3 == "VNM", (cols_to_update) := lapply(.SD, function(x) x * multiplier), 
    .SDcols = cols_to_update]


ggplot(data=est[iso3=="VNM" & year>2000], aes(x=year, y=mort))+
  geom_line(color="blue") +
  geom_line(data=old25[iso3=="VNM" & year>2000], aes(x=year, y=mort), color="red")

est[iso3 == "VNM",.(iso3,year,mort,mort.sd, mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort.h.sd)]


### Backup data
bckup.est3=copy(est)


### Plot before bounds
# 
# wr <- c('AMR', 'AFR', 'EMR', 'EUR', 'SEA', 'WPR')
# 
# for (i in wr) {
#   p <-
#     qplot(
#       year,
#       mort,
#       data = subset(est, g.whoregion == i & year>=2010),
#       geom = 'line',
#       colour = I('blue')
#     ) +
#     geom_ribbon(
#       aes(year, ymin = mort-1.96*mort.sd, ymax = mort+1.96*mort.sd),
#       fill = I('blue'),
#       alpha = I(0.4)
#     ) +
#     geom_line(
#       aes(year, mort),
#       data = subset(old25, g.whoregion == i & year>=2010),
#       colour = I('red'),
#       linetype = I(2)
#     ) +
#     scale_x_continuous(breaks=c(2010,2015,2020,2025)) +
#     
#     facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')
#   suppressWarnings(print(p))
#   
#   suppressWarnings(ggsave(here(
#     paste('inc_mort/analysis/graphs/mortality/Mortality_', i, '_compare_last_year.pdf', sep = '')
#   ),
#   width = 14,
#   height = 8))
# }



### Plot for countries exempted from the model this year

exemptmod.lst <- union(exemptmod.lst,"THA")

p <- est %>%
  filter(year >= 2010, iso3 %in% exemptmod.lst) %>%
  ggplot(aes(x = year)) +
  # Ribbon for uncertainty
  geom_ribbon(aes(ymin = mort - 1.96 * mort.sd, 
                  ymax = mort + 1.96 * mort.sd),
              fill = "blue", alpha = 0.4) +
  # Main line (est)
  geom_line(aes(y = mort), color = "blue", linewidth = 1) +
  # Old comparison line (old25)
  geom_line(data = old25 %>% filter(year >= 2010, iso3 %in% exemptmod.lst),
            aes(y = mort), color = "red", linetype = "dashed") +
  # Aesthetics
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  facet_wrap(~ iso3, scales = "free_y") +
  labs(x = "", y = "Rate per 100,000/year") +
  theme_minimal()

# Print and Save
print(p)

ggsave(here(paste0('inc_mort/analysis/graphs/mortality/Mortality_ExemptModel_compare_last_year.pdf')),
       plot = p, width = 14, height = 8)






### India : copy last year estimates for 2020:2024 and LOCF 2025
# To be removed once model completed

mort_vars <- c("mort","mort.lo","mort.hi", "mort.sd", "mort.nh", "mort.nh.sd", 
               "mort.h", "mort.h.sd")

est[iso3 == "IND" & year %in% 2000:2022, 
    (mort_vars) := old25[iso3 == "IND" & year %in% 2000:2022, .SD, .SDcols = mort_vars]]


#est[iso3 == "IND" & year == 2025, (mort_vars) := est[iso3 == "IND" & year == 2024, .SD, .SDcols = mort_vars]]

load(here("inc_mort/analysis/india_final.rda"))

for (y in 2023:yr) {
  est[iso3 == "IND" & year == y, `:=` (
    mort    = india_final[year == y, mort.mid],
    mort.lo = india_final[year == y, mort.lo],
    mort.hi = india_final[year == y, mort.hi]
  )]
  
  est[iso3 == "IND" & year == y, mort.sd := (mort.hi - mort.lo) / 3.92]
  est[iso3 == "IND" & year == y, mort.nh := mort - mort.h]
  est[iso3 == "IND" & year == y, mort.nh.sd := sqrt(mort.sd^2 + mort.h^2)]
}





###
###  7. Bounds
###


# If estimates are 0, bounds set to 0

est[mort==0, mort.lo:=0]
est[mort==0, mort.hi:=0]
est[mort.nh==0, mort.nh.lo:=0]
est[mort.nh==0, mort.nh.hi:=0]
est[mort.h==0, mort.h.lo:=0]
est[mort.h==0, mort.h.hi:=0]


### If no bounds yet (not imported from model) and mort !=0 and not model country (their bands are already provided)
### For model country, estimate bounds though in <=2019
### 

#mort

psg= (est$mort!=0 & est$iso3 %ni% countrymod.mort24.lst ) |
     (est$mort!=0 & est$year %in% 2000:2019 & est$iso3 %in% countrymod.mort24.lst)


out <- vlohi(est$mort[psg]/m, est$mort.sd[psg]/m)
est$mort.lo[psg] <- out[1,]*m
est$mort.hi[psg] <- out[2,]*m



#mort.nh
psg= est$mort.nh!=0

# Fix SD 20% of central estimate if SD=0
est[mort.nh!=0 & mort.nh.sd==0, mort.nh.sd:=0.2*mort.nh]

out <- vlohi(est$mort.nh[est$mort.nh>0]/m, est$mort.nh.sd[est$mort.nh>0]/m)
est$mort.nh.lo[est$mort.nh>0] <- out[1,]*m
est$mort.nh.hi[est$mort.nh>0] <- out[2,]*m


#mort.h
 psg= (est$mort.h!=0 & (est$iso3 %ni% model_mort_h_iso3 )) |
      (est$mort.h!=0 & est$year %in% 2000:2019 & est$iso3 %in% model_mort_h_iso3)


# Fix SD 20% of central estimate if SD=0

est[mort.h!=0 & mort.h.sd==0, mort.h.sd:=0.2*mort.h]
out <- vlohi(est$mort.h[psg]/m, est$mort.h.sd[psg]/m)
est$mort.h.lo[psg] <- out[1,]*m
est$mort.h.hi[psg] <- out[2,]*m



### Check missing
est[, .(sum(is.na(mort)), sum(is.na(mort.lo)),sum(is.na(mort.hi)),
        sum(is.na(mort.nh)), sum(is.na(mort.nh.lo)), sum(is.na(mort.nh.hi)),
        sum(is.na(mort.h)), sum(is.na(mort.h.lo)), sum(is.na(mort.h.hi))
        ), by=year]



# Check that TB mortality in HIV+ is not higher than UNAIDS official estimates
# mort.h greater than mort.hiv?

psg <- est$mort.h >= est$mort.hiv & est$mort.h>0 & est$mort.hiv > 0 
table(psg)
mort.h.corr.lst=est[psg, unique(iso3)]
est[psg, table(year)]
est[psg, summary(mort.h/mort.hiv)]
est[!psg & mort.hiv>0, summary(mort.h/mort.hiv)]

ggplot(data=est[iso3 %in% mort.h.corr.lst & year >= 2010,],
       aes(x=year,y=mort.h)) + 
  geom_line() +
  geom_line(aes(x=year,y=mort.hiv), color="red") +
  facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')

  


# Set at 80% of UNAIDS enveloppe
est[psg, mort.h := mort.hiv*0.8]
est[psg, mort.h.sd := mort.h * 0.25]
est[psg, mort.nh := mort - mort.h]
#Assuming cor of mort and mort.h is 0.5
est[psg, mort.nh.sd := sqrt(mort.sd^2 + mort.h.sd^2 - 2*0.5*mort.sd*mort.h.sd)]

# Restimate bounds for mort.h and mort.nh (mort remains unchange)
out <- vlohi(est$mort.h[psg & !is.na(psg)]/m, est$mort.h.sd[psg & !is.na(psg)]/m)
est$mort.h.lo[psg & !is.na(psg)] <- out[1,]*m
est$mort.h.hi[psg & !is.na(psg)] <- out[2,]*m

out <- vlohi(est$mort.nh[psg & !is.na(psg)]/m, est$mort.nh.sd[psg & !is.na(psg)]/m)
est$mort.nh.lo[psg & !is.na(psg)] <- out[1,]*m
est$mort.nh.hi[psg & !is.na(psg)] <- out[2,]*m






### Refine estimates that look odd in COVID period

###Stop here

refine.iso3 <- c("BGD")

est[iso3 %in% refine.iso3 & year >= 2019 & year <= 2024, c("mort", "mort.lo", "mort.hi","mort.nh","mort.nh.lo","mort.nh.hi") := .(NA, NA, NA, NA, NA, NA)]

est[iso3 %in% refine.iso3, `:=`(
  mort    = na.approx(mort, x = year, rule = 2, na.rm = FALSE),
  mort.lo = na.approx(mort.lo, x = year, rule = 2, na.rm = FALSE),
  mort.hi = na.approx(mort.hi, x = year, rule = 2, na.rm = FALSE)
), by = iso3]

est[iso3 %in% refine.iso3 & year >= 2011 & year <= 2024, `:=`(
  mort.nh    = mort-mort.h,
  mort.nh.sd = sqrt(mort.sd^2+mort.h.sd^2)
), by = iso3]

est[iso3 %in% refine.iso3, `:=`(
  mort.nh.lo    = na.approx(mort.nh.lo, x = year, rule = 2, na.rm = FALSE),
  mort.nh.hi    = na.approx(mort.nh.hi, x = year, rule = 2, na.rm = FALSE)
), by = iso3]


p <- est %>%
  filter(year >= 2010, iso3 %in% refine.iso3) %>%
  ggplot(aes(x = year)) +
  # Ribbon for uncertainty
  geom_ribbon(aes(ymin = mort.lo, 
                  ymax = mort.hi),
              fill = "blue", alpha = 0.4) +
  # Main line (est)
  geom_line(aes(y = mort), color = "blue", linewidth = 1) +
  # Old comparison line (old25)
  geom_line(data = old25 %>% filter(year >= 2010, iso3  %in% refine.iso3),
            aes(y = mort), color = "red", linetype = "dashed") +
  # Aesthetics
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  facet_wrap(~ iso3, scales = "free_y") +
  labs(x = "", y = "Rate per 100,000/year") +
  theme_minimal()




###
### Incorporate HIV+ TB mortality based on their VR data
### Data shared for 2019-2025 by Daiane Alves da Silva on July 22

#Input data
hivd_bra <- vr[iso3=="BRA"]
hivd_bra[year %in% 2019:2025, tbhivd := c(1625, 1632, 1786, 1835, 1894, 2044, 2057)]

# Adjusted for ill def and SD
hivd_bra[, tbhivd_adj := env * tbhivd/(total-ill_def)]
hivd_bra[, tbhivd_adj.sd := (tbhivd_adj / 4) * (1 / (vr.coverage * (1 - garbage)) - 1)]

#Rate
hivd_bra[, mort.h := tbhivd_adj*1e5/pop]
hivd_bra[, mort.h.sd := tbhivd_adj.sd*1e5/pop]

#Import into est
est[iso3=="BRA", .(year, mort.h, mort.h.sd, mort.h.lo,mort.h.hi )]

est[iso3=="BRA", `:=` (
  mort.h    = NA_real_,
  mort.h.sd = NA_real_,
  mort.h.lo = NA_real_,
  mort.h.hi = NA_real_
)]

hivd_bra <- hivd_bra[,.(iso3,year,mort.h, mort.h.sd)]

est[hivd_bra, 
    on = .(iso3, year), 
    `:=` (
      mort.h    = i.mort.h,
      mort.h.sd = i.mort.h.sd
    )]


# impute missing years using predicted mort.h calibrated to 2019
ratio_h  <- est[iso3 == "BRA" & year == 2019, mort.h / e.mort.h]
ratio_sd <- est[iso3 == "BRA" & year == 2019, mort.h.sd / e.mort.h.sd]

est[iso3 == "BRA" & year < 2019, `:=` (
  mort.h    = e.mort.h * ratio_h,
  mort.h.sd = e.mort.h.sd * ratio_sd
)]


#bounds
psg <- est$iso3=="BRA"
out <- vlohi(est$mort.h[psg]/m, est$mort.h.sd[psg]/m)
est$mort.h.lo[psg] <- out[1,]*m
est$mort.h.hi[psg] <- out[2,]*m

est[iso3=="BRA", .(year, mort.h, mort.h.sd, mort.h.lo,mort.h.hi,e.mort.h,e.mort.h.sd )]


# total death
est[iso3=="BRA", mort:=mort.nh+mort.h]
est[iso3=="BRA", mort.sd:=sqrt(mort.nh.sd^2+mort.h.sd^2)]

psg <- est$iso3=="BRA"
out <- vlohi(est$mort[psg]/m, est$mort.sd[psg]/m)
est$mort.lo[psg] <- out[1,]*m
est$mort.hi[psg] <- out[2,]*m



### MRT, LOCF 2023 in 2024 and 2025
vars <- c("mort", "mort.lo", "mort.hi",
          "mort.nh", "mort.nh.lo", "mort.nh.hi",
          "mort.h", "mort.h.lo", "mort.h.hi")

est[iso3 == "MRT" & year >= 2024 & year <= 2025, (vars) := NA]

est[iso3 == "MRT", (vars) := lapply(.SD, function(x) na_locf(x)), 
    .SDcols = vars, by = iso3]


### YEM, LOCF 2024 in 2025
vars <- c("mort", "mort.lo", "mort.hi",
          "mort.nh", "mort.nh.lo", "mort.nh.hi",
          "mort.h", "mort.h.lo", "mort.h.hi")

est[iso3 == "YEM" & year ==yr, (vars) := NA]

est[iso3 == "YEM", (vars) := lapply(.SD, function(x) na_locf(x)), 
    .SDcols = vars, by = iso3]



#
# Add absolut number for mortality and incidence
#


est <- within(est, {
  mort.num <- mort * pop / m
  mort.lo.num <- mort.lo * pop / m
  mort.hi.num <- mort.hi * pop / m

  mort.nh.num <- mort.nh * pop / m
  mort.nh.lo.num <- mort.nh.lo * pop / m
  mort.nh.hi.num <- mort.nh.hi * pop / m

  mort.h.num <- mort.h * pop / m
  mort.h.lo.num <- mort.h.lo * pop / m
  mort.h.hi.num <- mort.h.hi * pop / m

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



### Check aggregates over time vs last year


est[, .(
  sums(mort.nh.num),
  sums(mort.h.num),
  sums(mort.num),
  sums(inc.num),
  sums(inc.nh.num),
  sums(inc.h.num),
  sums(c_newinc),
  sums(mort.nh.num) / sums(inc.nh.num)
), by = year]


old25[, .(
  sums(mort.nh.num),
  sums(mort.h.num),
  sums(mort.num),
  sums(inc.num),
  sums(inc.nh.num),
  sums(inc.h.num),
  sums(c.newinc),
  sums(mort.nh.num) / sums(inc.nh.num)
), by = year]






### Backup data

bckup.est5=copy(est)



###
### CFR calculation
###


cfr <- est[,
           {
             tmp = divXY(mort, inc, mort.sd, inc.sd)
             list(cfr = tmp[[1]],
                  cfr.sd = tmp[[2]])
           }, by = c("iso3", "year")]

# Set CFR to a maximum of 1
cfr$cfr[cfr$cfr > 1 & !is.na(cfr$cfr)] <- 1

# Calculate lower and upper bounds using the beta distribution
# The final condition (cfr$cfr.sd^2 < (cfr$cfr*(1-cfr$cfr))) suggested
# and therefore we approximate the bounds using the normal distribution

sel_beta <- cfr$cfr > 0 & cfr$cfr < 1 & !is.na(cfr$cfr) & !is.na(cfr$cfr.sd) & cfr$cfr.sd !=0 & (cfr$cfr.sd^2 < (cfr$cfr*(1-cfr$cfr)))


out <- with(cfr[sel_beta], vlohi(cfr, cfr.sd))

cfr$cfr.lo[sel_beta] <- out[1,]
cfr$cfr.hi[sel_beta] <- out[2,]

# Apply the normal distribution to countries where the beta distribution was not applicable
sel_norm <- !sel_beta & !is.na(cfr$cfr.sd)
cfr$cfr.lo[sel_norm] <- cfr$cfr[sel_norm]-1.96*cfr$cfr.sd[sel_norm]
cfr$cfr.hi[sel_norm] <- cfr$cfr[sel_norm]+1.96*cfr$cfr.sd[sel_norm]

# Set lower bound of CFR to a minimum of 0
cfr$cfr.lo[sel_norm & cfr$cfr.lo < 0 ] <- 0

# Set upper bound of CFR to a maximum of 1
cfr$cfr.hi[sel_norm & cfr$cfr.hi > 1 ] <- 1

# Set all remaining values to NA
cfr$cfr[!sel_beta & !sel_norm] <- NA
cfr$cfr.sd[!sel_beta & !sel_norm] <- NA
cfr$cfr.lo[!sel_beta & !sel_norm] <- cfr$cfr.hi[!sel_beta & !sel_norm] <- NA

dim(est)
est=merge(est,cfr,by=c("iso3","year"),all.x = T)
dim(est)



###
### Final plots of mortality
###


wr <- unique(as.character(est$g.whoregion))

# Mortality total

for (i in wr) {
  p <-
    qplot(
      year,
      mort,
      data = subset(est, g.whoregion == i & year>=2010),
      geom = 'line',
      colour = I('blue')
    ) +
    geom_ribbon(
      aes(year, ymin = mort.lo, ymax = mort.hi),
      fill = I('blue'),
      alpha = I(0.4)
    ) +
    geom_line(
      aes(year, mort),
      data = subset(old25, g.whoregion == i & year>=2010),
      colour = I('red'),
      linetype = I(2)
    ) +
    scale_x_continuous(breaks=c(2010,2015,2020,2025)) +
    
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')
  suppressWarnings(print(p))

  suppressWarnings(ggsave(here(
    paste('inc_mort/analysis/graphs/mortality/Mortality_', i, '_compare_last_year.pdf', sep = '')
  ),
  width = 14,
  height = 8))
  
  #saveWidget(ggplotly(p), here(paste('inc_mort/output/checks/mortality_', i, '_compare.html', sep = '')))
  
}


# Mortality HBCs

hbc.lst <- est[g.hbc == T, unique(iso3)]

p <- qplot(year,
           mort,
           data = subset(est, iso3 %in% hbc.lst  & year >= 2010),
           geom = 'line',
           colour = I('blue')) +
  geom_ribbon(
    aes(
      year,
      ymin = mort.lo,
      ymax = mort.hi
    ),
    fill = I('blue'),
    alpha = I(.4)
  ) +
  geom_line(
    aes(year, mort),
    data = subset(old25, iso3 %in% hbc.lst & year >= 2010),
    colour = I('red'),
    linetype = I(2))+
  geom_ribbon(
    aes(
      year,
      ymin = mort.lo,
      ymax = mort.hi
    ),
    data = subset(old25, iso3 %in% hbc.lst & year >= 2010),
    fill = I('red'),
    alpha = I(.4)
  ) +
  scale_x_continuous(breaks=c(2010,2015,2020,2025)) +
  facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Moratality rate per 100k/yr')

suppressWarnings(ggsave(
  here(paste('inc_mort/analysis/graphs/mortality/Moratality_HBCs.pdf', sep = '')),
  plot = p,
  width = 14,
  height = 8
))

#saveWidget(ggplotly(p), here(paste('inc_mort/output/checks/mortality_HBC_compare.html')))


# Mortality HIV positive


for (i in wr) {
  p <-
    qplot(
      year,
      mort.h,
      data = subset(est, g.whoregion == i & year>=2010),
      geom = 'line',
      colour = I('blue')
    ) +
    geom_ribbon(
      aes(year, ymin = mort.h.lo, ymax = mort.h.hi),
      fill = I('blue'),
      alpha = I(0.4)
    ) +
    geom_line(
      aes(year, mort.h),
      data = subset(old25, g.whoregion == i & year>=2010),
      colour = I('red'),
      linetype = I(2)
    ) +
    scale_x_continuous(breaks=c(2010,2015,2020,2025)) +

    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')
  suppressWarnings(print(p))

  suppressWarnings(ggsave(here(
    paste('inc_mort/analysis/graphs/mortality/Moratality_HIVpos_', i, '_compare_last_year.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}

# Mortality HIV negative


for (i in wr) {
  p <-
    qplot(
      year,
      mort.nh,
      data = subset(est, g.whoregion == i & year>=2010),
      geom = 'line',
      colour = I('blue')
    ) +
    geom_ribbon(
      aes(year, ymin = mort.nh.lo, ymax = mort.nh.hi),
      fill = I('blue'),
      alpha = I(0.4)
    ) +
    geom_line(
      aes(year, mort.nh),
      data = subset(old25, g.whoregion == i & year>=2010),
      colour = I('red'),
      linetype = I(2)
    ) +
    scale_x_continuous(breaks=c(2010,2015,2020,2025)) +
    
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')
  suppressWarnings(print(p))

  suppressWarnings(ggsave(here(
    paste('inc_mort/analysis/graphs/mortality/Mortality_HIVneg_', i, '_compare_last_year.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}




# Mortality for modeled countries (country specific model)

p <- qplot(year,
           mort,
           data = subset(est, iso3 %in% countrymod.mort24.lst & year >= 2010),
           geom = 'line',
           colour = I('blue')) +
  geom_ribbon(
    aes(
      year,
      ymin = mort.lo,
      ymax = mort.hi
    ),
    fill = I('blue'),
    alpha = I(.4)
  ) +
  scale_x_continuous(breaks=c(2010,2015,2020,2025)) +
  geom_line(
    aes(year, mort),
    data = subset(old25, iso3 %in% union(countrymod.mort24.lst,regmod.mort24.lst)  & year >= 2010),
    colour = I('red'),
    linetype = I(2)
  ) +
  facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Mortality rate per 100k/yr')

suppressWarnings(ggsave(
  here(paste('inc_mort/analysis/graphs/mortality/Mortality_Model_Country_compare_last_year.pdf', sep = '')),
  plot = p,
  width = 14,
  height = 8
))




# Save

attr(est, "timestamp") <- Sys.Date() #set date
save(est, file = here('inc_mort/analysis/est.rda'))
# fwrite(est, file = here(paste0('inc_mort/analysis/csv/est_inc_mort_', Sys.Date(), '.csv')))



### Methodology used to estimate TB mortality
### Combination of historical method until 2019 and methods used in 2020-2024

#Load method dataset
load(here("inc_mort/analysis/est.method.rda"))

est.method$method.mort<- NULL

# Changes in listing
countrymod.mort24.lst <- setdiff(countrymod.mort24.lst,"THA")
ihme.lst <- setdiff(ihme.lst,"KHM")

model.tbps <- c("KHM")


# VR data used also now in PHL, THA, MNG
vr.lst <- union(vr.lst, c("PHL", "THA", "MNG"))


### VR data
# Full period
est.method[iso3 %in% vr.lst, method.mort:=1]

# 2000-2019 then country model for 2020 onwards
est.method[iso3 %in% vr.lst & iso3 %in% countrymod.mort24.lst, method.mort:=2]

# 2000-2019 then regional model for 2020 onwards
est.method[iso3 %in% vr.lst & iso3 %in% regmod.mort24.lst, method.mort:=3]


### IHME
# Full period
est.method[iso3 %in% ihme.lst, method.mort:=4]

# 2000-2019 then country model for 2020 onwards
est.method[iso3 %in% ihme.lst & iso3 %in% countrymod.mort24.lst, method.mort:=5]

# 2000-2019 then regional model for 2020 onwards
est.method[iso3 %in% ihme.lst & iso3 %in% regmod.mort24.lst, method.mort:=6]


### CFR based
# Full period
est.method[iso3 %in% cfr.lst, method.mort:=7]

# 2000-2019 then country model for 2020 onwards
est.method[iso3 %in% cfr.lst & iso3 %in% countrymod.mort24.lst, method.mort:=8]

# 2000-2019 then regional model for 2020 onwards
est.method[iso3 %in% cfr.lst & iso3 %in% regmod.mort24.lst, method.mort:=9]

### Model only from TBPS survey (Cambodia)
est.method[iso3 %in% model.tbps, method.mort:=10]



all_lists <- c(vr.lst, ihme.lst, cfr.lst, model.tbps)
duplicates <- unique(all_lists[duplicated(all_lists)])
print(duplicates)

missing_countries <- setdiff(unique(est$iso3), union(vr.lst, union(ihme.lst, union(cfr.lst, model.tbps))))
print(missing_countries)




#
#
# ### Dataset for methods
#

attr(est.method, "timestamp") <- Sys.Date() #set date
save(est.method, file = here('inc_mort/analysis/est.method.rda'))
fwrite(est.method, file = here(paste0('inc_mort/analysis/csv/est_method_', Sys.Date(), '.csv')))


# 
# ### Quick Maps of methods
# 
# incmap=est.method[,.(iso3,var=method.inc)]
# 
# inc.map <- whomap(
#   X = incmap, water.col = 'white',
#   legend.title = 'Method used',
#   colours = brewer.pal(12, "Set3"),
#   legend.pos = c(0.11,0.48)
# )
# ggsave(here("inc_mort/output/checks/method_inc.pdf"),width = 14,height = 8)
# 
# 
# ### END Mortality estimation
# mortmap=est.method[,.(iso3,var=method.mort)]
# 
# mort.map <- whomap(
#   X = mortmap, water.col = 'white',
#   legend.title = 'Method used',
#   colours = brewer.pal(9, "Set3"),
#   legend.pos = c(0.11,0.48)
# )
# ggsave(here("inc_mort/output/checks/method_mort.pdf"),width = 14,height = 8)
# 
# 
# 
# 


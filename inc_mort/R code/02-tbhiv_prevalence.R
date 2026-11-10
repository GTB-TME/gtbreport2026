#' ---
#' title: HIV prevalence in TB
#' author: Mathieu Bastard
#' date: 28/05/2026
#' ---


### 2026 Edits

# Adapt code with the new data system (WIDP)
# For this year, we used to entire previous time series from last year report 2000-2024
# and update only the year 2025 with the new data as no updates from previous years were made


#'
#' # Prevalence of HIV among TB cases
#'
#' This script estimates the prevalence of HIV among TB cases using the notifications data
#' and non-routine surveillance data (HIV surveys among TB and HIV sentinel surveillance with TB
#' defining a sentinel group) reported by countries. 
#' 
#' The historical data on TBHIV suffers from severe quality issues (implausible values, missing data), 
#' it should be consolidated in a manner similar to the DR data, through a process of documenting 
#' and rejecting all implausible and outlier values (e.g. ZAF 2020 prev of HIV in routinely tested 
#' suddenly increased from less than 60% to over 70% while testing coverage dropped from over 
#' 80% to around 60% due to covid disruptions -- that prev value should be rejected (see around 
#' line 433) and an estimate imputed through interpolation). Many data entry errors are c
#' orrected/rejecte-d in this script, an error-prone and unsatisfactory approach to 
#' consolidating the data. Most outliers and implausible values are spotted using graphic 
#' checks of the empirical data rather than though a systematic statistical approach 
#' applied to time-series.
#' 
#' Priority is given to the survey data (though very few country-year 
#' datapoints are reported overall) particularly where testing coverage is low (e.g. early KHM reports), 
#' but there are a number of routine testing results suspected to be reported as 
#' survey results possibly due to misunderstandings of relevant questions in the data collection
#' form. A threshold of 60% routine testing coverage is used (see around line 119). 
#' When both routine (coverage > threshold) and sentinel data are available, country-year points 
#' are combined in a statistical ensemble. Missing values surrounded (in country time-series) with 
#' valid values are interpolated. In previous years, missing tbhiv were indirectly imputed using
#' a predictive model. This year, the output from previous years is reused.
#' 
#' *In future exercises, this script should be reworked from scratch instead of reused/adapted, 
#' following a careful and documented consolidation of the empirical data in a process similar to the 
#' one used on the DR data.*
#'
#'
#' Dependences: 
#'  ~/data: tb.rda (notifications), sty.rda (non routine TBHIV data), 
#'  
#'  ~/R/fun.R utility functions
#'  
#'
#' Output:
#' - tbhiv  (both a dated csv file and an rda file that may rewrite a previous version)
#' - sets of graphical checks in ~/output/checks
#' 




rm(list=ls())


# Load libraries and data
#

suppressMessages(library(data.table))
suppressMessages(library(here))
suppressMessages(library(zoo))
suppressMessages(library(dplyr))

library(forecast)

#Run Functions script
source(here('inc_mort/R code/fun.R'))

#Function to import db from GTB databases
source(here("import/load_gtb.R"))


#Load notification and non-routine TB/HIV data
tbhiv <- load_gtb("tbhiv",convert_dots = FALSE)
setDT(tbhiv)
tb <- load_gtb("tb",convert_dots = FALSE)
setDT(tb)
load(here("data/gtb/other/pop.rda"))
setDT(pop)

#Estimates year
yr=2025

#Load previous year estimate data

load(here('inc_mort/estimates2025/est.rda'))
oest=copy(est)
rm(est)

# Load skeleton dataset for 2025 estimates
load(here("inc_mort/analysis/est.rda"))


#Merge previous tbhiv estimate and time series (2000:2024) to 2025 dataset

est <- merge(est,oest[,.(iso3,year,tbhiv,tbhiv.sd)],
             by=c("iso3","year"),
             all.x=TRUE)


#Merge HIV testing routine for 2025
est <- merge(est,tbhiv[year==2025,.(iso3,year,newrel_hivtest,newrel_hivpos)],
             by=c("iso3","year"),
             all.x=TRUE)

#Calculate HIV prevalence in TB for 2025 and complete the time series in tbhiv
est[year==2025, tbhiv := newrel_hivpos / newrel_hivtest]


#Estimate 95%UI for tbhiv using binomial distribution

#New total new episode for 2025, and impute it if missing
#Total new episode calculation

new_epis <- tb %>%
  mutate(
    c_newinc = case_when(
      # For years before 2013: if all are missing, return NA, otherwise sum with zeros
      year < 2013 & if_all(c(tot_newrel, newret_oth), is.na) ~ NA_real_,
      year < 2013 ~ coalesce(tot_newrel, 0) + 
        coalesce(newret_oth, 0),
      
      # For years 2013 to 2024: if all are missing, return NA, otherwise sum with zeros
      year >= 2013 & year <= 2024 & if_all(c(new_labconf, new_clindx, new_ep, 
                                             ret_rel_labconf, ret_rel_clindx, ret_rel_ep), is.na) ~ NA_real_,
      year >= 2013 & year <= 2024 ~ coalesce(new_labconf, 0) + 
        coalesce(new_clindx, 0) + 
        coalesce(new_ep, 0) + 
        coalesce(ret_rel_labconf, 0) + 
        coalesce(ret_rel_clindx, 0) + 
        coalesce(ret_rel_ep, 0),
      
      # For years 2025 onwards: check if ALL variables are missing
      year >= 2025 & if_all(c(pulm_labconf_new, pulm_labconf_rec, pulm_labconf_unk, 
                              pulm_clindx_new, pulm_clindx_rec, pulm_clindx_unk, 
                              ep_newinc), is.na) ~ NA_real_,
      
      # If at least one value exists, calculate the sum
      year >= 2025 ~ coalesce(pulm_labconf_new, 0) + 
        coalesce(pulm_labconf_rec, 0) + 
        coalesce(pulm_labconf_unk, 0) + 
        coalesce(pulm_clindx_new, 0) + 
        coalesce(pulm_clindx_rec, 0) + 
        coalesce(pulm_clindx_unk, 0) + 
        coalesce(ep_newinc, 0),
      
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(iso3, year, c_newinc) %>%
  filter(year > 1999)

# Merge to main dataset
est <- merge(est,new_epis,by=c("iso3","year"),all.x=TRUE)


### TEMPORARY, MRT NA in 2025 waiting for more discussions 

est[iso3=="MRT" & year == 2025, c_newinc:=NA]




# Calculate notification rate, merge pop
est <- merge(est,pop[,.(iso3,year,pop=e_pop_num)],by=c("iso3","year"),all.x=TRUE)
est[,newinc:=c_newinc*1e5/pop]

# Missing new episodes of TB in 2025
est[year == 2025, sum(is.na(c_newinc))]
est[year == 2025 & is.na(c_newinc), .(country)]

#Countries with 0 notifications in a year
zero_notif_records <- est[c_newinc == 0, .(iso3, country, year)]
countries_with_zeros <- unique(zero_notif_records$country)
cat("Number of countries with at least one year of zero notifications:", length(countries_with_zeros), "\n")
print(countries_with_zeros)

#TLS looks weird, replace by NA and then impute next steps
est[iso3 == "TLS" & c_newinc == 0, c_newinc := NA]
est[iso3 == "TLS" & newinc == 0, newinc := NA]
est[iso3 == "USA" & c_newinc == 0 & year==2024, c_newinc := NA]
est[iso3 == "USA" & newinc == 0 & year==2024, newinc := NA]
est[iso3 == "TUN" & c_newinc == 0 & year==2024, c_newinc := NA]
est[iso3 == "TUN" & newinc == 0 & year==2024, newinc := NA]


# Impute missing year using time series ARIMA (New)

impute_hybrid_arima <- function(sub_df, key) {
  
  # Initialize the new variable with the original data
  sub_df$imp.newinc <- sub_df$newinc
  
  # If there are no missing values at all, return early
  if (all(!is.na(sub_df$imp.newinc))) return(sub_df)
  
  # LINEAR INTERPOLATION (for gaps between observed years)
  sub_df$imp.newinc <- zoo::na.approx(sub_df$imp.newinc, na.rm = FALSE)
  
  # ARIMA FORECASTING (only for remaining trailing missing years)
  missing_indices <- which(is.na(sub_df$imp.newinc))
  
  # If linear interpolation cleared all NAs, we are done
  if (length(missing_indices) == 0) return(sub_df)
  
  # Extract the non-missing history up to the trailing missing years
  history <- sub_df$imp.newinc[-missing_indices]
  
  # Ensure we have enough data points (minimum 3) to fit ARIMA
  if (sum(!is.na(history)) > 3) {
    ts_history <- ts(history, start = min(sub_df$year))
    
    # Fit ARIMA on the history (which now includes the interpolated mid-gaps)
    fit <- auto.arima(ts_history,
                      max.p = 1, 
                      max.q = 1, 
                      d = 1, 
                      allowdrift = TRUE, 
                      stepwise = FALSE)
    
    # Forecast ahead for the number of remaining missing trailing years
    fc <- forecast(fit, h = length(missing_indices))
    
    # Insert the ARIMA forecast values into the trailing NA slots
    sub_df$imp.newinc[missing_indices] <- as.numeric(fc$mean)
  } else {
    # Fallback if history is too sparse to model
    sub_df$imp.newinc[missing_indices] <- NA_real_
  }
  
  return(sub_df)
}

# Apply the hybrid function to your 'est' dataset
est <- est %>%
  group_by(iso3) %>%
  arrange(year) %>% 
  group_modify(~ impute_hybrid_arima(.x, .y)) %>%
  ungroup()

setDT(est)

#Calculate imputed c_newinc
est[,imp_c_newinc:=round(imp.newinc*pop/1e5,0)]


est[is.na(tbhiv.sd), tbhiv.sd := {
  
  if (!is.na(tbhiv) & !is.na(imp_c_newinc) & imp_c_newinc > 0) {
    
    # Run the exact binomial test using imputed case counts
    tmp <- binom.test(round(tbhiv * imp_c_newinc), imp_c_newinc)$conf.int
    
    # Calculate SD from the confidence interval width
    (tmp[2] - tmp[1]) / 3.92
    
  } else {
    NA_real_
  }
}, by = .(iso3, year)]




#Threshhold of 60%, if coverage of HIV testing <60%, do not use it
threshold=0.6

est[,hiv_coverage:=newrel_hivtest/c_newinc]
est[year==yr,tbhiv.routine.ok:=ifelse(hiv_coverage>=threshold, TRUE,FALSE)]

est[year==yr & tbhiv.routine.ok==FALSE,tbhiv:=NA]
est[year==yr & tbhiv.routine.ok==FALSE,tbhiv.sd:=NA]



#Impute the missing tbhiv with the same logic: linear interpolation or ARIMA
impute_hybrid_arima_tbhiv <- function(sub_df, key) {
  
  # Initialize the new variable with the original data
  sub_df$imp.tbhiv <- sub_df$tbhiv
  
  # If there are no missing values at all, return early
  if (all(!is.na(sub_df$imp.tbhiv))) return(sub_df)
  
  # LINEAR INTERPOLATION (for gaps between observed years)
  sub_df$imp.tbhiv <- zoo::na.approx(sub_df$imp.tbhiv, na.rm = FALSE)
  
  # ARIMA FORECASTING (only for remaining trailing missing years)
  missing_indices <- which(is.na(sub_df$imp.tbhiv))
  
  # If linear interpolation cleared all NAs, we are done
  if (length(missing_indices) == 0) return(sub_df)
  
  # Extract the non-missing history up to the trailing missing years
  history <- sub_df$imp.tbhiv[-missing_indices]
  
  # Ensure we have enough data points (minimum 3) to fit ARIMA
  if (sum(!is.na(history)) > 3) {
    ts_history <- ts(history, start = min(sub_df$year))
    
    # Fit ARIMA on the history (which now includes the interpolated mid-gaps)
    fit <- auto.arima(ts_history,
                      max.p = 1, 
                      max.q = 1, 
                      d = 1, 
                      allowdrift = TRUE, 
                      stepwise = FALSE)
    
    # Forecast ahead for the number of remaining missing trailing years
    fc <- forecast(fit, h = length(missing_indices))
    
    # Insert the ARIMA forecast values into the trailing NA slots
    sub_df$imp.tbhiv[missing_indices] <- as.numeric(fc$mean)
  } else {
    # Fallback if history is too sparse to model
    sub_df$imp.tbhiv[missing_indices] <- NA_real_
  }
  
  return(sub_df)
}

# Apply the hybrid function to 'est' dataset
est <- est %>%
  group_by(iso3) %>%
  arrange(year) %>% 
  group_modify(~ impute_hybrid_arima_tbhiv(.x, .y)) %>%
  ungroup()

setDT(est)
est[iso3=="BGD", .(iso3,year,c_newinc,tbhiv,hiv_coverage,imp.tbhiv )]

# Global prevalence of HIV among TB

(est[, .(tbhiv = weighted.mean(imp.tbhiv,
                                 w = imp_c_newinc,
                                 na.rm = TRUE)), by =
         year])



est[tbhiv<0 ,tbhiv:=0]
est[imp.tbhiv<0 ,imp.tbhiv:=0]

# Check plot of time series
#
wr <- unique(as.character(est$g.whoregion))

for (i in wr) {
  p <-
    qplot(
      year,
      0,
      data = subset(est, g.whoregion == i & year>=2010),
      geom = 'line',
      colour = I('grey90')
    ) +
    geom_point(aes(year, imp.tbhiv),
               colour = I('blue'),
               shape = I(1)) +
    geom_point(aes(year, tbhiv),
               colour = I('red'),
               shape = I(1)) +    
    geom_line(aes(year, imp.tbhiv), colour = I('blue')) +
    facet_wrap( ~ iso3, scales = 'free_y') + xlab('') +
    scale_x_continuous(breaks=c(2010,2015,2020,yr)) +
    ylab('HIV prevalence in TB') +
    theme_bw(base_size = 14)
  suppressWarnings(print(p))
  suppressWarnings(ggsave(here(
    paste('inc_mort/analysis/graphs/tbhiv/tbhiv_', i, '.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}




# save
#
attr(est, "timestamp") <- Sys.Date() #set date
save(est, file = here('inc_mort/analysis/est.rda'))

#fwrite(tbhiv, file = here(paste0('inc_mort/analysis/csv/tbhiv_', Sys.Date(), '.csv')))




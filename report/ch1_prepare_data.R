#' ---
#' title: Data preparation, Global TB Report 2025, webpages 1.1 and 1.2
#' author: Mathieu Bastard
#' date: 01/07/2025



rm(list=ls())

library(data.table)
library(here)
library(gtbreport)
library(plotly)
library(dplyr)
library(tidyverse)
library(tidyr)

#Run Functions script
source(here('inc_mort/R code/fun.R'))
source(here("report/functions/output_ggplot.R"))

#Function to import db from GTB databases
source(here("import/load_gtb.R"))

#Load last year estimates data
load(here('inc_mort/estimates2025/est.rda'))
old=copy(est)
rm(est)

global25=load(here('inc_mort/estimates2025/global.rda'))
globalold=global

#Load current estimates
load(here('inc_mort/analysis/est.rda'))
est.full <- copy(est)
load(here('inc_mort/analysis/hbc.rda'))
load(here('inc_mort/analysis/regional.rda'))
load(here('inc_mort/analysis/hbc.rda'))
load(here('inc_mort/analysis/unaids.rda'))
load(here('inc_mort/analysis/global.rda'))
load(here("inc_mort/analysis/estimates_income.rda"))
load(here('inc_mort/analysis/vr.rda'))

est[,c.newinc:=c_newinc]



# temp fix to remove

est[is.na(mort.h.lo.num), mort.h.lo.num := mort.h.num-1.96*0.2*mort.h.num]
est[is.na(mort.h.hi.num), mort.h.hi.num := mort.h.num+1.96*0.2*mort.h.num]

hbc[is.na(mort.h.lo), mort.h.lo := mort.h-1.96*0.2*mort.h]
hbc[is.na(mort.h.hi), mort.h.hi := mort.h+1.96*0.2*mort.h]


#GTB data

tb=load_gtb("tb",convert_dashes = TRUE)
cty=load_gtb("cty",convert_dashes = TRUE)
pop=load_gtb("pop",convert_dashes = TRUE)
grpmbr=load_gtb("grpmbr",convert_dashes = TRUE)


setDT(tb)

#Load disaggregated estimates
#load(here('disaggregation/output/h30splt.rda'))
# load(here('disaggregation/output/globsplt.rda'))
# load(here('disaggregation/output/regsplt.rda'))
# load(here('disaggregation/output/Mglobsplt.rda'))
# load(here('disaggregation/output/MPglobsplit.rda'))
# load(here('disaggregation/output/Mregsplt.rda'))

load(here("disaggregation/output/db_estimates_country_all.Rdata"))
load(here("disaggregation/output/db_estimates_group_all.Rdata"))
load(here("disaggregation/output/db_hn_mortality_country_all.Rdata"))
load(here("disaggregation/output/db_hn_mortality_group_all.Rdata"))
load(here("disaggregation/output/db_hp_mortality_country_all.Rdata"))
load(here("disaggregation/output/db_hp_mortality_group_all.Rdata"))

#Load RRTB estimates
load(here('drtb/output/db_dr_group.rda'))


#Load methods
load(here("inc_mort/analysis/est.method.rda"))


#top10 <- fread(here('data/mortality/top10.csv'))
top2023 <- fread(here('data/mortality/Top15COD_GHE2023.csv'))



#Year estimate
yr <- 2025
vlohi <- Vectorize(lohi, c('ev', 'sd'))

snapshot_date <- latest_snapshot_date()
est_date <- attr(est, which = "timestamp")

# Function for writing thousands

thous=function(x){
  return(paste0(x," ","000"))
}




# region names
regional$region <- factor(regional$g.whoregion, levels=c('AFR','AMR','SEA','EUR','EMR','WPR'))
levels(regional$region) <-
  c(
    'African Region',
    'Region of the Americas',
    'South-East Asia Region',
    'European Region',
    'Eastern Mediterranean Region',
    'Western Pacific Region'
  )
regional$region <- ordered(regional$region, )


# income names
# region names
income$group <- factor(income$g.income, levels=c('HIC','LIC','LMC','UMC'))
levels(income$group) <-
  c(
    'High-income countries',
    'Low-income countries',
    'Lower-middle-income countries',
    'Upper-middle-income countries'
  )
income$group <- ordered(income$group, )


# regional inc and mort milestones
# These are from the End TB Strategy
# Update these as and when needed
#
# For the 2025 report we show the 2025 milestones, which are:
#
# - a 50% drop in incidence per 100,000 population compared to 2015
# - a 75% drop in total TB deaths (HIV-negative + HIV-positive) compared to 2015

inc.mort.milestone.yr <- 2025
inc.milestone.vs.2015 <- 0.5     # a 50% drop compared to 2015
mort.milestone.vs.2015 <- 0.25   # a 75% drop compared to 2015



# Calculate the latest milestone values and add them to the aggregated datasets

global[, inc.milestone := inc[year==2015] * inc.milestone.vs.2015]
global[, mort.milestone := mort.num[year==2015] * mort.milestone.vs.2015]

regional[, inc.milestone := inc[year==2015] * inc.milestone.vs.2015, by = g.whoregion]
regional[, mort.milestone := mort.num[year==2015] * mort.milestone.vs.2015, by = g.whoregion]

income[, mort.milestone := mort.num[year==2015] * mort.milestone.vs.2015, by = group]



# output for manual manipulation to incorporate into other's files
#
# Table EA 5.1 - Sources of data (Marie)
#
# tab <-
#   as.data.frame(est[year == yr][g.hbc==TRUE, list(iso3, source.mort)])
# (tab)
# vr <- merge(vr, est[year==yr, .(iso3, g.hbc)], by='iso3')
# (vr[g.hbc==T & !is.na(tb),.(min.year = min(year), max.year = max(year)), by='iso3'])
# 
# (est[g.hbc==T & !is.na(vr.raw),.(min.year = min(year), max.year = max(year)), by='iso3'])
# # (est[g.hbc==T & year==yr & old.source.mort %in% c('VR','IHME'), .(iso3, source.mort)])


# Country name to est dataset
#est=merge(est,subset(cty,select=c("iso3","country")),by="iso3",all.x=T)


# Identify the top countries accounting for most incident cases, to be used in the text of 1.1
# Weirdly couldn't just refer to global[global$year==yr, "inc.num"] , had to coerce to a standalone integer
global_inc <- as.integer(global[global$year==yr, "inc.num"])

f1.1.2_top  <- est |>
  dplyr::filter(year == yr) |> 
  dplyr::arrange(desc(inc.num)) |>
  dplyr::inner_join(cty, by="iso3") |>
  dplyr::select(iso3, country.in.text.EN, size = inc.num) |> 
  # Calculate proportion of global incidence
  dplyr::mutate(pct = size * 100 / global_inc ) |>
  dplyr::mutate(pct_cumsum = cumsum(pct)) |>
  # pick the countries accounting for more than two thirds of global burden
  dplyr::filter(pct_cumsum < 68) |>
  dplyr::select(country.in.text.EN, pct)



# chapter 2 tables


# Estimated epidemiological burden of TB in 2024 for 30 high TB burden countries,
# WHO regions and globally.
tab1.1 <- subset(
  est,
  g.hbc == TRUE & year == yr,
  select = c(
    "country",
    "pop",
    "inc.num",
    "inc.lo.num",
    "inc.hi.num",
    "inc.h.num",
    "inc.h.lo.num",
    "inc.h.hi.num",
    "mort.nh.num",
    "mort.nh.lo.num",
    "mort.nh.hi.num",
    "mort.h.num",
    "mort.h.lo.num",
    "mort.h.hi.num"
  )
)

tab1.2 <- subset(
  hbc,
  year == yr,
  select = c(
    "pop",
    "inc.num",
    "inc.lo.num",
    "inc.hi.num",
    "inc.h.num",
    "inc.h.lo.num",
    "inc.h.hi.num",
    "mort.nh.num",
    "mort.nh.lo.num",
    "mort.nh.hi.num",
    "mort.h.num",
    "mort.h.lo.num",
    "mort.h.hi.num"
  )
)

tab1.3 <- subset(
  regional,
  year == yr,
  select = c(
    "g.whoregion",
    "pop",
    "inc.num",
    "inc.lo.num",
    "inc.hi.num",
    "inc.h.num",
    "inc.h.lo.num",
    "inc.h.hi.num",
    "mort.nh.num",
    "mort.nh.lo.num",
    "mort.nh.hi.num",
    "mort.h.num",
    "mort.h.lo.num",
    "mort.h.hi.num"
  )
)

tab1.4 <- subset(
  global,
  year == yr,
  select = c(
    "pop",
    "inc.num",
    "inc.lo.num",
    "inc.hi.num",
    "inc.h.num",
    "inc.h.lo.num",
    "inc.h.hi.num",
    "mort.nh.num",
    "mort.nh.lo.num",
    "mort.nh.hi.num",
    "mort.h.num",
    "mort.h.lo.num",
    "mort.h.hi.num"
  )
)


setnames(tab1.1, "country", "rowname")
setnames(tab1.3, "g.whoregion", "rowname")
tab1.1 <- tab1.1[order(rowname)]
tab1.3 <- tab1.3[order(rowname)]
tab1.2 <- cbind(rowname = "High TB burden countries", tab1.2)
tab1.4 <- cbind(rowname = "Global", tab1.4)

tab1 <- rbind(tab1.1, tab1.2, tab1.3, tab1.4)
tab1[, pop := round(pop)]
tab1[, 2:14 := lapply(.SD, function(x)
  x / 1000), .SDcols = 2:14]
tab1[, 2:14 := lapply(.SD, gtbreport::ftb), .SDcols = 2:14]

names(tab1) <- c(
  " ",
  "Population",
  "Total TB Incidence",
  " ",
  " ",
  "HIV-positive TB incidence",
  " ",
  " ",
  "HIV-negative TB mortality",
  " ",
  " ",
  "HIV-positive TB mortality",
  " ",
  " "
)

tab1[32:37, 1] <- c(
  'African Region',
  'Region of the Americas',
  'Eastern Mediterranean Region',
  'European Region',
  'South-East Asia Region',
  'Western Pacific Region'
)

(tabinc <- copy(tab1[c(1:33,36,35,34,37,38), ]))




# Epi burden (rates) for HBC

#temporary fix to be removed
# hbc[is.na(mort.h.lo), mort.h.lo := mort.h-1.96*0.2*mort.h]
# hbc[is.na(mort.h.hi), mort.h.hi := mort.h+1.96*0.2*mort.h]



tab1b.1 <- subset(
  est,
  g.hbc == TRUE & year == yr,
  select = c(
    "country",
    "pop",
    "inc",
    "inc.lo",
    "inc.hi",
    "inc.h",
    "inc.h.lo",
    "inc.h.hi",
    "mort.nh",
    "mort.nh.lo",
    "mort.nh.hi",
    "mort.h",
    "mort.h.lo",
    "mort.h.hi"
  )
)

tab1b.2 <- subset(
  hbc,
  year == yr,
  select = c(
    "pop",
    "inc",
    "inc.lo",
    "inc.hi",
    "inc.h",
    "inc.h.lo",
    "inc.h.hi",
    "mort.nh",
    "mort.nh.lo",
    "mort.nh.hi",
    "mort.h",
    "mort.h.lo",
    "mort.h.hi"
  )
)

tab1b.3 <- subset(
  regional,
  year == yr,
  select = c(
    "g.whoregion",
    "pop",
    "inc",
    "inc.lo",
    "inc.hi",
    "inc.h",
    "inc.h.lo",
    "inc.h.hi",
    "mort.nh",
    "mort.nh.lo",
    "mort.nh.hi",
    "mort.h",
    "mort.h.lo",
    "mort.h.hi"
  )
)


tab1b.4 <- subset(
  global,
  year == yr,
  select = c(
    "pop",
    "inc",
    "inc.lo",
    "inc.hi",
    "inc.h",
    "inc.h.lo",
    "inc.h.hi",
    "mort.nh",
    "mort.nh.lo",
    "mort.nh.hi",
    "mort.h",
    "mort.h.lo",
    "mort.h.hi"
  )
)


setnames(tab1b.1, "country", "rowname")
setnames(tab1b.3, "g.whoregion", "rowname")
tab1b.1 <- tab1b.1[order(rowname)]
tab1b.3 <- tab1b.3[order(rowname)]
tab1b.2 <- cbind(rowname = "High TB burden countries", tab1b.2)
tab1b.4 <- cbind(rowname = "Global", tab1b.4)

tab1b <- rbind(tab1b.1, tab1b.2, tab1b.3, tab1b.4)

tab1b[, 6:8 := lapply(.SD, function(x)
  x), .SDcols = 6:8]
tab1b[, 3:14 := lapply(.SD, gtbreport::ftb), .SDcols = 3:14]
tab1b[, pop := NULL]
names(tab1b) <- c(
  " ",
  "Incidence",
  " ",
  " ",
  "HIV Incidence",
  " ",
  " ",
  "HIV-negative TB mortality",
  " ",
  " ",
  "HIV-positive TB mortality",
  " ",
  " "
)

tab1b[32:37, 1] <- c(
  'African Region',
  'Region of the Americas',
  'Eastern Mediterranean Region',
  'European Region',
  'South-East Asia Region',
  'Western Pacific Region'
)

(tabmort <- copy(tab1b[c(1:33,36,35,34,37,38), ]))



# HBCs
#
hest <- subset(est, g.hbc == TRUE)

levels(hest$country)[match('Democratic Republic of the Congo', levels(hest$country))] <-
  'Democratic Republic\nof the Congo'
levels(hest$country)[match('United Republic of Tanzania', levels(hest$country))] <-
  'United Republic\nof Tanzania'
levels(hest$country)[match("Democratic People's Republic of Korea", levels(hest$country))] <-
  "Democratic People's\nRepublic of Korea"


# Calculate the latest milestone values and add them to the country datasets

hest[, inc.milestone := inc[year==2015] * inc.milestone.vs.2015, by = iso3]
hest[, mort.milestone := mort.num[year==2015] * mort.milestone.vs.2015, by = iso3]

est[, inc.milestone := inc[year==2015] * inc.milestone.vs.2015, by = iso3]
est[, mort.milestone := mort.num[year==2015] * mort.milestone.vs.2015, by = iso3]

hest[, country2 := country]
hest[grep('Republic', country2), country2 := paste('the',country2)]




# Confirmed
#
# 
# tb[, wconf := rowSums(cbind(new.labconf, ret.rel.labconf, new.clindx, ret.rel.clindx),
#                       na.rm = TRUE)]
# dta <-
#   merge(tb[year > 1999, .(iso3, g.whoregion, year, newinc, conf, wconf, ep, sex.ratio)],
#         est[, .(iso3, g.hbc, g.income, year, inc, tbhiv)], by = c('iso3', 'year'))
# 
# sel <- dta$wconf >= 100 & !is.na(dta$wconf) & dta$year > 2012
# 
# (dta[sel, weighted.mean(conf,
#                         w = wconf,
#                         na.rm = TRUE)
#      , by = .(year)][order(year)])
# 
# (dta[sel, {
#   tmp = weighted.quantile(
#     conf,
#     probs = c(0, 0.25, 0.5, 0.75, 1),
#     w = wconf,
#     na.rm = TRUE
#   )
# 
#   list(
#     conf.med = tmp[3],
#     conf.min = tmp[1],
#     conf.q1 = tmp[2],
#     conf.q3 = tmp[4],
#     conf.max = tmp[5]
#   )
# 
# }, by = .(g.income == 'HIC', year)][order(year)])
# 
# (conf <- dta[sel, {
#   tmp = weighted.quantile(
#     conf,
#     probs = c(0, 0.25, 0.5, 0.75, 1),
#     w = wconf,
#     na.rm = TRUE
#   )
# 
#   list(
#     conf.med = tmp[3],
#     conf.min = tmp[1],
#     conf.q1 = tmp[2],
#     conf.q3 = tmp[4],
#     conf.max = tmp[5]
#   )
# 
# }, by = .(g.income, year)][order(year)])
# 
# conf[, income := factor(
#   g.income,
#   levels = c('LIC', 'LMC', 'UMC', 'HIC'),
#   labels = c(
#     'low-income',
#     'lower-middle-income',
#     'upper-middle-income',
#     'high-income'
#   )
# )]
# 



### Re calculate some of the age sex variables depending on disaggregation level

tb <- tb %>%
  mutate(
    # Ensure source columns are treated as 0 if they are NA
    m59 = coalesce(newrel.m59, 0),
    m1014 = coalesce(newrel.m1014, 0),
    m1519 = coalesce(newrel.m1519, 0),
    m2024 = coalesce(newrel.m2024, 0),
    
    # Replace newrel.m514 with (m59 + m1014) if m514 is missing (NA), else keep m514
    newrel.m514 = if_else(is.na(newrel.m514), m59 + m1014, newrel.m514),
    newrel.m1524 = if_else(is.na(newrel.m1524), m1519 + m2024, newrel.m1524),
  
  ) %>%
  # Optional: drop temporary columns if you don't need them
  select(-m59, -m1014,-m1519,-m2024)

tb <- tb %>%
  mutate(
    # Ensure source columns are treated as 0 if they are NA
    f59 = coalesce(newrel.f59, 0),
    f1014 = coalesce(newrel.f1014, 0),
    f1519 = coalesce(newrel.f1519, 0),
    f2024 = coalesce(newrel.f2024, 0),
    
    # Replace newrel.f514 with (f59 + f1014) if f514 is fissing (NA), else keep f514
    newrel.f514 = if_else(is.na(newrel.f514), f59 + f1014, newrel.f514),
    newrel.f1524 = if_else(is.na(newrel.f1524), f1519 + f2024, newrel.f1524),
    
  ) %>%
  # Optional: drop tefporary colufns if you don't need thef
  select(-f59, -f1014,-f1519,-f2024)

### 2025, Using the new outputs from Pete's disaggregation

#incidence

#time series H
inc.d.h=db_estimates_group_all[group_type=="global" & sex=="m" & age_group %ni% c("0-14","5-9","10-14","15-19","20-24","15plus","a")
                               & year > 2009,]

#time series F
inc.d.f=db_estimates_group_all[group_type=="global" & sex=="f" & age_group %ni% c("0-14","5-9","10-14","15-19","20-24","15plus","a")
                               & year > 2009,]


#age pyramid as before at year yr
#recreate globsplt dataset

globsplt=db_estimates_group_all[year==yr & group_type=="global" & (sex=="m" | sex=="f") & age_group %ni% c("0-14","5-9","10-14","15-19","20-24","15plus","a")
                                & year > 2009,]

globsplt[, child := age_group %in% c('0-4','5-14')]
globsplt[sex=="m", sex:="M"]
globsplt[sex=="f", sex:="F"]

globsplt <- globsplt %>% 
  rename(inc = best)

notif.m=tb[year==2024,.(iso3,year,
                      newrel.m04,newrel.m514,newrel.m1524,newrel.m2534,
                      newrel.m3544,newrel.m4554,newrel.m5564,newrel.m65)]

newrel.m <- colSums(notif.m[, c("newrel.m04", "newrel.m514",
                                           "newrel.m1524", "newrel.m2534",
                                           "newrel.m3544", "newrel.m4554", "newrel.m5564",
                                           "newrel.m65")], na.rm = T)

newrel.m <- data.frame(
  age_group = c("0-4","5-14","15-24","25-34","35-44","45-54","55-64","65plus"), # Extracts the names (age groups)
  newrel = as.numeric(newrel.m),
  sex="M"
)

notif.f=tb[year==2024,.(iso3,year,
                        newrel.f04,newrel.f514,newrel.f1524,newrel.f2534,
                        newrel.f3544,newrel.f4554,newrel.f5564,newrel.f65)]


newrel.f <- colSums(notif.f[, c("newrel.f04", "newrel.f514",
                                "newrel.f1524", "newrel.f2534",
                                "newrel.f3544", "newrel.f4554", "newrel.f5564",
                                "newrel.f65")], na.rm = T)

newrel.f <- data.frame(
  age_group = c("0-4","5-14","15-24","25-34","35-44","45-54","55-64","65plus"), # Extracts the names (age groups)
  newrel = as.numeric(newrel.f),
  sex="F"
)

newrel.all=rbind(newrel.m,newrel.f)
newrel.all.glob<-copy(newrel.all)


globsplt=merge(globsplt,newrel.all,by=c("sex","age_group"),all.x=T)

globsplt=globsplt[,.(sex,age=age_group,inc,newrel,pop=1)]
factor(globsplt$age,levels=c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65plus"),
                    labels=c("0\u20134", "5\u201314", "15\u201324", "25\u201334", "35\u201344", "45\u201354", "55\u201364", "\u226565"))

### recreating regsplt, regional split
regsplt=db_estimates_group_all[year==yr & group_type=="g_whoregion" & (sex=="m" | sex=="f") & age_group %ni% c("0-14","5-9","10-14","15-19","20-24","15plus","a")
                               & year > 2009,]

regsplt[, child := age_group %in% c('0-4',"5-14")]
regsplt[sex=="m", sex:="M"]
regsplt[sex=="f", sex:="F"]

regsplt <- regsplt %>% 
  rename(inc = best)

  
notif.m=tb[year==2024,.(iso3,year,
                        newrel.m04,newrel.m514,newrel.m1524,newrel.m2534,
                        newrel.m3544,newrel.m4554,newrel.m5564,newrel.m65)]

notif.m=merge(notif.m,cty[,.(iso3,g.whoregion)],by="iso3",all.x=T)


notif.m.grp <- notif.m %>%
  group_by(g.whoregion) %>%
  summarise(
    newrel.m04 = sum(newrel.m04, na.rm = TRUE),
    newrel.m514 = sum(newrel.m514, na.rm = TRUE),
    newrel.m1524 = sum(newrel.m1524, na.rm = TRUE),
    newrel.m2534 = sum(newrel.m2534, na.rm = TRUE),
    newrel.m3544 = sum(newrel.m3544, na.rm = TRUE),
    newrel.m4554 = sum(newrel.m4554, na.rm = TRUE),
    newrel.m5564 = sum(newrel.m5564, na.rm = TRUE),
    newrel.m65 = sum(newrel.m65, na.rm = TRUE),
    v.groups = 'drop' # Recommended to drop grouping after summarising
  )

# Convert to long format
notif.m.grp.long <- notif.m.grp %>%
  pivot_longer(
    cols = starts_with("newrel.m"), # Selects all columns that start with "newrel.m"
    names_to = "age_group",         # Name of the new column that will store the original column names
    values_to = "newrel",            # Name of the new column that will store the values
    names_prefix = "newrel.m"       # Remove "newrel.m" prefix from the age_group column
  )

notif.m.grp.long <- notif.m.grp.long %>%
  mutate(
    age_group = case_when(
      age_group == "04" ~ "0-4",
      age_group == "514" ~ "5-14",
      age_group == "1524" ~ "15-24",
      age_group == "2534" ~ "25-34",
      age_group == "3544" ~ "35-44",
      age_group == "4554" ~ "45-54",
      age_group == "5564" ~ "55-64",
      age_group == "65" ~ "65plus",
      TRUE ~ age_group # Keep all other values as they are
    )
  )


notif.m.grp.long$v.groups<-NULL
notif.m.grp.long$sex="M"


notif.f=tb[year==2024,.(iso3,year,
                        newrel.f04,newrel.f514,newrel.f1524,newrel.f2534,
                        newrel.f3544,newrel.f4554,newrel.f5564,newrel.f65)]

notif.f=merge(notif.f,cty[,.(iso3,g.whoregion)],by="iso3",all.x=T)


notif.f.grp <- notif.f %>%
  group_by(g.whoregion) %>%
  summarise(
    newrel.f04 = sum(newrel.f04, na.rm = TRUE),
    newrel.f514 = sum(newrel.f514, na.rm = TRUE),
    newrel.f1524 = sum(newrel.f1524, na.rm = TRUE),
    newrel.f2534 = sum(newrel.f2534, na.rm = TRUE),
    newrel.f3544 = sum(newrel.f3544, na.rm = TRUE),
    newrel.f4554 = sum(newrel.f4554, na.rm = TRUE),
    newrel.f5564 = sum(newrel.f5564, na.rm = TRUE),
    newrel.f65 = sum(newrel.f65, na.rm = TRUE),
    v.groups = 'drop' # Recoffended to drop grouping after suffarising
  )

# Convert to long forfat
notif.f.grp.long <- notif.f.grp %>%
  pivot_longer(
    cols = starts_with("newrel.f"), # Selects all colufns that start with "newrel.f"
    names_to = "age_group",         # Nafe of the new colufn that will store the original colufn nafes
    values_to = "newrel",            # Nafe of the new colufn that will store the values
    names_prefix = "newrel.f"       # Refove "newrel.f" prefix frof the age_group colufn
  )

notif.f.grp.long <- notif.f.grp.long %>%
  mutate(
    age_group = case_when(
      age_group == "04" ~ "0-4",
      age_group == "514" ~ "5-14",
      age_group == "1524" ~ "15-24",
      age_group == "2534" ~ "25-34",
      age_group == "3544" ~ "35-44",
      age_group == "4554" ~ "45-54",
      age_group == "5564" ~ "55-64",
      age_group == "65" ~ "65plus",
      TRUE ~ age_group # Keep all other values as they are
    )
  )


notif.f.grp.long$v.groups<-NULL
notif.f.grp.long$sex="F"

newrel.all=rbind(notif.m.grp.long,notif.f.grp.long)
regsplt$g.whoregion=regsplt$group_name
regsplt=merge(regsplt,newrel.all,by=c("g.whoregion","sex","age_group"),all.x=T)


regsplt=regsplt[,.(g.whoregion,age=age_group,sex,inc,newrel)]



### Regions names

regsplt$name <- factor(regsplt$g.whoregion, levels=c('AFR','AMR','SEA','EUR','EMR','WPR'))

levels(regsplt$name) <-
  c(
    'African Region',
    'Region of the Americas',
    'South-East Asia Region',
    'European Region',
    'Eastern Mediterranean Region',
    'Western Pacific Region'
  )
regsplt$name <- ordered(regsplt$name)





### Recreate global and regional mortality split estimates from new Pete output
setDT(pop)
### HIV neg

mortsplit=db_hn_mortality_group_all[year==yr & group_type=="global" & (sex=="m" | sex=="f") & age_group %ni% c("0-14","5-9","10-14","15-19","20-24","15plus","a")
           & year > 2009,]
  
Mglobsplt=mortsplit[,.(sex,age=age_group,mort=best,mort.lo=lo,mort.hi=hi)]

Mglobsplt[, child := age %in% c('0-4',"5-14")]
Mglobsplt[sex=="m", sex:="M"]
Mglobsplt[sex=="f", sex:="F"]


mortsplitr=db_hn_mortality_group_all[year==yr & group_type=="g_whoregion" & (sex=="m" | sex=="f") & age_group %ni% c("0-14","5-9","10-14","15-19","20-24","15plus","a")
                                     & year > 2009,]


Mregsplt=mortsplitr[,.(g.whoregion=group_name,sex,age=age_group,mort=best,mort.lo=lo,mort.hi=hi)]



Mregsplt$name <- factor(Mregsplt$g.whoregion, levels=c('AFR','AMR','SEA','EUR','EMR','WPR'))
levels(Mregsplt$name) <-
  c(
    'African Region',
    'Region of the Americas',
    'South-East Asia Region',
    'European Region',
    'Eastern Mediterranean Region',
    'Western Pacific Region'
  )
Mregsplt$name <- ordered(Mregsplt$name)

Mregsplt[, child := age %in% c('0-4',"5-14")]
Mregsplt[sex=="m", sex:="M"]
Mregsplt[sex=="f", sex:="F"]


### HIV pos

mortsplith=db_hp_mortality_group_all[year==yr & group_type=="global" & (sex=="m" | sex=="f") & age_group %ni% c("0-14","5-9","10-14","15-19","20-24","15plus","a")
                                    & year > 2009,]

MPglobsplt=mortsplith[,.(sex,age=age_group,mort=best,mort.lo=lo,mort.hi=hi)]

MPglobsplt[, child := age %in% c('0-4',"5-14")]
MPglobsplt[sex=="m", sex:="M"]
MPglobsplt[sex=="f", sex:="F"]



mortsplitrh=db_hp_mortality_group_all[year==yr & group_type=="g_whoregion" & (sex=="m" | sex=="f") & age_group %ni% c("0-14","5-9","10-14","15-19","20-24","15plus","a")
                                     & year > 2009,]


MPregsplt=mortsplitrh[,.(g.whoregion=group_name,sex,age=age_group,mort=best,mort.lo=lo,mort.hi=hi)]



MPregsplt$name <- factor(MPregsplt$g.whoregion, levels=c('AFR','AMR','SEA','EUR','EMR','WPR'))
levels(MPregsplt$name) <-
  c(
    'African Region',
    'Region of the Americas',
    'South-East Asia Region',
    'European Region',
    'Eastern Mediterranean Region',
    'Western Pacific Region'
  )
MPregsplt$name <- ordered(MPregsplt$name)

MPregsplt[, child := age %in% c('0-4',"5-14")]
MPregsplt[sex=="m", sex:="M"]
MPregsplt[sex=="f", sex:="F"]






# Split Incidence
globspltbis=db_estimates_group_all[year==yr & group_type=="global" & (sex=="m" | sex=="f") & age_group %ni% c("0-14","5-9","10-14","15-19","20-24","15plus","a")
                                & year > 2009,]

globspltbis[, child := age_group %in% c('0-4','5-14')]
globspltbis[sex=="m", sex:="M"]
globspltbis[sex=="f", sex:="F"]

globspltbis <- globspltbis %>% 
  rename(inc = best)

inctot=global$inc.num[global$year==yr]
inctot.lo=global$inc.lo.num[global$year==yr]
inctot.hi=global$inc.hi.num[global$year==yr]

inc.men=sum(globspltbis$inc[globspltbis$sex=="M"&globspltbis$child==F])
inc.men.lo=sum(globspltbis$lo[globspltbis$sex=="M"&globspltbis$child==F])
inc.men.hi=sum(globspltbis$hi[globspltbis$sex=="M"&globspltbis$child==F])

inc.women=sum(globspltbis$inc[globspltbis$sex=="F"&globspltbis$child==F])
inc.women.lo=sum(globspltbis$lo[globspltbis$sex=="F"&globspltbis$child==F])
inc.women.hi=sum(globspltbis$hi[globspltbis$sex=="F"&globspltbis$child==F])

inc.child=sum(globspltbis$inc[globspltbis$child==T])
inc.child.lo=sum(globspltbis$lo[globspltbis$child==T])
inc.child.hi=sum(globspltbis$hi[globspltbis$child==T])



# Split mortality

#Global

death22=global$mort.nh.num[global$year==yr]

# mort.split=Mglobsplt %>% pivot_wider(names_from = sex, values_from = mort)
# mort.split$M=round(mort.split$M,1)
# mort.split$F=-round(mort.split$F,1)
# mort.split$i=seq(1,dim(mort.split)[1],1)
# mort.split=mort.split %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
# mort.split=mort.split %>% arrange(rev(i))


# Global HIV pos
death22.h=global$mort.h.num[global$year==yr]

# mort.split.h=MPglobsplit %>% pivot_wider(names_from = sex, values_from = deaths.pc)
# mort.split.h$M=round(mort.split.h$M/100*death22.h,1)
# mort.split.h$F=-round(mort.split.h$F/100*death22.h,1)
# mort.split.h$i=seq(1,dim(mort.split.h)[1],1)
# mort.split.h=mort.split.h %>% mutate(acat=ifelse(acat=="0-14","<15","\u226515"))
# mort.split.h=mort.split.h %>% arrange(rev(i))


# By region

deaths22_afr=regional$mort.nh.num[regional$year==yr & regional$g.whoregion=="AFR"]
deaths22_amr=regional$mort.nh.num[regional$year==yr & regional$g.whoregion=="AMR"]
deaths22_emr=regional$mort.nh.num[regional$year==yr & regional$g.whoregion=="EMR"]
deaths22_eur=regional$mort.nh.num[regional$year==yr & regional$g.whoregion=="EUR"]
deaths22_sea=regional$mort.nh.num[regional$year==yr & regional$g.whoregion=="SEA"]
deaths22_wpr=regional$mort.nh.num[regional$year==yr & regional$g.whoregion=="WPR"]


# mort.split.reg=Mregsplt %>% pivot_wider(names_from = sex, values_from = mort)
# 
# mort.split.reg$M[mort.split.reg$g.whoregion=="AFR"]=round(mort.split.reg$M[mort.split.reg$g.whoregion=="AFR"]*deaths22_afr,1)
# mort.split.reg$F[mort.split.reg$g.whoregion=="AFR"]=-round(mort.split.reg$F[mort.split.reg$g.whoregion=="AFR"]*deaths22_afr,1)
# 
# mort.split.reg$M[mort.split.reg$g.whoregion=="AMR"]=round(mort.split.reg$M[mort.split.reg$g.whoregion=="AMR"]*deaths22_amr,1)
# mort.split.reg$F[mort.split.reg$g.whoregion=="AMR"]=-round(mort.split.reg$F[mort.split.reg$g.whoregion=="AMR"]*deaths22_amr,1)
# 
# mort.split.reg$M[mort.split.reg$g.whoregion=="EMR"]=round(mort.split.reg$M[mort.split.reg$g.whoregion=="EMR"]*deaths22_emr,1)
# mort.split.reg$F[mort.split.reg$g.whoregion=="EMR"]=-round(mort.split.reg$F[mort.split.reg$g.whoregion=="EMR"]*deaths22_emr,1)
# 
# mort.split.reg$M[mort.split.reg$g.whoregion=="EUR"]=round(mort.split.reg$M[mort.split.reg$g.whoregion=="EUR"]*deaths22_eur,1)
# mort.split.reg$F[mort.split.reg$g.whoregion=="EUR"]=-round(mort.split.reg$F[mort.split.reg$g.whoregion=="EUR"]*deaths22_eur,1)
# 
# mort.split.reg$M[mort.split.reg$g.whoregion=="SEA"]=round(mort.split.reg$M[mort.split.reg$g.whoregion=="SEA"]*deaths22_sea,1)
# mort.split.reg$F[mort.split.reg$g.whoregion=="SEA"]=-round(mort.split.reg$F[mort.split.reg$g.whoregion=="SEA"]*deaths22_sea,1)
# 
# mort.split.reg$M[mort.split.reg$g.whoregion=="WPR"]=round(mort.split.reg$M[mort.split.reg$g.whoregion=="WPR"]*deaths22_wpr,1)
# mort.split.reg$F[mort.split.reg$g.whoregion=="WPR"]=-round(mort.split.reg$F[mort.split.reg$g.whoregion=="WPR"]*deaths22_wpr,1)
# 
# 
# mort.split.reg.afr=subset(mort.split.reg,g.whoregion=="AFR")
# mort.split.reg.afr$i=seq(1,dim(mort.split.reg.afr)[1],1)
# mort.split.reg.afr=mort.split.reg.afr %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
# mort.split.reg.afr=mort.split.reg.afr %>% arrange(rev(i))
# 
# mort.split.reg.amr=subset(mort.split.reg,g.whoregion=="AMR")
# mort.split.reg.amr$i=seq(1,dim(mort.split.reg.amr)[1],1)
# mort.split.reg.amr=mort.split.reg.amr %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
# mort.split.reg.amr=mort.split.reg.amr %>% arrange(rev(i))
# 
# mort.split.reg.emr=subset(mort.split.reg,g.whoregion=="EMR")
# mort.split.reg.emr$i=seq(1,dim(mort.split.reg.emr)[1],1)
# mort.split.reg.emr=mort.split.reg.emr %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
# mort.split.reg.emr=mort.split.reg.emr %>% arrange(rev(i))
# 
# mort.split.reg.eur=subset(mort.split.reg,g.whoregion=="EUR")
# mort.split.reg.eur$i=seq(1,dim(mort.split.reg.eur)[1],1)
# mort.split.reg.eur=mort.split.reg.eur %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
# mort.split.reg.eur=mort.split.reg.eur %>% arrange(rev(i))
# 
# mort.split.reg.sea=subset(mort.split.reg,g.whoregion=="SEA")
# mort.split.reg.sea$i=seq(1,dim(mort.split.reg.sea)[1],1)
# mort.split.reg.sea=mort.split.reg.sea %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
# mort.split.reg.sea=mort.split.reg.sea %>% arrange(rev(i))
# 
# mort.split.reg.wpr=subset(mort.split.reg,g.whoregion=="WPR")
# mort.split.reg.wpr$i=seq(1,dim(mort.split.reg.wpr)[1],1)
# mort.split.reg.wpr=mort.split.reg.wpr %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
# mort.split.reg.wpr=mort.split.reg.wpr %>% arrange(rev(i))
# 




### TB deaths in children

death22=global$mort.nh.num[global$year==yr]
death22.lo=global$mort.nh.lo.num[global$year==yr]
death22.hi=global$mort.nh.hi.num[global$year==yr]

m.nh.l5=sum(Mglobsplt$mort[Mglobsplt$age=="0-4"])
m.nh.l5.lo=sum(Mglobsplt$mort.lo[Mglobsplt$age=="0-4"])
m.nh.l5.hi=sum(Mglobsplt$mort.hi[Mglobsplt$age=="0-4"])

m.nh.5.14=sum(Mglobsplt$mort[Mglobsplt$age=="5-14"])
m.nh.5.14.lo=sum(Mglobsplt$mort.lo[Mglobsplt$age=="5-14"])
m.nh.5.14.hi=sum(Mglobsplt$mort.hi[Mglobsplt$age=="5-14"])

m.nh.tot=m.nh.l5+m.nh.5.14
m.nh.tot.lo=m.nh.l5.lo+m.nh.5.14.lo
m.nh.tot.hi=m.nh.l5.hi+m.nh.5.14.hi


death22.h=global$mort.h.num[global$year==yr]
death22.h.lo=global$mort.h.lo.num[global$year==yr]
death22.h.hi=global$mort.h.hi.num[global$year==yr]



m.h.l5=sum(MPglobsplt$mort[MPglobsplt$age=="0-4"])
m.h.l5.lo=sum(MPglobsplt$mort.lo[MPglobsplt$age=="0-4"])
m.h.l5.hi=sum(MPglobsplt$mort.hi[MPglobsplt$age=="0-4"])

m.h.5.14=sum(MPglobsplt$mort[MPglobsplt$age=="5-14"])
m.h.5.14.lo=sum(MPglobsplt$mort.lo[MPglobsplt$age=="5-14"])
m.h.5.14.hi=sum(MPglobsplt$mort.hi[MPglobsplt$age=="5-14"])

m.h.tot=m.h.l5+m.h.5.14
m.h.tot.lo=m.h.l5.lo+m.h.5.14.lo
m.h.tot.hi=m.h.l5.hi+m.h.5.14.hi


m.tot.children=m.nh.tot+m.h.tot
m.tot.children.lo=m.nh.tot.lo+m.h.tot.lo
m.tot.children.hi=m.nh.tot.hi+m.h.tot.hi



### TB deaths in adults

death22=global$mort.nh.num[global$year==yr]
death22.lo=global$mort.nh.lo.num[global$year==yr]
death22.hi=global$mort.nh.hi.num[global$year==yr]

m.nh.male=sum(Mglobsplt$mort[Mglobsplt$child==F & Mglobsplt$sex=="M"])
m.nh.male.lo=sum(Mglobsplt$mort.lo[Mglobsplt$child==F & Mglobsplt$sex=="M"])
m.nh.male.hi=sum(Mglobsplt$mort.hi[Mglobsplt$child==F & Mglobsplt$sex=="M"])

m.nh.fem=sum(Mglobsplt$mort[Mglobsplt$child==F & Mglobsplt$sex=="F"])
m.nh.fem.lo=sum(Mglobsplt$mort.lo[Mglobsplt$child==F & Mglobsplt$sex=="F"])
m.nh.fem.hi=sum(Mglobsplt$mort.hi[Mglobsplt$child==F & Mglobsplt$sex=="F"])

death22.h=global$mort.h.num[global$year==yr]
death22.h.lo=global$mort.h.lo.num[global$year==yr]
death22.h.hi=global$mort.h.hi.num[global$year==yr]

m.h.male=sum(MPglobsplt$mort[MPglobsplt$child==F & MPglobsplt$sex=="M"])
m.h.male.lo=sum(MPglobsplt$mort.lo[MPglobsplt$child==F & MPglobsplt$sex=="M"])
m.h.male.hi=sum(MPglobsplt$mort.hi[MPglobsplt$child==F & MPglobsplt$sex=="M"])

m.h.fem=sum(MPglobsplt$mort[MPglobsplt$child==F & MPglobsplt$sex=="F"])
m.h.fem.lo=sum(MPglobsplt$mort.lo[MPglobsplt$child==F & MPglobsplt$sex=="F"])
m.h.fem.hi=sum(MPglobsplt$mort.hi[MPglobsplt$child==F & MPglobsplt$sex=="F"])


m.tot.male=m.nh.male+m.h.male
m.tot.male.lo=m.nh.male.lo+m.h.male.lo
m.tot.male.hi=m.nh.male.hi+m.h.male.hi

m.tot.fem=m.nh.fem+m.h.fem
m.tot.fem.lo=m.nh.fem.lo+m.h.fem.lo
m.tot.fem.hi=m.nh.fem.hi+m.h.fem.hi


### Countries using mathematical models
cm.lst=unique(est.method$iso3[est.method$method.inc==2 | est.method$method.inc==5 |
                              est.method$method.inc==8 | est.method$method.inc==11])

rm.lst=unique(est.method$iso3[est.method$method.inc==3 | est.method$method.inc==6 |
                              est.method$method.inc==9 | est.method$method.inc==12])

n.model.inc=length(cm.lst)+length(rm.lst)


cm.lst=unique(est.method$iso3[est.method$method.mort==2 | est.method$method.mort==5 |
                                est.method$method.mort==8])

rm.lst=unique(est.method$iso3[est.method$method.mort==3 | est.method$method.mort==6 |
                                est.method$method.mort==9])

n.model.mort=length(cm.lst)+length(rm.lst)



### Change in Incidence by age group since 2015

change.inc.age=db_estimates_group_all[year %in% c(2015,yr) & group_type=="global" & (sex=="m" | sex=="f") & age_group %ni% c("0-14","5-9","10-14","15-19","20-24","15plus","a"),]

vars_to_aggregate <- c("e.pop.m04", "e.pop.m514", "e.pop.m1524", "e.pop.m2534", "e.pop.m3544", "e.pop.m4554", "e.pop.m5564", "e.pop.m65",
                       "e.pop.f04", "e.pop.f514", "e.pop.f1524", "e.pop.f2534", "e.pop.f3544", "e.pop.f4554", "e.pop.f5564", "e.pop.f65")

poptot <- pop[year==2015 | year == yr] %>%
  group_by(year) %>%
  summarise(across(all_of(vars_to_aggregate), sum, .names = "{.col}_sum"))


change.inc.age[year==2015 & sex =="m" & age_group=="0-4", pop:=poptot$e.pop.m04_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="m" & age_group=="5-14", pop:=poptot$e.pop.m514_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="m" & age_group=="15-24", pop:=poptot$e.pop.m1524_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="m" & age_group=="25-34", pop:=poptot$e.pop.m2534_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="m" & age_group=="35-44", pop:=poptot$e.pop.m3544_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="m" & age_group=="45-54", pop:=poptot$e.pop.m4554_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="m" & age_group=="55-64", pop:=poptot$e.pop.m5564_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="m" & age_group=="65plus", pop:=poptot$e.pop.m65_sum[poptot$year==2015]]

change.inc.age[year==2024 & sex =="m" & age_group=="0-4", pop:=poptot$e.pop.m04_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="m" & age_group=="5-14", pop:=poptot$e.pop.m514_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="m" & age_group=="15-24", pop:=poptot$e.pop.m1524_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="m" & age_group=="25-34", pop:=poptot$e.pop.m2534_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="m" & age_group=="35-44", pop:=poptot$e.pop.m3544_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="m" & age_group=="45-54", pop:=poptot$e.pop.m4554_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="m" & age_group=="55-64", pop:=poptot$e.pop.m5564_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="m" & age_group=="65plus", pop:=poptot$e.pop.m65_sum[poptot$year==2024]]

change.inc.age[year==2015 & sex =="f" & age_group=="0-4", pop:=poptot$e.pop.f04_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="f" & age_group=="5-14", pop:=poptot$e.pop.f514_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="f" & age_group=="15-24", pop:=poptot$e.pop.f1524_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="f" & age_group=="25-34", pop:=poptot$e.pop.f2534_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="f" & age_group=="35-44", pop:=poptot$e.pop.f3544_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="f" & age_group=="45-54", pop:=poptot$e.pop.f4554_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="f" & age_group=="55-64", pop:=poptot$e.pop.f5564_sum[poptot$year==2015]]
change.inc.age[year==2015 & sex =="f" & age_group=="65plus", pop:=poptot$e.pop.f65_sum[poptot$year==2015]]

change.inc.age[year==2024 & sex =="f" & age_group=="0-4", pop:=poptot$e.pop.f04_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="f" & age_group=="5-14", pop:=poptot$e.pop.f514_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="f" & age_group=="15-24", pop:=poptot$e.pop.f1524_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="f" & age_group=="25-34", pop:=poptot$e.pop.f2534_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="f" & age_group=="35-44", pop:=poptot$e.pop.f3544_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="f" & age_group=="45-54", pop:=poptot$e.pop.f4554_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="f" & age_group=="55-64", pop:=poptot$e.pop.f5564_sum[poptot$year==2024]]
change.inc.age[year==2024 & sex =="f" & age_group=="65plus", pop:=poptot$e.pop.f65_sum[poptot$year==2024]]


change.inc.age.wide <- pivot_wider(
  data = change.inc.age,
  id_cols = c(group_type, group_name, measure, unit, age_group, sex),
  names_from = year,
  values_from = c(best, lo, hi,pop)
)

change.inc.age.wide <- change.inc.age.wide %>%
  mutate(
    
    best_2015_rate=best_2015*1e5/pop_2015,
    best_2025_rate=best_2025*1e5/pop_2025,
    lo_2015_rate=lo_2015*1e5/pop_2015,
    lo_2025_rate=lo_2025*1e5/pop_2025,
    hi_2015_rate=hi_2015*1e5/pop_2015,
    hi_2025_rate=hi_2025*1e5/pop_2025,
    
    sd_2025_rate=(hi_2025_rate-lo_2025_rate) / 3.92,
    sd_2015_rate=(hi_2015_rate-lo_2015_rate) / 3.92,
 
    prop_change_rate = (best_2025_rate - best_2015_rate) / best_2015_rate,
    sd_prop_change_rate = (1 / abs(best_2015_rate)) * sqrt( ((best_2025_rate / best_2015_rate)^2 * sd_2015_rate^2) + sd_2025_rate^2 ),
    prop_change_lo_rate = prop_change_rate - 1.96*sd_prop_change_rate,
    prop_change_hi_rate = prop_change_rate + 1.96*sd_prop_change_rate
  )



change.inc.age.wide <- change.inc.age.wide %>%
  mutate(
    prop_change_rate = 100*prop_change_rate,
    prop_change_lo_rate = prop_change_lo_rate*100 ,
    prop_change_hi_rate = prop_change_hi_rate*100
  )


change.inc.age.wide <- pivot_wider(
  data = change.inc.age.wide,
  id_cols = c(age_group),
  names_from = sex,
  values_from = c(prop_change_rate, prop_change_lo_rate, prop_change_hi_rate)
)










change.inc.age.wide$age_group=factor(change.inc.age.wide$age_group,levels=c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65plus"),
       labels=c("0\u20134", "5\u201314", "15\u201324", "25\u201334", "35\u201344", "45\u201354", "55\u201364", "\u226565"))


change.inc.age.wide %>% arrange(desc(age_group))







# New fish bone graph for age/sex disagg

# Define the amount of horizontal dodging
dodge <- position_dodge(width = 0.6)
# Generate the plot

### with notif



globsplt3=db_estimates_group_all[year==yr & group_type=="global" & (sex=="m" | sex=="f") & age_group %ni% c("0-14","5-9","10-14","15-19","20-24","15plus","a")
                                & year > 2009,]
globsplt3[sex=="m", sex:="M"]
globsplt3[sex=="f", sex:="F"]

globsplt3=merge(globsplt3,newrel.all.glob,by=c("sex","age_group"),all.x=T)


# Reorder the 'age_group' factor to ensure a logical order on the x-axis
age_order <- c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65plus")
globsplt3$age_group <- factor(globsplt3$age_group, levels = age_order)


globsplt3.piv <- pivot_wider(
  data = globsplt3,
  id_cols = c(age_group),
  names_from = sex,
  values_from = c(best,lo,hi,newrel)
)

# Reorder the 'age_group' factor to ensure a logical order on the x-axis
age_order <- c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65plus")
globsplt3.piv$age_group <- factor(globsplt3.piv$age_group, levels = age_order)



# Define the amount of horizontal dodging to offset M and F
dodge <- position_dodge(width = 0.8)

# Define the color palette to be used for both bars and points
color_palette <- c("M" = "#F4A81D", "F" = "#6363C0") # Blue for M, Orange-Red for F

# Generate the plot
combined_plot_with_bars <- ggplot(globsplt3, aes(x = age_group, group = sex)) +
  
  # 1. Add the new BAR CHART layer for the 'newrel' variable
  # `geom_col` is used for bars where the height is a value in the data.
  # We use `fill` to color the bars and `alpha` to make them semi-transparent.
  geom_col(
    aes(y = newrel, fill = sex),
    position = dodge,
    width = 0.7,
    alpha = 0.6
  ) +
  
  # 2. Add the ERROR BAR layer for the 'best', 'lo', and 'hi' variables
  # This is the same as the previous plot.
  geom_errorbar(
    aes(y = best, ymin = lo, ymax = hi, color = sex),
    width = 0.3, # Width of the horizontal caps
    linewidth = 0.8,
    position = dodge
  ) +
  
  # 3. Add the POINT layer for the 'best' estimate
  # This is also the same as the previous plot.
  geom_point(
    aes(y = best, color = sex),
    size = 3,
    position = dodge
  ) +
  
  # 4. Manually set the colors and labels for the plot
  # We define scales for both `fill` (for the bars) and `color` (for points/lines)
  # Using the same parameters for both ensures they share a single, combined legend.
  scale_fill_manual(
    name = "Sex",
    values = color_palette,
    labels = c("M" = "Male", "F" = "Female")
  ) +
  scale_color_manual(
    name = "Sex",
    values = color_palette,
    labels = c("M" = "Male", "F" = "Female")
  ) +
  
  # Add titles and labels for clarity
  labs(
    title = "Incidence and TB notifications by age group and sex",
    subtitle = "Bars represent TB notifications, while points and error bars show the incidence estimate and its range",
    x = "Age group",
    y = "TB incidence (absolute number)"
  ) +
  
  # Apply a clean theme and customizations
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  ) +
  
  # Format y-axis numbers with commas for readability
  scale_y_continuous(labels = scales::comma)



# By region


regsplt2=db_estimates_group_all[year==yr & group_type=="g_whoregion" & (sex=="m" | sex=="f") & age_group %ni% c("0-14","5-9","10-14","15-19","20-24","15plus","a")
                               & year > 2009,]

regsplt2[sex=="m", sex:="M"]
regsplt2[sex=="f", sex:="F"]

regsplt2$g.whoregion=regsplt2$group_name
regsplt2=merge(regsplt2,newrel.all,by=c("g.whoregion","sex","age_group"),all.x=T)

### Regions names

regsplt2$name <- factor(regsplt2$g.whoregion, levels=c('AFR','AMR','SEA','EUR','EMR','WPR'))

levels(regsplt2$name) <-
  c(
    'African Region',
    'Region of the Americas',
    'South-East Asia Region',
    'European Region',
    'Eastern Mediterranean Region',
    'Western Pacific Region'
  )
regsplt2$name <- ordered(regsplt2$name)


# Reorder the 'age_group' factor to ensure a logical order on the x-axis
age_order <- c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65plus")
regsplt2$age_group <- factor(regsplt2$age_group, levels = age_order)





regsplt2.piv <- pivot_wider(
  data = regsplt2,
  id_cols = c(name,age_group),
  names_from = sex,
  values_from = c(best,lo,hi,newrel)
)


disag_afr=subset(regsplt2.piv,name=="African Region") 
disag_emr=subset(regsplt2.piv,name=="Eastern Mediterranean Region") 
disag_eur=subset(regsplt2.piv,name=="European Region") 
disag_amr=subset(regsplt2.piv,name=="Region of the Americas") 
disag_sea=subset(regsplt2.piv,name=="South-East Asia Region") 
disag_wpr=subset(regsplt2.piv,name=="Western Pacific Region") 


# Define the amount of horizontal dodging to offset M and F
dodge <- position_dodge(width = 0.8)

# Define the color palette to be used for both bars and points
color_palette <- c("M" = "#F4A81D", "F" = "#6363C0") # Blue for M, Orange-Red for F



# Generate the plot
combined_plot_with_bars_reg <- ggplot(regsplt2, aes(x = age_group, group = sex)) +
  
  # 1. Add the new BAR CHART layer for the 'newrel' variable
  # `geom_col` is used for bars where the height is a value in the data.
  # We use `fill` to color the bars and `alpha` to make them semi-transparent.
  geom_col(
    aes(y = newrel, fill = sex),
    position = dodge,
    width = 0.7,
    alpha = 0.6
  ) +
  
  # 2. Add the ERROR BAR layer for the 'best', 'lo', and 'hi' variables
  # This is the same as the previous plot.
  geom_errorbar(
    aes(y = best, ymin = lo, ymax = hi, color = sex),
    width = 0.3, # Width of the horizontal caps
    linewidth = 0.8,
    position = dodge
  ) +
  
  # 3. Add the POINT layer for the 'best' estimate
  # This is also the same as the previous plot.
  geom_point(
    aes(y = best, color = sex),
    size = 3,
    position = dodge
  ) +
  
  facet_wrap(~ name, ncol = 2, scales = "free_y") +
  
  # 4. Manually set the colors and labels for the plot
  # We define scales for both `fill` (for the bars) and `color` (for points/lines)
  # Using the same parameters for both ensures they share a single, combined legend.
  scale_fill_manual(
    name = "Sex",
    values = color_palette,
    labels = c("M" = "Male", "F" = "Female")
  ) +
  scale_color_manual(
    name = "Sex",
    values = color_palette,
    labels = c("M" = "Male", "F" = "Female")
  ) +
  
  # Add titles and labels for clarity
  labs(
    title = "Incidence and TB notifications by age group and sex",
    subtitle = "Bars represent TB notifications, while points and error bars show the incidence estimate and its range",
    x = "Age group",
    y = "TB incidence (absolute number)"
  ) +
  
  # Apply a clean theme and customizations
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  ) +
  
  # Format y-axis numbers with commas for readability
  scale_y_continuous(labels = scales::comma) 









### HIV neg Mortality

# Ensure the age groups are ordered logically on the axis
age_order <- c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54","55-64", "65plus")
Mglobsplt$age <- factor(Mglobsplt$age, levels = age_order)


Mglobsplt2=Mglobsplt[,.(sex,age,mort,lo=mort.lo,hi=mort.hi)]

Mglobsplt.piv <- pivot_wider(
  data = Mglobsplt2,
  id_cols = c(age),
  names_from = sex,
  values_from = c(mort,lo,hi)
)




# Define the amount of vertical dodging to offset M and F
dodge <- position_dodge(width = 0.6)

# Define the color palette to be used for both bars and points
color_palette <- c("M" = "#F4A81D", "F" = "#6363C0") # Blue for M, Orange-Red for F


# --- Generate the Plot ---
mortality_plot_updated <- ggplot(
  Mglobsplt,
  # The 'shape' aesthetic has been removed from this line
  aes(x = age, y = mort, color = sex, group = sex)
) +
  
  # Add the error bars
  geom_errorbar(
    aes(ymin = mort.lo, ymax = mort.hi),
    width = 0.2,
    linewidth = 0.8,
    position = dodge
  ) +
  
  # Add the points for the central estimate
  geom_point(size = 4, position = dodge) +
  
  # Use the new color palette you provided
  scale_color_manual(
    name = "Sex",
    values = c("M" = "#F4A81D", "F" = "#6363C0"),
    labels = c("M" = "Male", "F" = "Female")
  ) +
  
  # Add titles and labels
  labs(
    title = "Mortality by Age and Sex",
    x = "Age Group",
    y = "Number of deaths"
  ) +
  
  # Format the horizontal axis to have commas
  scale_y_continuous(labels = scales::comma) +
  
  # Apply a clean theme
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "top"
  ) 
  

# HIV post mortality
# Ensure the age groups are ordered logically on the axis
age_order <- c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54","55-64", "65plus")
MPglobsplt$age <- factor(MPglobsplt$age, levels = age_order)


MPglobsplt2=MPglobsplt[,.(sex,age,mort,lo=mort.lo,hi=mort.hi)]

MPglobsplt.piv <- pivot_wider(
  data = MPglobsplt2,
  id_cols = c(age),
  names_from = sex,
  values_from = c(mort,lo,hi)
)


# Define the amount of vertical dodging to offset M and F
dodge <- position_dodge(width = 0.6)

# Define the color palette to be used for both bars and points
color_palette <- c("M" = "#F4A81D", "F" = "#6363C0") # Blue for M, Orange-Red for F


# --- Generate the Plot ---
mortality_plot_updatedP <- ggplot(
  MPglobsplt,
  # The 'shape' aesthetic has been removed from this line
  aes(x = age, y = mort, color = sex, group = sex)
) +
  
  # Add the error bars
  geom_errorbar(
    aes(ymin = mort.lo, ymax = mort.hi),
    width = 0.2,
    linewidth = 0.8,
    position = dodge
  ) +
  
  # Add the points for the central estimate
  geom_point(size = 4, position = dodge) +
  
  # Use the new color palette you provided
  scale_color_manual(
    name = "Sex",
    values = c("M" = "#F4A81D", "F" = "#6363C0"),
    labels = c("M" = "Male", "F" = "Female")
  ) +
  
  # Add titles and labels
  labs(
    title = "Mortality by Age and Sex",
    x = "Age Group",
    y = "Number of deaths"
  ) +
  
  # Format the horizontal axis to have commas
  scale_y_continuous(labels = scales::comma) +
  
  # Apply a clean theme
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "top"
  ) 


### HIV neg Mortality regional

# Ensure the age groups are ordered logically on the axis
age_order <- c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54","55-64", "65plus")
Mregsplt$age <- factor(Mregsplt$age, levels = age_order)


Mregsplt2=Mregsplt[,.(name,sex,age,mort,lo=mort.lo,hi=mort.hi)]

Mregsplt.piv <- pivot_wider(
  data = Mregsplt2,
  id_cols = c(name,age),
  names_from = sex,
  values_from = c(mort,lo,hi)
)



mdisag_afr=subset(Mregsplt.piv,name=="African Region") 
mdisag_emr=subset(Mregsplt.piv,name=="Eastern Mediterranean Region") 
mdisag_eur=subset(Mregsplt.piv,name=="European Region") 
mdisag_amr=subset(Mregsplt.piv,name=="Region of the Americas") 
mdisag_sea=subset(Mregsplt.piv,name=="South-East Asia Region") 
mdisag_wpr=subset(Mregsplt.piv,name=="Western Pacific Region") 





# Define the amount of vertical dodging to offset M and F
dodge <- position_dodge(width = 0.6)

# Define the color palette to be used for both bars and points
color_palette <- c("M" = "#F4A81D", "F" = "#6363C0") # Blue for M, Orange-Red for F


# --- Generate the Plot ---
mortality_plot_updated_reg <- ggplot(
  Mregsplt,
  # The 'shape' aesthetic has been removed from this line
  aes(x = age, y = mort, color = sex, group = sex)
) +
  
  # Add the error bars
  geom_errorbar(
    aes(ymin = mort.lo, ymax = mort.hi),
    width = 0.2,
    linewidth = 0.8,
    position = dodge
  ) +
  
  # Add the points for the central estimate
  geom_point(size = 4, position = dodge) +
  
  facet_wrap(~ name, ncol = 2, scales = "free_y") +
  
  
  # Use the new color palette you provided
  scale_color_manual(
    name = "Sex",
    values = c("M" = "#F4A81D", "F" = "#6363C0"),
    labels = c("M" = "Male", "F" = "Female")
  ) +
  
  # Add titles and labels
  labs(
    title = "Mortality by Age and Sex",
    x = "Age Group",
    y = "Number of deaths"
  ) +
  
  # Format the horizontal axis to have commas
  scale_y_continuous(labels = scales::comma) +
  
  # Apply a clean theme
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "top"
  ) 


### HIV pos Mortality regional

# Ensure the age groups are ordered logically on the axis
age_order <- c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54","55-64", "65plus")
MPregsplt$age <- factor(MPregsplt$age, levels = age_order)

# Define the amount of vertical dodging to offset M and F
dodge <- position_dodge(width = 0.6)

# Define the color palette to be used for both bars and points
color_palette <- c("M" = "#F4A81D", "F" = "#6363C0") # Blue for M, Orange-Red for F


# --- Generate the Plot ---
mortality_plot_updated_regP <- ggplot(
  MPregsplt,
  # The 'shape' aesthetic has been removed from this line
  aes(x = age, y = mort, color = sex, group = sex)
) +
  
  # Add the error bars
  geom_errorbar(
    aes(ymin = mort.lo, ymax = mort.hi),
    width = 0.2,
    linewidth = 0.8,
    position = dodge
  ) +
  
  # Add the points for the central estimate
  geom_point(size = 4, position = dodge) +
  
  facet_wrap(~ name, ncol = 2, scales = "free_y") +
  
  
  # Use the new color palette you provided
  scale_color_manual(
    name = "Sex",
    values = c("M" = "#F4A81D", "F" = "#6363C0"),
    labels = c("M" = "Male", "F" = "Female")
  ) +
  
  # Add titles and labels
  labs(
    title = "Mortality by Age and Sex",
    x = "Age Group",
    y = "Number of deaths"
  ) +
  
  # Format the horizontal axis to have commas
  scale_y_continuous(labels = scales::comma) +
  
  # Apply a clean theme
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "top"
  ) 


###

hest <- hest |> 
  mutate(country = ifelse(country == "Democratic People's Republic of Korea", "Democratic People's\nRepublic of Korea",
                          ifelse(country == "Democratic Republic of the Congo", "Democratic Republic\nof the Congo",
                                 country
                          )))


### Countries reaching 2020 milestone
### Create Word and Excel files listing these countries 

library(data.table)
library(flextable)
library(officer)
library(openxlsx)
library(here)

est2 <- est[year %in% 2015:2025, .(iso3, country, year, inc, mort.num)]
est2 <- merge(est2, cty[, .(iso3, g.whostatus, g.whoregion)], by = "iso3")
est2 <- est2[g.whostatus == "M", ]

# Milestone thresholds
inc_milestone2020_pct  <- 20
mort_milestone2020_pct <- 35
inc_milestone2025_pct  <- 50
mort_milestone2025_pct <- 75

# Decline percentages relative to 2015
est2[, ref_inc  := inc[year == 2015], by = iso3]
est2[, ref_mort := mort.num[year == 2015], by = iso3]
est2[, inc_decline_pct  := ((ref_inc - inc) / ref_inc) * 100]
est2[, mort_decline_pct := ((ref_mort - mort.num) / ref_mort) * 100]

final_year_data <- est2[year == 2025]

# Qualifying countries in 2025 for each of the four milestones
qualifying_inc2020  <- final_year_data[inc_decline_pct  >= inc_milestone2020_pct,  .(iso3)]
qualifying_mort2020 <- final_year_data[mort_decline_pct >= mort_milestone2020_pct, .(iso3)]
qualifying_inc2025  <- final_year_data[inc_decline_pct  >= inc_milestone2025_pct,  .(iso3)]
qualifying_mort2025 <- final_year_data[mort_decline_pct >= mort_milestone2025_pct, .(iso3)]

# First year each qualifying country reached its milestone (min year, floor 2020)
first_inc2020 <- est2[iso3 %in% qualifying_inc2020$iso3 & inc_decline_pct >= inc_milestone2020_pct,
                      .(first_year = min(year)), by = iso3][, milestone := "Incidence decline ≥ 20%"]
first_mort2020 <- est2[iso3 %in% qualifying_mort2020$iso3 & mort_decline_pct >= mort_milestone2020_pct,
                       .(first_year = min(year)), by = iso3][, milestone := "Mortality decline ≥ 35%"]
first_inc2025 <- est2[iso3 %in% qualifying_inc2025$iso3 & inc_decline_pct >= inc_milestone2025_pct,
                      .(first_year = min(year)), by = iso3][, milestone := "Incidence decline ≥ 50%"]
first_mort2025 <- est2[iso3 %in% qualifying_mort2025$iso3 & mort_decline_pct >= mort_milestone2025_pct,
                       .(first_year = min(year)), by = iso3][, milestone := "Mortality decline ≥ 75%"]

all_milestones <- rbindlist(list(first_inc2020, first_mort2020, first_inc2025, first_mort2025))
all_milestones[, first_year := pmax(first_year, 2015)]
all_milestones <- merge(all_milestones, unique(est2[, .(iso3, country, g.whoregion)]), by = "iso3")

setorder(all_milestones, g.whoregion, milestone, first_year, country)

# Columns that should render bold (the stricter, 2025-tier milestones)
bold_cols <- c("Incidence decline ≥ 50%", "Mortality decline ≥ 75%")
col_order <- c("year", "Incidence decline ≥ 20%", "Mortality decline ≥ 35%",
               "Incidence decline ≥ 50%", "Mortality decline ≥ 75%")

regions <- sort(unique(all_milestones$g.whoregion))

region_tables <- lapply(regions, function(reg) {
  dat <- all_milestones[g.whoregion == reg]
  tab <- dcast(dat, first_year ~ milestone, value.var = "country", fun.aggregate = toString)
  setnames(tab, "first_year", "year")
  # Ensure all four milestone columns exist even if a region has none for one
  missing_cols <- setdiff(col_order, names(tab))
  for (mc in missing_cols) tab[[mc]] <- NA_character_
  setcolorder(tab, col_order)
  as.data.frame(tab)
})
names(region_tables) <- regions


### Create Word doc
doc <- read_docx()

for (reg in regions) {
  df <- region_tables[[reg]]
  ft <- flextable(df)
  ft <- fit_to_width(ft, max_width = 6.5)
  
  # Bold the header and body of the 2025-tier columns
  ft <- bold(ft, j = bold_cols, bold = TRUE, part = "all")
  
  doc <- doc %>%
    body_add_par(reg, style = "heading 2") %>%
    body_add_flextable(ft) %>%
    body_add_par("", style = "Normal")
}

print(doc, target = here("report/local/milestone_report_by_region.docx"))





### Create the Excel file

wb <- createWorkbook()
addWorksheet(wb, "Milestones by Region")

title_style  <- createStyle(textDecoration = "bold", fontSize = 12)
header_style <- createStyle(textDecoration = "bold", border = "TopBottom", fgFill = "#D9E1F2")
bold_style   <- createStyle(textDecoration = "bold")  # for 2025-tier cell contents

current_row <- 1
bold_col_idx <- which(col_order %in% bold_cols)  # column positions, consistent across regions

for (reg in regions) {
  df <- region_tables[[reg]]
  
  writeData(wb, "Milestones by Region", reg, startRow = current_row, startCol = 1)
  addStyle(wb, "Milestones by Region", title_style, rows = current_row, cols = 1)
  current_row <- current_row + 1
  
  writeData(wb, "Milestones by Region", df,
            startRow = current_row, startCol = 1,
            headerStyle = header_style)
  
  # Bold the 2025-tier columns' header + body
  addStyle(wb, "Milestones by Region", bold_style,
           rows = current_row:(current_row + nrow(df)),
           cols = bold_col_idx, gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, "Milestones by Region", cols = 1:ncol(df), widths = "auto")
  
  current_row <- current_row + nrow(df) + 2
}

saveWorkbook(wb, here("report/local/milestone_report_by_region.xlsx"), overwrite = TRUE)



### Create the Excel file (one row per country)

milestone_rename <- c(
  "Incidence decline ≥ 20%" = "Incidence 2020 milestone",
  "Mortality decline ≥ 35%" = "Mortality 2020 milestone",
  "Incidence decline ≥ 50%" = "Incidence 2025 milestone",
  "Mortality decline ≥ 75%" = "Mortality 2025 milestone"
)

# Long -> wide: one row per country, one column per milestone, value = first_year
# (g.whoregion is carried along on the LHS since it's unique per country)
country_table <- dcast(all_milestones, country + g.whoregion ~ milestone, value.var = "first_year")
setnames(country_table, c("country", "g.whoregion"), c("Country", "WHO region"))

# Rename only the milestone columns that are actually present (a milestone may
# have been met by zero countries, in which case dcast won't create a column for it)
for (old_name in names(milestone_rename)) {
  if (old_name %in% names(country_table)) {
    setnames(country_table, old_name, milestone_rename[[old_name]])
  }
}

milestone_col_order <- c("Country", "WHO region", "Incidence 2020 milestone", "Mortality 2020 milestone",
                         "Incidence 2025 milestone", "Mortality 2025 milestone")
missing_cols <- setdiff(milestone_col_order, names(country_table))
for (mc in missing_cols) country_table[[mc]] <- NA_integer_
setcolorder(country_table, milestone_col_order)
setorder(country_table, `WHO region`, Country)
country_table <- as.data.frame(country_table)

wb <- createWorkbook()
addWorksheet(wb, "Milestones by Country")

header_style <- createStyle(textDecoration = "bold", border = "TopBottom", fgFill = "#D9E1F2")
bold_style   <- createStyle(textDecoration = "bold")  # for 2025-tier cell contents

writeData(wb, "Milestones by Country", country_table, headerStyle = header_style)

# Bold the 2025-tier columns' header + body
bold_col_idx <- which(milestone_col_order %in% c("Incidence 2025 milestone", "Mortality 2025 milestone"))
addStyle(wb, "Milestones by Country", bold_style,
         rows = 1:(nrow(country_table) + 1),
         cols = bold_col_idx, gridExpand = TRUE, stack = TRUE)

setColWidths(wb, "Milestones by Country", cols = 1:ncol(country_table), widths = "auto")

saveWorkbook(wb, here("report/local/milestone_report_by_country.xlsx"), overwrite = TRUE)








### UNAIDS global update

unaids.glob <-fread(here("data/unaids/unaids_global.csv"))
setDT(unaids.glob)



### Regional progress 2015 2025


reduction_dt <- regional[year %in% c(2015, 2025), 
                         dcast(.SD, g.whoregion ~ year, value.var = "mort.num")][
                           , pct_reduction := ((`2015` - `2025`) / `2015`) * 100
                         ][
                           , formatted_reduction := fifelse(
                             abs(pct_reduction) >= 10, 
                             sprintf("%.0f%%", round(pct_reduction)), 
                             sprintf("%.1f%%", pct_reduction)
                           )
                         ]


### Both milestone
library(dplyr)
library(tidyr)
target_countries <- est %>%
  filter(year %in% c(2015, 2025)) %>%
  select(iso3, year, inc, mort.num, g.whostatus) %>%
  pivot_wider(
    names_from = year, 
    values_from = c(inc, mort.num),
    names_prefix = "yr_"
  ) %>%
  mutate(
    inc_reduction_pct = ((inc_yr_2015  - inc_yr_2025 ) / inc_yr_2015 ) * 100,
    mort_reduction_pct = ((mort.num_yr_2015 - mort.num_yr_2025) / mort.num_yr_2015) * 100
  ) %>%
  filter(
    inc_reduction_pct >= 20,
    mort_reduction_pct >= 35,
    g.whostatus == "M"
  )

# View the resulting countries
print(target_countries)
dim(target_countries
    )










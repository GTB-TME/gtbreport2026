#' ---
#' title: Aggregated PAF and attributable cases for the 5 risk factors
#' author: Mathieu Bastard
#' date: 26/06/2025
#' 
#' 
#' 
#' Deps: libraries data.table, here and propagate
#' 
#' 
#' Input: att.rda (see script ~/R/07-attr.R)
#'        global.rda (global estimates), est.rda (country estimates), rf.global.rda
#' 
#' 
#' 
#' Table 6.3.1 - Attributable cases
#'
#Run Functions script


source(here('inc_mort/R code/fun.R'))

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(propagate))
suppressPackageStartupMessages(library(dplyr))

library(here)

# !!! add here the libraries needed to run lines 84-102 (coded added by TME)

load(here("inc_mort/analysis/attributable_cases.rda"))
load(here("inc_mort/analysis/global.rda"))
load(here("inc_mort/analysis/est.rda"))
load(here("inc_mort/analysis/rf.global.rda"))

vglohi <- Vectorize(glohi, c("ev", "sd"))

att <- att %>% subset(sex=="a")

yr=2024


#' Global IRR
hiv <- est[year == yr, addXY(hiv, r.sd = hiv.sd, weights = pop)] # TY:changed from yr to year

DT <- cbind(
  inc.h = c(last(global$inc.h), last(global$inc.h.sd)),
  hiv = c(hiv$r, hiv$r.sd),
  inc.nh = c(last(global$inc.nh), last(global$inc.nh.sd))
)
EXPR <- expression((inc.h / hiv) / (inc.nh / (1 - hiv))) 
out <- propagate(EXPR, DT)

# # Risk Ratios (mean, 95%CIs)
rr.hiv <- with(out, c(prop[2], prop[5], prop[6]))  #generate "rr.hiv" object for table

rr.alc <-  c(3.33,2.14,5.19) # alcohol, Eur Resp Dis 2017
rr.dia <-  c(1.6,1.42,1.80) # diabetes, MA review in 2024
rr.und <-  c(1.96,1.73,2.21) # undernutrition, MA review in 2024
rr.smk <-  c(1.57,1.18,2.1) # smoking, GTB 2019

# generate a base df for summarizing RRs
dta <- data.table(
  risk.factor = c("alcohol", "diabetes old","diabetes", "hiv", "smoking","undernutrition old", "undernutrition"),
  rr = c(rr.alc[1], rr.dia[1],rr.dia[1], rr.hiv[1], rr.smk[1], rr.und[1],rr.und[1]),
  rr.lo = c(rr.alc[2], rr.dia[2], rr.dia[2], rr.hiv[2], rr.smk[2], rr.und[2],rr.und[2]),
  rr.hi = c(rr.alc[3], rr.dia[3], rr.dia[3], rr.hiv[3], rr.smk[3], rr.und[3],rr.und[3])
)


# add size of exposed population into the base df 
h <- 1000

att[,prev.und.new:=prev.und.new/100]
att[,prev.und.sd.new:=prev.und.sd.new/100]
att[,prev.dia.new:=prev.dia.new/100]
att[,prev.dia.sd.new:=prev.dia.sd.new/100]

att[, exp.alc := adults * alcohol]
att[, exp.dia := adults * diabetes]
att[, exp.dia.new := adults * prev.dia.new]


att[, exp.hiv := pop * hiv]
att[, exp.smk := adults * smoking]
att[, exp.und.new := pop * prev.und.new]
att[, exp.und := pop * undernutrition]

exposed <- att %>% dplyr::select(exp.alc,exp.dia,exp.dia.new,exp.hiv,exp.smk,exp.und,exp.und.new) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) 

exposed <- as.data.frame(t(exposed)) %>%
  rename(exposed=1)

dta <- cbind.data.frame(dta,exposed)

# add PAF into the base df 
pop <- att %>% dplyr::select(pop,adults) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) 

dta <- cbind.data.frame(dta,pop)


# take attributable from rf.global.rda and add into the base df 

dta=merge(dta,rf.global[,.(risk.factor,best,lo,hi)], by="risk.factor")


# recalculate PAF from the attributable cases, modify inc of those only in adults once available
load(here("inc_mort/analysis/global.rda"))

load(here("disaggregation/output/db_estimates_group_all.Rdata"))

dta$inc24=as.integer(global$inc.num[global$year==yr])

globsplt=db_estimates_group_all[group_name=="global" & year==yr]

inc.men=globsplt$best[globsplt$age_group=="15plus" & globsplt$sex =="m"]
inc.women=globsplt$best[globsplt$age_group=="15plus" & globsplt$sex =="f"]
inc.adult=inc.men+inc.women




dta <- dta %>%
  mutate(PAF = case_when(
    risk.factor=="hiv" ~ best/inc24,
    risk.factor=="diabetes old" ~ best/inc.adult,
    risk.factor=="diabetes" ~ best/inc.adult,
    risk.factor=="alcohol" ~ best/inc.adult,
    risk.factor=="smoking" ~ best/inc.adult,
    risk.factor=="undernutrition old" ~ best/inc24,
    risk.factor=="undernutrition" ~ best/inc24
  ))


dta=as.data.table(dta)

# export a raw table
fwrite(dta[,.(risk.factor,rr,rr.lo,rr.hi,exposed,PAF,best,lo,hi)], file = here::here("inc_mort/analysis/PAF/PAF and attributable cases raw.csv"))

# formatting the table with GTB style
dta <- dta %>% 
  dplyr::select(!pop&!adults&!inc24) %>%
  mutate(exposed=exposed/1e6,
         PAF=PAF*100,
         best=best/1e6,
         lo=lo/1e6,
         hi=hi/1e6) 
  
dta <- dta %>% 
  mutate(across(where(is.numeric), ftb)) 

# export the formatted table for GTBR
fwrite(dta, file = here::here("inc_mort/analysis/PAF/PAF and attributable cases report format.csv"))



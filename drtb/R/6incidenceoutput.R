## creating neat graphs and data
## libraries
rm(list = ls())
library(here)
library(data.table)
library(ggplot2)
library(ggrepel)
library(scales)
library(glue)

## utilities
source(here("import/load_gtb.R"))
source(here("drtb/R/utils/incdr.R"))
source(here("drtb/R/utils/ftb.R"))
gh <- function(x) glue(here(x))
ssum <- function(x, ...) sqrt(sum(x^2, ...)) # sqrt sum sqrs
brkt <- function(x, y) {
  paste0(
    ftb(x), " (",
    ftb(pmax(0, x - 1.96 * y)), " - ",
    ftb(x + 1.96 * y), ")"
  )
}
getdate <- function() glue(gsub("-", "-", Sys.Date()))


## old data for comparison
fn <- here("drtb/indata/dr.est.rda")
if (file.exists(fn)) {
  load(fn) # last year's db_dr_country renamed
} else {
  load(here("drtb/data/db_dr_country.25.rda"))
  dr.est <- data.table(db_dr_country)
  dr.est <- dr.est[, .(
    source_new, source_ret,
    iso3, year,
    e_rr_prop_new, e_rr_prop_new_lo, e_rr_prop_new_hi, e_rr_prop_new_se,
    e_rr_prop_ret, e_rr_prop_ret_lo, e_rr_prop_ret_hi, e_rr_prop_ret_se,
    e_inc_rr_num, e_inc_rr_num_lo, e_inc_rr_num_hi
  )]
  rm(db_dr_country)
  save(dr.est, file = fn)
}


## main data
noests.yet <- FALSE
if (noests.yet) {
  load(here("drtb/tmpdata/est.rda"))
} else {
  est <- load_gtb(
    "est",
    convert_dashes = TRUE, convert_data.table = TRUE
  )
}
load(file = here("drtb/data/rhofnr.Rdata"))


## NOTE
estyr <- est[, max(year)]

## NOTE 2025 sensitivity analysis
## SA <- ".indSA" #NOTE set to "" for basecase
SA <- "" # NOTE set to "" for basecase

## what is the model choice
mdl <- scan(
  here("drtb/R/utils/modelchoice.txt"),
  what = "char"
) # NOTE depends choice
cat("Using model: ", mdl, "\n")
load(file = gh("drtb/outdata/KO.{mdl}{SA}.Rdata"))


## results from this year on
cutyr <- 2015 #not changing

## recheck
setdiff(KO$iso3, est$iso3)
(missed <- setdiff(est$iso3, KO$iso3))
missed <- setdiff(missed, c("ANT", "SCG")) # NOTE not done
(fmissed <- setdiff(est$iso3, rhofnr$iso3))


## --- regional averages for TKL: seems to be missing from KO
unique(est[iso3 %in% missed, .(iso3, g.whoregion)]) #NOTE
KOR <- KO[,
  .(RR.mid = mean(RR.mid), RR.sdw = mean(RR.hi - RR.lo)),
  by = .(g_whoregion, patients, year, type)
]
KOR[, c("RR.lo", "RR.hi") := .(
  pmax(0, RR.mid - RR.sdw / 2),
  RR.mid + RR.sdw / 2
)]
KOR[, RR.sdw := NULL]
KOR <- KOR[g_whoregion %in% c("WPR", "EUR")] #for GRL, TKL
KOR[g_whoregion == "WPR", iso3 := "TKL"]
KOR[g_whoregion == "EUR", iso3 := "GRL"]
KO <- rbind(KO, KOR) # join on


## remove average GRL:
KO <- KO[iso3 != "GRL"]
## --- use DNK for GRL
KOR <- copy(KO[iso3 == "DNK"])
KOR[, iso3 := "GRL"]
KO <- rbind(KO, KOR) # join on

## checks:
setdiff(KO$iso3, est$iso3)
setdiff(KO$iso3, rhofnr$iso3)
setdiff(est$iso3, rhofnr$iso3)
KO[, all(table(iso3) == 52)]
rhofnr[, all(table(iso3) == 1)]


## create incidence:
## NOTE this silently uses rhofnr & est for other data
tout <- addIncidence(
  KO[year > 1999 & year <= estyr],
  secondorder = FALSE
) # NOTE excludes any current year

summary(tout)
(missing <- tout[is.na(inc.rr), unique(iso3)])
est[iso3 %in% missing, .(iso3, g.whoregion, year, inc.num)]

rhofnr[, unique(table(iso3))]
KO[, unique(table(iso3))]
tout[, unique(table(iso3))]

## no NA bug - but CUW, MNE, SRB, TLS from different years OK after 2015
##  "CUW" "MNE" "SRB" "SSD" "SXM" "TLS" <- countries with changed boundaries?
## est[iso3 %in% missing,range(year)] #= 2002-2023: missing 2000-2002?
## some missing from as recent as 2011, but none after 2015 so should not affect estimates of interest

## coherence check: zero when incidence is zero
unique(tout[
  inc.new.rr < 1e-10 & inc.new.rr.sd < 1e-9,
  .(iso3, year)
])
tzero <- setdiff(
  names(tout),
  c("iso3", "year", "g_whoregion", "inc.num", "inc.num.sd")
)
tout[inc.new.rr < 1e-10 & inc.new.rr.sd < 1e-9, c(tzero) := 0.0]
tout[inc.new.rr < 1e-10 & inc.new.rr.sd < 1e-9]



## --- create source string ---
load(gh("drtb/data/LB{SA}.Rdata"))
load(gh("drtb/data/YB{SA}.Rdata"))
src.cutyr <- 2000 # cutyr
svy.new <- YB[year >= src.cutyr & patients == "new", unique(iso3)]
svy.ret <- YB[year >= src.cutyr & patients == "ret", unique(iso3)]
svl.new <- LB[year >= src.cutyr & patients == "new", unique(iso3)]
svl.ret <- LB[year >= src.cutyr & patients == "ret", unique(iso3)]
src.new <- data.table(iso3 = est[, unique(iso3)])
src.ret <- data.table(iso3 = est[, unique(iso3)])
## new
src.new[iso3 %in% svy.new, source_new := "Survey"]
src.new[iso3 %in% svl.new, source_new := "Surveillance"]
src.new[
  iso3 %in% intersect(svy.new, svl.new),
  source_new := "Survey & Surveillance"
]
src.new[is.na(source_new), source_new := "Model"]
src.new[, table(source_new)]
## ret
src.ret[iso3 %in% svy.ret, source_ret := "Survey"]
src.ret[iso3 %in% svl.ret, source_ret := "Surveillance"]
src.ret[
  iso3 %in% intersect(svy.ret, svl.ret),
  source_ret := "Survey & Surveillance"
]
src.ret[is.na(source_ret), source_ret := "Model"]
src.ret[, table(source_ret)]
## join
src.both <- merge(src.new, src.ret, by = "iso3")



setdiff(est[, unique(iso3)], KO[, unique(iso3)]) # "ANT" "SCG"

## --- regional averages for TKL; NOTE GRL listed as surveillance, but data is from DNK
src.both[iso3 %in% c("TKL"), .(source_new, source_ret)]
src.both[
  iso3 %in% c("TKL"),
  c("source_new", "source_ret") := "Regional average"
]


## --------- country-level db output

## Filename
## db_dr_country_yyyy-mm-dd.csv
## Structure
## iso3,
## year,
## source_new,                           (“Survey” or “Surveillance” or “Model”)
## source_drs_year_new,
## source_drs_all_areas_new,       (1 for national, 0 for sub-national data source)
## e_rr_prop_new,
## e_rr_prop_new_lo,
## e_rr_prop_new_hi,
## e_rr_prop_new_se,
## e_mdr_prop_rr_new,
## source_ret,                        (“Survey” or “Surveillance” or “Model”)
## source_drs_year_ret,
## source_drs_all_areas_ret,       (1 for national, 0 for sub-national data source)
## e_rr_prop_ret,
## e_rr_prop_ret_lo,
## e_rr_prop_ret_hi,
## e_rr_prop_ret_se,
## e_mdr_prop_rr_ret,
## e_inc_rr_num,
## e_inc_rr_num_lo,
## e_inc_rr_num_hi,


## join proportion and incidence data
KOD <- dcast(KO, iso3 + year + g_whoregion ~ patients,
  value.var = c("RR.mid", "RR.lo", "RR.hi")
)
KOD <- KOD[, .(iso3, year,
  e_rr_prop_new = RR.mid_new / 1e2,
  e_rr_prop_new_lo = RR.lo_new / 1e2,
  e_rr_prop_new_hi = RR.hi_new / 1e2,
  e_rr_prop_new_se = (RR.hi_new - RR.lo_new) / 392,
  e_rr_prop_ret = RR.mid_ret / 1e2,
  e_rr_prop_ret_lo = RR.lo_ret / 1e2,
  e_rr_prop_ret_hi = RR.hi_ret / 1e2,
  e_rr_prop_ret_se = (RR.hi_ret - RR.lo_ret) / 392
)]
KOE <- tout[, .(iso3, year,
  e_inc_rr_num = inc.rr,
  e_inc_rr_num_lo = pmax(0, inc.rr - 1.96 * inc.rr.sd),
  e_inc_rr_num_hi = inc.rr + 1.96 * inc.rr.sd
)]
KOE <- merge(KOD, KOE, by = c("iso3", "year"))
attr(KOE, "timestamp") <- Sys.Date() # set date


## save all time version
fn <- gh("drtb/outdata/KOE{SA}.rda")
save(KOE, file = fn)


summary(KOE)

## restrict to post 2015
db_dr_country <- KOE[year >= cutyr]
db_dr_country <- merge(
  src.both, db_dr_country,
  by = "iso3", all.y = TRUE
) # merge in src
attr(db_dr_country, "timestamp") <- Sys.Date() # set date
summary(db_dr_country)
db_dr_country[, unique(table(iso3))]

## save out as  Rdata
fn <- gh("drtb/output/db_dr_country{SA}.rda")
save(db_dr_country, file = fn)


## NOTE HBC30 restriction for convenience
drhbc <- tout[
  iso3 %in% est[g.hbmdr == TRUE, unique(iso3)] &
    year >= cutyr,
  .(iso3, year,
    rr.prop, rr.prop.lo, rr.prop.hi,
    e_inc_rr_num = inc.rr,
    e_inc_rr_num_lo = pmax(0, inc.rr - 1.96 * inc.rr.sd),
    e_inc_rr_num_hi = inc.rr + 1.96 * inc.rr.sd
  )
]
drhbc <- merge(drhbc, KOD, by = c("iso3", "year"), all.x = TRUE, all.y = FALSE)
attr(drhbc, "timestamp") <- Sys.Date() # set date

fn <- gh("drtb/outdata/drhbc{SA}.rda")
save(drhbc, file = fn)


## -------------------- group-level db output

## Filename
## db_dr_group_yyyy-mm-dd.csv
## Structure
## group_type,                       (“g_whoregion”,  “global”, …)
## group_name,                     (“AFR”, “AMR”, …)
## year,
## e_rr_prop_new,
## e_rr_prop_new_lo,
## e_rr_prop_new_hi,
## e_rr_prop_new_se,
## e_rr_prop_ret,
## e_rr_prop_ret_lo,
## e_rr_prop_ret_hi,
## e_inc_rr_num,
## e_inc_rr_num_lo,
## e_inc_rr_num_hi,

NARM <- FALSE #dangerous if TRUE as sweeps over input problems: dev work only

## === global summaries
## -- totals
glob <- tout[year >= cutyr, .(
  inc = sum(inc.num, na.rm = NARM),
  inc.sd = ssum(inc.num.sd, na.rm = NARM),
  inc.new = sum(inc.new, na.rm = NARM),
  inc.new.sd = ssum(inc.new.sd, na.rm = NARM),
  inc.ret = sum(inc.ret, na.rm = NARM),
  inc.ret.sd = ssum(inc.ret.sd, na.rm = NARM),
  inc.rr = sum(inc.rr, na.rm = NARM),
  inc.rr.sd = ssum(inc.rr.sd, na.rm = NARM),
  inc.new.rr = sum(inc.new.rr, na.rm = NARM),
  inc.new.rr.sd = ssum(inc.new.rr.sd, na.rm = NARM),
  inc.ret.rr = sum(inc.ret.rr, na.rm = NARM),
  inc.ret.rr.sd = ssum(inc.ret.rr.sd, na.rm = NARM)
),
by = year
]

glob[,`:=` (
  inc.rr.lo = pmax(0, inc.rr - 1.96 * inc.rr.sd),
  inc.rr.hi = inc.rr + 1.96 * inc.rr.sd,
  inc.new.rr.lo = pmax(0, inc.new.rr - 1.96 * inc.new.rr.sd),
  inc.new.rr.hi = inc.new.rr + 1.96 * inc.new.rr.sd,
  inc.ret.rr.lo = pmax(0, inc.ret.rr - 1.96 * inc.ret.rr.sd),
  inc.ret.rr.hi = inc.ret.rr + 1.96 * inc.ret.rr.sd
)]


## --- proportions
glob[, c("prop.rr", "prop.new.rr", "prop.ret.rr") :=
  .(inc.rr / inc, inc.new.rr / inc.new, inc.ret.rr / inc.ret)]
glob[, prop.rr.sd := prop.rr * sqrt((inc.rr.sd / inc.rr)^2 + (inc.sd / inc)^2)]
glob[, prop.new.rr.sd := prop.new.rr * sqrt((inc.new.rr.sd / inc.new.rr)^2 +
  (inc.new.sd / inc.new)^2)]
glob[, prop.ret.rr.sd := prop.ret.rr * sqrt((inc.ret.rr.sd / inc.ret.rr)^2 +
  (inc.ret.sd / inc.ret)^2)]


glob[, `:=`(
  prop.rr.lo = pmax(0, prop.rr - 1.96 * prop.rr.sd),
  prop.rr.hi = pmin(1, prop.rr + 1.96 * prop.rr.sd),
  prop.new.rr.lo = pmax(0, prop.new.rr - 1.96 * prop.new.rr.sd),
  prop.new.rr.hi = pmin(1, prop.new.rr + 1.96 * prop.new.rr.sd),
  prop.ret.rr.lo = pmax(0, prop.ret.rr - 1.96 * prop.ret.rr.sd),
  prop.ret.rr.hi = pmin(1, prop.ret.rr + 1.96 * prop.ret.rr.sd)
)]


## -- totals
reg <- tout[year >= cutyr, .(
  inc = sum(inc.num, na.rm = NARM),
  inc.sd = ssum(inc.num.sd, na.rm = NARM),
  inc.new = sum(inc.new, na.rm = NARM),
  inc.new.sd = ssum(inc.new.sd, na.rm = NARM),
  inc.ret = sum(inc.ret, na.rm = NARM),
  inc.ret.sd = ssum(inc.ret.sd, na.rm = NARM),
  inc.rr = sum(inc.rr, na.rm = NARM),
  inc.rr.sd = ssum(inc.rr.sd, na.rm = NARM),
  inc.new.rr = sum(inc.new.rr, na.rm = NARM),
  inc.new.rr.sd = ssum(inc.new.rr.sd, na.rm = NARM),
  inc.ret.rr = sum(inc.ret.rr, na.rm = NARM),
  inc.ret.rr.sd = ssum(inc.ret.rr.sd, na.rm = NARM)
),
by = .(g_whoregion, year)
]

reg[, `:=`(
  inc.rr.lo = pmax(0, inc.rr - 1.96 * inc.rr.sd),
  inc.rr.hi = inc.rr + 1.96 * inc.rr.sd,
  inc.new.rr.lo = pmax(0, inc.new.rr - 1.96 * inc.new.rr.sd),
  inc.new.rr.hi = inc.new.rr + 1.96 * inc.new.rr.sd,
  inc.ret.rr.lo = pmax(0, inc.ret.rr - 1.96 * inc.ret.rr.sd),
  inc.ret.rr.hi = inc.ret.rr + 1.96 * inc.ret.rr.sd
)]



## --- proportions
reg[, c("prop.rr", "prop.new.rr", "prop.ret.rr") :=
  .(inc.rr / inc, inc.new.rr / inc.new, inc.ret.rr / inc.ret)]
reg[, prop.rr.sd := prop.rr * sqrt((inc.rr.sd / inc.rr)^2 + (inc.sd / inc)^2)]
reg[, prop.new.rr.sd := prop.new.rr * sqrt((inc.new.rr.sd / inc.new.rr)^2 +
  (inc.new.sd / inc.new)^2)]
reg[, prop.ret.rr.sd := prop.ret.rr * sqrt((inc.ret.rr.sd / inc.ret.rr)^2 +
  (inc.ret.sd / inc.ret)^2)]

reg[,`:=`(
  prop.rr.lo = pmax(0, prop.rr - 1.96 * prop.rr.sd),
  prop.rr.hi = pmin(1, prop.rr + 1.96 * prop.rr.sd),
  prop.new.rr.lo = pmax(0, prop.new.rr - 1.96 * prop.new.rr.sd),
  prop.new.rr.hi = pmin(1, prop.new.rr + 1.96 * prop.new.rr.sd),
  prop.ret.rr.lo = pmax(0, prop.ret.rr - 1.96 * prop.ret.rr.sd),
  prop.ret.rr.hi = pmin(1, prop.ret.rr + 1.96 * prop.ret.rr.sd)
)]


## --- join up
reg[, group_type := "g_whoregion"]
setnames(reg, "g_whoregion", "group_name")
glob[, group_type := "global"]
glob[, group_name := "global"]
db_dr_group <- rbind(reg, glob)
db_dr_group <- db_dr_group[year >= cutyr] # restrict by year
names(db_dr_group)

oldnmz <- c(
  "prop.new.rr",
  "prop.new.rr.lo",
  "prop.new.rr.hi",
  "prop.new.rr.sd",
  "prop.ret.rr",
  "prop.ret.rr.lo",
  "prop.ret.rr.hi",
  "inc.rr",
  "inc.rr.lo",
  "inc.rr.hi"
)
newnmz <- c(
  "e_rr_prop_new",
  "e_rr_prop_new_lo",
  "e_rr_prop_new_hi",
  "e_rr_prop_new_se",
  "e_rr_prop_ret",
  "e_rr_prop_ret_lo",
  "e_rr_prop_ret_hi",
  "e_inc_rr_num",
  "e_inc_rr_num_lo",
  "e_inc_rr_num_hi"
)
setnames(db_dr_group, oldnmz, newnmz)
attr(db_dr_group, "timestamp") <- Sys.Date() # set date

summary(db_dr_group)


## save out as Rdata
fn <- gh("drtb/output/")
if (!file.exists(fn)) dir.create(fn, recursive = TRUE)

fn <- gh("drtb/output/db_dr_group{SA}.rda")
save(db_dr_group, file = fn)


## =============== comparisons with older estimates
names(dr.est) <- gsub("_", ".", names(dr.est))

tmp <- dr.est[, .(
  iso3, year,
  e.rr.prop.new, e.rr.prop.new.lo, e.rr.prop.new.hi,
  e.rr.prop.ret, e.rr.prop.ret.lo, e.rr.prop.ret.hi,
  e.inc.rr.num, e.inc.rr.num.lo, e.inc.rr.num.hi
)]

tmp2 <- db_dr_country[, .(
  iso3, year,
  e_rr_prop_new, e_rr_prop_new_lo, e_rr_prop_new_hi,
  e_rr_prop_ret, e_rr_prop_ret_lo, e_rr_prop_ret_hi,
  e_inc_rr_num, e_inc_rr_num_lo, e_inc_rr_num_hi
)]

tmp <- merge(tmp, tmp2, by = c("iso3", "year"))


GP <- ggplot(
  tmp[year == 2019],
  aes(
    x = e_inc_rr_num, y = e.inc.rr.num,
    xmin = e_inc_rr_num_lo, ymin = e.inc.rr.num.lo,
    xmax = e_inc_rr_num_hi, ymax = e.inc.rr.num.hi,
    label = iso3
  )
) +
  scale_x_sqrt() +
  scale_y_sqrt() +
  xlab("RR incidence, new verion (sqrt scale)") +
  ylab("RR incidence, old verion (sqrt scale)") +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_errorbarh(height = 0) +
  geom_abline(slope = 1, intercept = 0, col = 2) +
  geom_text_repel() +
  theme_bw()
GP

ggsave(GP,
  file = gh("drtb/plots/CF.inc.RR.version{SA}.pdf"),
  w = 10, h = 10
)


GP <- ggplot(
  tmp[year == 2019],
  aes(
    x = e_rr_prop_new, y = e.rr.prop.new,
    xmin = e_rr_prop_new_lo, ymin = e.rr.prop.new.lo,
    xmax = e_rr_prop_new_hi, ymax = e.rr.prop.new.hi,
    label = iso3
  )
) +
  scale_x_sqrt() +
  scale_y_sqrt() +
  xlab("RR proportion, new verion (sqrt scale)") +
  ylab("RR proportion, old verion (sqrt scale)") +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_errorbarh(height = 0) +
  geom_abline(slope = 1, intercept = 0, col = 2) +
  geom_text_repel() +
  theme_bw()
GP

ggsave(GP,
  file = gh("drtb/plots/CF.newprop.RR.version{SA}.pdf"),
  w = 10, h = 10
)


GP <- ggplot(
  tmp[year == 2019],
  aes(
    x = e_rr_prop_ret, y = e.rr.prop.ret,
    xmin = e_rr_prop_ret_lo, ymin = e.rr.prop.ret.lo,
    xmax = e_rr_prop_ret_hi, ymax = e.rr.prop.ret.hi,
    label = iso3
  )
) +
  scale_x_sqrt() +
  scale_y_sqrt() +
  xlab("RR proportion, new verion (sqrt scale)") +
  ylab("RR proportion, old verion (sqrt scale)") +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_errorbarh(height = 0) +
  geom_abline(slope = 1, intercept = 0, col = 2) +
  geom_text_repel() +
  theme_bw()
GP

ggsave(GP,
  file = gh("drtb/plots/CF.retprop.RR.version{SA}.pdf"),
  w = 10, h = 10
)

## adapting PG's analysis to create the f & r parms for the DR incidence formula
rm(list = ls())

## libraries
library(here)
library(glue)
library(data.table)
library(metafor)
library(ggplot2)
library(ggrepel)
gh <- function(x) glue(here(x))

## utilities
source(here("drtb/R/utils/fun.R"))
source(here("import/load_gtb.R"))

noests.yet <- FALSE
## load data
tb <- load_gtb("tb",
  convert_dashes = TRUE, convert_data.table = TRUE
) # notifications
load(here("drtb/indata/rel.rda")) # TODO check to use same
if (noests.yet) {
  load(here("drtb/tmpdata/dre.rda")) # make from drhnew, drhnet
  ## xtra <- est[year==max(year)]
  ## xtra[,year:=year+1]
  ## est <- rbind(est,xtra)
  ## save(est,file=here('drtb/tmpdata/est.rda')) #NOTE old extended LOCF
  load(here("drtb/tmpdata/est.rda")) # NOTE old extended LOCF
} else {
  est <- load_gtb("est",
    convert_dashes = TRUE, convert_data.table = TRUE
  )
  ## make dre (used in PG version)
  Hn <- load_gtb("drhnew",
    convert_dashes = TRUE, convert_data.table = TRUE
  )
  Hr <- load_gtb("drhret",
    convert_dashes = TRUE, convert_data.table = TRUE
  )
  Hn[, row.rank.new := NULL]
  Hr[, row.rank.ret := NULL] # only thing not in dre
  ## most recent year
  Hn <- Hn[, .SD[year.new == max(year.new)], by = iso3]
  Hr <- Hr[, .SD[year.ret == max(year.ret)], by = iso3]
  ## setdiff(c(names(Hn),names(Hr)),names(dre))
  ## setdiff(names(dre),c(names(Hn),names(Hr)))
  ## missing countries
  dre <- merge(Hn, Hr, by = intersect(names(Hn), names(Hr)), all = TRUE)
  xtra <- data.table(iso3 = setdiff(est$iso3, dre$iso3))
  nnmz <- names(dre)
  nnmz <- nnmz[-1]
  xtra[, c(nnmz) := NA]
  dre <- rbind(dre, xtra)
  ## save(dre,file=here('drtb/tmpdata/dre.rda'))
}


## globals
yr <- max(est$year)      #NOTE
newversion <- TRUE #NOTE change this flag to either run old analysis, or new
## load chosen estimates
mdl <- scan(
  here("drtb/R/utils/modelchoice.txt"),
  what = "char"
) # NOTE depends choice
load(file = gh("drtb/outdata/KO.{mdl}.Rdata"))
## reformat
rr <- dcast(KO[year == yr],
  iso3 + year + type + g_whoregion ~ patients,
  value.var = c("RR.mid", "RR.lo", "RR.hi")
)
oldnmz <- c(
  "g_whoregion",
  "RR.mid_new", "RR.mid_ret",
  "RR.lo_new", "RR.lo_ret",
  "RR.hi_new", "RR.hi_ret"
)
newnmz <- c(
  "g.whoregion",
  "prop.rr.new", "prop.rr.ret",
  "prop.rr.new.lo", "prop.rr.ret.lo",
  "prop.rr.new.hi", "prop.rr.ret.hi"
)
setnames(rr, oldnmz, newnmz)
## convert from percent to proportion
xcols <- newnmz[-1]
rr[, (xcols) := lapply(.SD, function(x) x / 1e2), .SDcols = xcols]
rr[, c("prop.rr.new.sd", "prop.rr.ret.sd") :=
  .(
    (prop.rr.new.hi - prop.rr.new.lo) / 3.92,
    (prop.rr.ret.hi - prop.rr.ret.lo) / 3.92
  )]




## Before 2013, relapses were collected in old variables:
## c.ret = ret.nrel + (ret.rel+ret.taf+ret.tad+ret.oth)
## For 2013-2024,
## c.ret = ret.nrel + (ret.rel.ep + ret.rel.clindx + ret.rel.labconf)
## Since 2025 (and the new disaggregation data collection)
## c.ret = ret.nrel +( ret_rel_labconf + ret_rel_clindx + ret_rel_ep )
rrtb <- tb[
  ,
  .(iso3, year, ret.nrel,
    c.ret = fcase(
      year < 2012,
      ret.nrel + (ret.rel + ret.taf + ret.tad + ret.oth),
      year >= 2013 & year < 2025,
      ret.nrel + (ret.rel.ep + ret.rel.clindx + ret.rel.labconf),
      year >= 2025,
      ret.nrel + (ret.rel.labconf + ret.rel.clindx + ret.rel.ep)
    )
  )
]
rrtb <- rrtb[year > yr - 5]
rrtb2 <- est[year > yr - 5, .(iso3, year, c.newinc)]
rrtb <- merge(rrtb, rrtb2, by = c("iso3", "year"), all.x = TRUE)

setkey(rrtb, iso3, year)
rrtb[, ret.rel := c.ret - ret.nrel] # relapse
rrtb[!is.na(ret.rel), test.ispos(ret.rel)] # check


## --- f ---
rrtb[, f := ret.nrel / c.newinc] #definition of f

## safeties
rrtb[c.newinc == 0, f := 1]
rrtb[c.newinc < c.ret, f := 1]
rrtb[!is.na(f) & (f > 1 | f < 0)] # TODO BEL 2023/2024 odd

tb[
  iso3 == "BEL" & year %in% c(2023, 2024),
  .(iso3, year, ret.nrel, ret.rel.ep, ret.rel.clindx, ret.rel.labconf)
]

rrtb[!is.na(f) & (f > 1 | f < 0), f:=1] # TODO FIX for now
rrtb[!is.na(f), test.isbinom(f)]

## --- r ---
rrtb[
  f < 1,
  r :=
    pmin(
      (ret.rel / c.newinc) / (1 - f), # definition
      1
    ) # safety
]
rrtb[f == 1, r := 0] #safety

## check
rrtb[!is.na(r), test.isbinom(r)]

## collect f & r values by country
out <- rrtb[, .(
  r = mean(r, na.rm = TRUE),
  r.sd = sd(r, na.rm = TRUE),
  f = mean(f, na.rm = TRUE),
  f.sd = sd(f, na.rm = TRUE)
), by = iso3]
summary(out) #very few NAs

## checks
out[!is.na(r), test.ispos(r)]
out[!is.na(r.sd), test.ispos(r.sd)]
out[!is.na(f), test.ispos(f)]
out[!is.na(f.sd), test.ispos(f.sd)]

## where SD is missing, assume 20% of mean
out[is.na(r.sd) & !is.na(r), r.sd := r * .2]
out[is.na(f.sd) & !is.na(f), f.sd := f * .2]

## uniformize NAs
out[!is.finite(r), r := NA]
out[!is.finite(r.sd), r.sd := NA]
out[!is.finite(f), f := NA]
out[!is.finite(f.sd), f.sd := NA]
summary(out) #very few NAs

## imputation - pool r and f by mdr region
gmdr <- est[year == yr, .(iso3, g.mdr = g.whoregion)]
FSU <- c(
  "ARM", "AZE", "BLR", "GEO", "KAZ",
  "KGZ", "MDA", "TJK", "TKM", "UKR", "UZB"
)
gmdr[iso3 %in% FSU, g.mdr := "EEU"] #to match previous year MDR grouping
out <- merge(out, gmdr, by = "iso3")

fit.r <- rma(
  yi = r,
  sei = r.sd,
  data = out[r.sd > 0],
  mods =  ~ g.mdr - 1
)
fit.f <- rma(
  yi = f,
  sei = f.sd,
  data = out[f.sd > 0],
  mods =  ~ g.mdr - 1
)

imp.r <- fit.r$b[, 1]
imp.f <- fit.f$b[, 1]
imp.r.sd <- fit.r$se
imp.f.sd <- fit.f$se

## check the order of regions in reg is the same as in imp*
reg <- names(table(gmdr$g.mdr))
all.equal(reg, gsub("g.mdr", "", dimnames(fit.r$beta)[[1]]))
all.equal(reg, gsub("g.mdr", "", dimnames(fit.f$beta)[[1]]))


## out[, g.mdr := NULL]
if ("g.mdr" %in% names(rr)) rr[, g.mdr := NULL]
rr <- merge(rr, out, by = "iso3", all.x = TRUE)


## add in imputed f/r - regional MAs
table(!is.na(rr$r))
table(!is.na(rr$f))
for (i in seq_along(reg)) {
  sel <- rr$g.mdr == reg[i] & (is.na(rr$r) | rr$r == 0)
  rr[sel, r := imp.r[i]]
  rr[sel, r.sd := imp.r.sd[i]]
  sel <- rr$g.mdr == reg[i] & (is.na(rr$f) | rr$f == 0)
  rr[sel, f := imp.f[i]]
  rr[sel, f.sd := imp.f.sd[i]]
}


## check!
table(!is.na(rr$r))
table(!is.na(rr$f))

## --- rho ---
## $\rho$ and its SD
## (see rel.R)
rho <- 5.5 # reverting to last year's after USAID discussions
rho.sd <- (6.8 - 4.4) / 3.92
rho.cv <- rho.sd / rho

## use the relapse dataset
rel2 <- rel[level == "national",
  .(
    rho.rr = last(ratio.rel.new),
    rho.sd.rr = last(ratio.rel.new.sd)
  ),
  by = iso3
]


(rel3 <- rel2[
  rho.rr <= rho & rho.rr > 1
]) # 15 countries with their own sensible value
rr <- merge(rr, rel3, by = "iso3", all.x = TRUE) # use these for preference


## but check that these results are sensible & modify otherwise
rr[
  !is.na(rho.rr),
  rho.rr := ifelse(prop.rr.ret / prop.rr.new > rho.rr,
    rho.rr,
    prop.rr.ret / prop.rr.new
  )
]


## for the countries not in rel use rho, but with same sense-corrections
rr[
  is.na(rho.rr),
  rho.rr := ifelse(prop.rr.ret / prop.rr.new > rho,
    rho,
    prop.rr.ret / prop.rr.new
  )
]
rr[is.na(rho.sd.rr), rho.sd.rr := rho.rr * rho.cv]


## keep what we need:
rhofnr <- rr[
  ,
  .(iso3, rho = rho.rr, rho.sd = rho.sd.rr, f, f.sd, r, r.sd)
] # NOTE rename
summary(rhofnr)

## a few small places missing
rhofnr[is.na(r)]
(missfr <- rhofnr[is.na(r), iso3])
(missall <- setdiff(est$iso3, rhofnr$iso3))

## regional averages to complete
rhofnr <- merge(
  rhofnr,
  est[, .(inc.num = mean(inc.num)), by = .(iso3, g.whoregion)],
  by = "iso3"
)

rav <- rhofnr[!is.na(r), .(
  rho = weighted.mean(rho, w = inc.num),
  rho.sd = weighted.mean(rho.sd, w = inc.num),
  f = weighted.mean(f, w = inc.num),
  f.sd = weighted.mean(f.sd, w = inc.num),
  r = weighted.mean(r, w = inc.num),
  r.sd = weighted.mean(r.sd, w = inc.num)
), by = g.whoregion]
setkey(rav, g.whoregion)

rhofnr[iso3 %in% missfr,
  c("f", "f.sd", "r", "r.sd") := .(
    rav[.BY$g.whoregion, f],
    rav[.BY$g.whoregion, f.sd],
    rav[.BY$g.whoregion, r],
    rav[.BY$g.whoregion, r.sd]
  ),
  by = g.whoregion
]

## CHECK
rhofnr[iso3 %in% missfr]
rav

## completely missing:
xtra <- unique(est[iso3 %in% missall, .(iso3, g.whoregion)])
xtra <- cbind(
  xtra,
  rav[xtra, on = "g.whoregion"][, .(rho, rho.sd, f, f.sd, r, r.sd)]
)

## add in
rhofnr[, inc.num := NULL]
rhofnr <- rbind(rhofnr, xtra)
rhofnr[, g.whoregion := NULL]

save(rhofnr, file = here("drtb/data/rhofnr.Rdata"))

## === compare
load(file = here("drtb/data/rhofnr.25.Rdata"))
rhofnr.old <- copy(rhofnr)

load(file = here("drtb/data/rhofnr.Rdata"))

## comparison test with 'old' version
ii <- 2:ncol(rhofnr.old)
names(rhofnr.old)[ii] <- paste0(names(rhofnr.old)[ii], ".old")
CF <- merge(rhofnr, rhofnr.old, by = "iso3")
CFM <- melt(CF, id = "iso3")
CFM[, vn := ifelse(grepl("old", variable), "old", "new")]
CFM[, variable := gsub("\\.old", "", variable)]
CFM <- dcast(CFM, iso3 + variable ~ vn)
CFM[abs(new / old - 1) > 0.1]
CFM[, summary(abs(new / old - 1))]

## plot
ggplot(CFM[!grepl("rho", variable)], aes(old, new, label = iso3)) +
  geom_point() +
  geom_text_repel() +
  facet_wrap(~variable) +
  geom_abline(intercept = 0, slope = 1, col = 2)


ggsave(here("drtb/plots/CF.png"), w = 7, h = 7)
CFM[variable == "rho" & abs(new / old - 1) > 0.1] # CHECK



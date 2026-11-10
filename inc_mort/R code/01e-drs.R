### Create drnew drret

rm(list=ls())

library(data.table)
library(here)
library(dplyr)

source(here("import/load_gtb.R"))

drs <- load_gtb("drs",convert_dashes = TRUE)
tb <- load_gtb("tb",convert_dashes = TRUE)

setDT(drs)
setDT(tb)

# Load 2025 dateset
load(here("inc_mort/estimates2025/drret.rda"))
load(here("inc_mort/estimates2025/drnew.rda"))

load(here("inc_mort/estimates2025/drhret.rda"))
load(here("inc_mort/estimates2025/drhnew.rda"))

load(here("inc_mort/estimates2025/drfq.rda"))


setDT(drnew)

# Drop Nepal data on surveillance in drnew and drret
drnew <- drnew[!(iso3 == "NPL" & year.new == 2021 & source.new == "Surveillance")]
drret <- drret[!(iso3 == "NPL" & year.ret == 2024 & source.ret == "Surveillance")]





#Variable from DRS
existing_cols <- names(drs)

#Variable list from drnew
drnew_variable_list <- list(drnew_vars = colnames(drnew))
full_list <- drnew_variable_list$drnew_vars
clean_list_drnew <- grep("pcnt", full_list, value = TRUE, invert = TRUE)

drret_variable_list <- list(drret_vars = colnames(drret))
full_list <- drret_variable_list$drret_vars
clean_list_drret<- grep("pcnt", full_list, value = TRUE, invert = TRUE)



### 2025 data in drs and recalculating variables

# New cases

drnew_2025 <- drs[year==2025,.(iso3,country,year.new=year,g.whoregion,
                               dst.rlt.new=dst.new.rr.hr+dst.new.rr.hs+dst.new.rs.hr+dst.new.rs.hs,
                               dst.rlt.hr.new=dst.new.rr.hr+dst.new.rs.hr,
                               dst.rlt.rr.new=dst.new.rr.hs+dst.new.rr.hr,
                               mdr.new=dst.new.rr.hr,
                               dr.r.nh.new, xpert.new,
                               xpert.dr.r.new, 
                               r.rlt.new=r.rlt.dst.new,
                               rr.new=rr.dst.new
                          )]

drnew_2025[, source.new:="Surveillance"]
drnew_2025[, area.desc.new:=NA]
drnew_2025[, all.areas.covered.new:=1]


#' Quality
#' two criteria must be met: 
#' (i) at least 80% of notified new pulmonary bacteriologically-confirmed TB cases must have 
#' a documented drug susceptibility testing result for at least rifampicin, 
#' (ii) the proportion of pulmonary bacteriologically-confirmed cases having 
#' unknown previous treatment history must be no more than 25%.

drnew_2025 <- merge(drnew_2025, tb[year == 2025, .(iso3, pulm.labconf.new, pulm.labconf.rec,pulm.labconf.pulm.rereg, pulm.labconf.unk)],
                    by = "iso3")

drnew_2025[, total_pulm := pulm.labconf.new +  pulm.labconf.unk]

drnew_2025[, `:=`(
  cov.dr.new   = r.rlt.new / total_pulm,
  prop.unk.new = pulm.labconf.unk / total_pulm
)]

copy_drnew_2025 <- copy(drnew_2025)


drnew_2025[cov.dr.new >= 0.8 & prop.unk.new < 0.25, surv.quality.new := "A"]
drnew_2025 <- drnew_2025[surv.quality.new == "A"]

drnew_2025[, c("pulm.labconf.new", "pulm.labconf.rec", "pulm.labconf.unk",  "pulm.labconf.pulm.rereg",
               "cov.dr.new", "prop.unk.new", "total_pulm") := NULL]


# Merge with previous years

drnew_merge <- rbind(drnew,drnew_2025,fill = TRUE)


# ad-hoc add Somalia results
new_row <- data.table(
  country = "Somalia",
  iso3 = "SOM",
  g.whoregion = "EMR",
  year.new = 2025,
  source.new = "Survey",
  all.areas.covered.new = 1,
  rr.new.pcnt = 2.6,
  rr.new.pcnt.lo = 1.4,
  rr.new.pcnt.hi = 4.7
)

drnew_merge <- rbind(drnew_merge, new_row, fill = TRUE)


# ad-hoc add Nigeria results
new_row <- data.table(
  country = "Nigeria",
  iso3 = "NGA",
  g.whoregion = "AFR",
  year.new = 2024,
  source.new = "Survey",
  all.areas.covered.new = 1,
  rr.new.pcnt = 2.9,
  rr.new.pcnt.lo = 2.0,
  rr.new.pcnt.hi = 4.2
)

drnew_merge <- rbind(drnew_merge, new_row, fill = TRUE)


# Ret

drret_2025 <- drs[year==2025,.(iso3,country,year.ret=year,g.whoregion,
                               dst.rlt.ret=dst.ret.rr.hr+dst.ret.rr.hs+dst.ret.rs.hr+dst.ret.rs.hs,
                               dst.rlt.hr.ret=dst.ret.rr.hr+dst.ret.rs.hr,
                               dst.rlt.rr.ret=dst.ret.rr.hs+dst.ret.rr.hr,
                               mdr.ret=dst.ret.rr.hr,
                               dr.r.nh.ret, xpert.ret,
                               xpert.dr.r.ret, 
                               r.rlt.ret=r.rlt.dst.ret,
                               rr.ret=rr.dst.ret
)]

drret_2025[, source.ret:="Surveillance"]
drret_2025[, area.desc.ret:=NA]
drret_2025[, all.areas.covered.ret:=1]


#' Quality
#' 1 criteria must be met: 
#' (i) at least 80% of notified new pulmonary bacteriologically-confirmed TB cases must have 
#' a documented drug susceptibility testing result for at least rifampicin, 


drret_2025 <- merge(drret_2025, tb[year == 2025, .(iso3, pulm.labconf.new, pulm.labconf.rec,pulm.labconf.pulm.rereg, pulm.labconf.unk)],
                    by = "iso3")

drret_2025[, total_pulm := pulm.labconf.rec + pulm.labconf.pulm.rereg]

drret_2025[, `:=`(
  cov.dr.ret   = r.rlt.ret / total_pulm,
  prop.unk.ret = pulm.labconf.unk / total_pulm
)]

copy_drret_2025 <- copy(drret_2025)

drret_2025[cov.dr.ret >= 0.8 , surv.quality.ret := "A"]
drret_2025 <- drret_2025[surv.quality.ret == "A"]

drret_2025[, c("pulm.labconf.new", "pulm.labconf.rec", "pulm.labconf.unk", 
               "cov.dr.ret", "prop.unk.ret", "total_pulm") := NULL]


# Merge with previous years

drret_merge <- rbind(drret,drret_2025,fill = TRUE)


# ad-hoc add Somalia results
new_row <- data.table(
  country = "Somalia",
  iso3 = "SOM",
  g.whoregion = "EMR",
  year.ret = 2025,
  source.ret = "Survey",
  all.areas.covered.ret = 1,
  rr.ret.pcnt = 3.2,
  rr.ret.pcnt.lo = 0.5,
  rr.ret.pcnt.hi = 19.7
)

drret_merge <- rbind(drret_merge, new_row, fill = TRUE)


# ad-hoc add Nigeria results
new_row <- data.table(
  country = "Nigeria",
  iso3 = "NGA",
  g.whoregion = "AFR",
  year.ret = 2024,
  source.ret = "Survey",
  all.areas.covered.ret = 1,
  rr.ret.pcnt = 15.6,
  rr.ret.pcnt.lo = 10.0,
  rr.ret.pcnt.hi = 23.4
)

drret_merge <- rbind(drret_merge, new_row, fill = TRUE)




### H resistance

# Criteria for data to be included
# dst.rlt.new/r.rlt.new>=0.8

#New
drhnew_2025 <- copy(copy_drnew_2025)
drhnew_2025[dst.rlt.new/r.rlt.new >= 0.8 , surv.quality.new := "A"]
drhnew_2025 <- drhnew_2025[surv.quality.new == "A"]

drhnew_2025[, c("pulm.labconf.new", "pulm.labconf.rec", "pulm.labconf.unk",  "pulm.labconf.pulm.rereg",
               "cov.dr.new", "prop.unk.new", "total_pulm") := NULL]

# Merge with previous years

drhnew_merge <- rbind(drhnew,drhnew_2025,fill = TRUE)

#Ret
drhret_2025 <- copy(copy_drret_2025)
drhret_2025[dst.rlt.ret/r.rlt.ret >= 0.8 , surv.quality.ret := "A"]
drhret_2025 <- drhret_2025[surv.quality.ret == "A"]

drhret_2025[, c("pulm.labconf.new", "pulm.labconf.rec", "pulm.labconf.unk", 
                "cov.dr.ret", "prop.unk.ret", "total_pulm") := NULL]

# Merge with previous years

drhret_merge <- rbind(drhret,drhret_2025,fill = TRUE)



#' 
#' 
#' # Ret
#' 
#' drret_2025 <- drs[year==2025,.(iso3,country,year.ret=year,g.whoregion,
#'                                dst.rlt.ret=dst.ret.rr.hr+dst.ret.rr.hs+dst.ret.rs.hr+dst.ret.rs.hs,
#'                                dst.rlt.hr.ret=dst.ret.rr.hr+dst.ret.rs.hr,
#'                                dst.rlt.rr.ret=dst.ret.rr.hs+dst.ret.rr.hr,
#'                                mdr.ret=dst.ret.rr.hr,
#'                                dr.r.nh.ret, xpert.ret,
#'                                xpert.dr.r.ret, 
#'                                r.rlt.ret=r.rlt.dst.ret,
#'                                rr.ret=rr.dst.ret
#' )]
#' 
#' drret_2025[, source.ret:="Surveillance"]
#' drret_2025[, area.desc.ret:=NA]
#' drret_2025[, all.areas.covered.ret:=1]
#' 
#' 
#' #' Quality
#' #' 1 criteria must be met: 
#' #' (i) at least 80% of notified new pulmonary bacteriologically-confirmed TB cases must have 
#' #' a documented drug susceptibility testing result for at least rifampicin, 
#' 
#' 
#' drret_2025 <- merge(drret_2025, tb[year == 2025, .(iso3, pulm.labconf.new, pulm.labconf.rec,pulm.labconf.pulm.rereg, pulm.labconf.unk)],
#'                     by = "iso3")
#' 
#' drret_2025[, total_pulm := pulm.labconf.new + pulm.labconf.rec + pulm.labconf.unk + pulm.labconf.pulm.rereg]
#' 
#' drret_2025[, `:=`(
#'   cov.dr.ret   = r.rlt.ret / total_pulm,
#'   prop.unk.ret = pulm.labconf.unk / total_pulm
#' )]
#' 
#' copy_drret_2025 <- copy(drret_2025)
#' 
#' drret_2025[cov.dr.ret >= 0.8 , surv.quality.ret := "A"]
#' drret_2025 <- drret_2025[surv.quality.ret == "A"]
#' 
#' drret_2025[, c("pulm.labconf.new", "pulm.labconf.rec", "pulm.labconf.unk", 
#'                "cov.dr.ret", "prop.unk.ret", "total_pulm") := NULL]
#' 
#' 
#' # Merge with previous years
#' 
#' drret_merge <- rbind(drret,drret_2025,fill = TRUE)
#' 
#' 




### FQ


# New cases

drfq_2025 <- drs[year==2025,.(iso3,country,year,g.whoregion,
                              rr.dr.fq, rr.dst.rlt, rr.fqr,
                              rr.dst.rlt.fq, mdr.dr.fq,mdr.dst.rlt
)]

drfq_2025[, drs.record.type:="Surveillance"]
drfq_2025[, all.areas.covered.new:=1]

# Merge with previous years

drfq_merge <- rbind(drfq,drfq_2025,fill = TRUE)


# Save files

rm(drnew,drret,drhnew,drhret, drfq)

drnew <- drnew_merge
drret <- drret_merge
drhnew <- drhnew_merge
drhret <- drhret_merge
drfq <- drfq_merge


attr(drnew, "timestamp") <- Sys.Date()
save(drnew,file=here("data/gtb/other/drnew.rda"))

attr(drret, "timestamp") <- Sys.Date()
save(drret,file=here("data/gtb/other/drret.rda"))

attr(drhnew, "timestamp") <- Sys.Date()
save(drhnew,file=here("data/gtb/other/drhnew.rda"))

attr(drhret, "timestamp") <- Sys.Date()
save(drhret,file=here("data/gtb/other/drhret.rda"))

attr(drfq, "timestamp") <- Sys.Date()
save(drfq,file=here("data/gtb/other/drfq.rda"))

### END


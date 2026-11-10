# Export for WIDP profile

rm(list=ls())

library(data.table)
library(here)
load(here("inc_mort/analysis/est.rda"))

load(here("disaggregation/output/db_estimates_country_all.Rdata"))
load(here("drtb/output/db_dr_country.rda"))
load(here("disaggregation/output/db_estimates_country_all.Rdata"))
load(here("inc_mort/analysis/attributable_cases.rda"))
load(here("data/gtb/other/pop.rda"))
load(here("data/gtb/other/drnew.rda"))
load(here("data/gtb/other/drret.rda"))
load(here("inc_mort/analysis/est.method.rda"))




setDT(pop)

`%ni%` <- Negate(`%in%`)

#Add milestone 2025
ref_2015 <- est[year == 2015, .(iso3, 
                                inc_ref = inc, 
                                mort_ref = mort.num)]

est[ref_2015, on = "iso3", 
    `:=`(inc_milestone = 0.5 * i.inc_ref, 
         mort_milestone = 0.25 * i.mort_ref)]

### Export main estimates

test.est <- est[,.(iso3,year,inc,inc.lo,inc.hi,
                              inc.num,inc.num.lo=inc.lo.num,inc.num.hi=inc.hi.num,
                              inc.h,inc.h.lo,inc.h.hi,
                              inc.h.num,inc.h.num.lo=inc.h.lo.num,inc.h.num.hi=inc.h.hi.num,
                              inc.nh,inc.nh.lo,inc.nh.hi,
                              inc.nh.num,inc.nh.num.lo=inc.nh.lo.num,inc.nh.num.hi=inc.nh.hi.num,
                              mort,mort.lo,mort.hi,
                              mort.num,mort.num.lo=mort.lo.num,mort.num.hi=mort.hi.num,
                              mort.h,mort.h.lo,mort.h.hi,
                              mort.h.num,mort.h.num.lo=mort.h.lo.num,mort.h.num.hi=mort.h.hi.num,
                              mort.nh,mort.nh.lo,mort.nh.hi,
                              mort.nh.num,mort.nh.num.lo=mort.nh.lo.num,mort.nh.num.hi=mort.nh.hi.num,
                              cfr, cfr.lo, cfr.hi,
                              inc_milestone, mort_milestone
                              )]


test.agesex <- db_estimates_country_all[year==2025 & sex != "a" & age_group %in% c("0-4","5-9","10-14","15-19","20-24","25-34",
                                                         "35-44","45-54","55-64","65plus") , 
                                                                       .(iso3,year,
                                                                         age_group,sex,
                                                                         inc.num=best,inc.num.lo=lo,inc.num.hi=hi)]



# DR

#Estimate
db_dr_country <- merge(db_dr_country,pop[,.(iso3,year,e_pop_num)],by=c("iso3","year"),all.x=T)

test.rr <- db_dr_country[, .(iso3, year, 
                                          e_rr_prop_new, e_rr_prop_new_lo,e_rr_prop_new_hi,
                                          e_rr_prop_ret,e_rr_prop_ret_lo,e_rr_prop_ret_hi,
                                          e_inc_rr_num,e_inc_rr_num_lo,e_inc_rr_num_hi,
                                          e_inc_rr=e_inc_rr_num*1e5/e_pop_num,
                                          e_inc_rr_lo=e_inc_rr_num_lo*1e5/e_pop_num,
                                          e_inc_rr_hi=e_inc_rr_num_hi*1e5/e_pop_num)]


duplicates <- test.rr[duplicated(test.rr, by = c("iso3", "year")) | 
                        duplicated(test.rr, by = c("iso3", "year"), fromLast = TRUE)]

print(duplicates[order(iso3, year)])
test.rr <- unique(test.rr, by = c("iso3", "year"))

#Survey and surveillance data points

test.o.new <- drnew[, .(iso3,year=year.new,source.new,o_rr_prop_new=rr.new/r.rlt.new,
                                    s_rr_prop_new=fcoalesce(rr.new.pcnt, mdr.new.pcnt))]

test.o.new[, source_type := {
  has_surv <- "Surveillance" %in% source.new
  has_survey <- "Survey" %in% source.new
  
  if (has_surv && has_survey) {
    3
  } else if (has_survey) {
    2
  } else if (has_surv) {
    1
  } else {
    NA
  }
}, by = iso3]



source.new <- setorder(test.o.new, iso3, -year)[, .SD[1], by = iso3, .SDcols = "source_type"]
setnames(source.new, "source_type", "source_type_new")





test.o.ret <- drret[, .(iso3,year=year.ret,source.ret,o_rr_prop_ret=rr.ret/r.rlt.ret,
                                     s_rr_prop_ret=fcoalesce(rr.ret.pcnt, mdr.ret.pcnt))]

test.o.ret[, source_type := {
  has_surv <- "Surveillance" %in% source.ret
  has_survey <- "Survey" %in% source.ret
  
  if (has_surv && has_survey) {
    3
  } else if (has_survey) {
    2
  } else if (has_surv) {
    1
  } else {
    NA
  }
}, by = iso3]


source.ret<- setorder(test.o.ret, iso3, -year)[, .SD[1], by = iso3, .SDcols = "source_type"]
setnames(source.ret, "source_type", "source_type_ret")

# Merge source data to main export
# 9 if no sources abd using regional average

test.rr <- merge(test.rr,source.new,by="iso3",all.x=T)
test.rr <- merge(test.rr,source.ret,by="iso3",all.x=T)

test.rr[is.na(source_type_new), source_type_new := 9]
test.rr[is.na(source_type_ret), source_type_ret := 9]



### Att cases

test.att <- att[,.(iso3,
                   inc.at.und.num=inc.at.und.num.new,inc.at.und.num.lo=inc.at.und.lo.num.new,inc.at.und.num.hi=inc.at.und.hi.num.new,
                   inc.at.dia.num.new,inc.at.dia.num.lo=inc.at.dia.lo.num.new,inc.at.dia.num.hi=inc.at.dia.hi.num.new,
                   inc.at.hiv.num,inc.at.hiv.num.lo=inc.at.hiv.lo.num,inc.at.hiv.num.hi=inc.at.hiv.hi.num,
                   inc.at.smk.num,inc.at.smk.num.lo=inc.at.smk.lo.num,inc.at.smk.num.hi=inc.at.smk.hi.num,
                   inc.at.alc.num,inc.at.alc.num.lo=inc.at.alc.lo.num,inc.at.alc.num.hi=inc.at.alc.hi.num
                   )]


### Method (to be reviewed later)
test.est.method <- est.method




# Underscore in var name
list_of_data <- list(test.est = test.est, test.agesex = test.agesex, test.att = test.att,
                     test.o.new = test.o.new, test.o.ret = test.o.ret, test.est.method = test.est.method)

processed_list <- lapply(list_of_data, function(dt) {
  
  old_names <- names(dt)
  new_names <- gsub("(.*)\\.(lo|hi)\\.num$", "\\1.num.\\2", old_names)
  
  new_names <- gsub(".", "_", new_names, fixed = TRUE)
  
  setnames(dt, old = old_names, new = new_names)
  
  return(dt)
})

list2env(processed_list, envir = .GlobalEnv)


### Blank few countries : PRK

target_countries <- c("PRK")

list_of_data <- list(test.est = test.est, test.agesex = test.agesex, test.att = test.att,
                     test.o.new = test.o.new, test.o.ret = test.o.ret,
                     test.rr = test.rr, test.est.method = test.est.method)


lapply(list_of_data, function(dt) {
  
  cols_to_blank <- setdiff(names(dt), c("iso3", "year"))
  
  dt[iso3 %in% target_countries, (cols_to_blank) := NA]
  
  return(invisible(dt))
})

list2env(list_of_data, envir = .GlobalEnv)


#Save as csv

target_dir <- here("inc_mort/analysis/profile_widp")


list_of_data <- list(
  main_est    = test.est,
  main_agesex = test.agesex,
  main_rr     = test.rr,
  main_att    = test.att,
  main_o_new  = test.o.new,
  main_o_ret  = test.o.ret,
  main_est_method = test.est.method
)

lapply(names(list_of_data), function(name) {
  
  file_path <- file.path(target_dir, paste0(name, ".csv"))
  
  fwrite(list_of_data[[name]], file = file_path)
  
  message(paste("Successfully saved:", file_path))
})


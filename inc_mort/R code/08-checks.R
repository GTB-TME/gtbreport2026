### 
### Global TB report 2026
### Final check file
### 26/06/2026
### Author: Mathieu Bastard
###


# Checking estimates for all countries are available

yr=2025
load(here("inc_mort/analysis/est.rda"))


### Missing data on incidence data
print(paste("Missing data on incidence estimate:", sum(is.na(est$inc))))
print(paste("Missing data on incidence SE estimate:", sum(is.na(est$inc.sd))))
print(paste("Missing data on incidence bounds:", sum(is.na(est$inc.lo)),sum(is.na(est$inc.hi))))

print(paste("Missing data on incidence HIV+ estimate:", sum(is.na(est$inc.h) & !is.na(est$tbhiv))))
print(paste("Missing data on incidence HIV+ SD estimate:", sum(is.na(est$inc.h.sd) & !is.na(est$tbhiv))))
print(paste("Missing data on incidence bounds:", sum(is.na(est$inc.h.lo) & !is.na(est$tbhiv)),sum(is.na(est$inc.h.hi) & !is.na(est$tbhiv))))

print(paste("Missing data on incidence HIV- estimate:", sum(is.na(est$inc.nh) & !is.na(est$tbhiv))))
print(paste("Missing data on incidence HIV- SD estimate:", sum(is.na(est$inc.nh.sd) & !is.na(est$tbhiv))))
print(paste("Missing data on incidence bounds:", sum(is.na(est$inc.nh.lo) & !is.na(est$tbhiv)),sum(is.na(est$inc.nh.hi) & !is.na(est$tbhiv))))


### Missing data on mortality data
print(paste("Missing data on mortality estimate:", sum(is.na(est$mort))))
print(paste("Missing data on mortality SE estimate:", sum(is.na(est$mort.sd))))
print(paste("Missing data on mortality bounds:", sum(is.na(est$mort.lo)),sum(is.na(est$mort.hi))))

print(paste("Missing data on mortality HIV+ estimate:", sum(is.na(est$mort.h) & !is.na(est$tbhiv))))
print(paste("Missing data on mortality HIV+ SD estimate:", sum(is.na(est$mort.h.sd) & !is.na(est$tbhiv))))
print(paste("Missing data on mortality bounds:", sum(is.na(est$mort.h.lo) & !is.na(est$tbhiv)),sum(is.na(est$mort.h.hi) & !is.na(est$tbhiv))))

print(paste("Missing data on mortality HIV- estimate:", sum(is.na(est$mort.nh) & !is.na(est$tbhiv))))
print(paste("Missing data on mortality HIV- SD estimate:", sum(is.na(est$mort.nh.sd) & !is.na(est$tbhiv))))
print(paste("Missing data on mortality bounds:", sum(is.na(est$mort.nh.lo) & !is.na(est$tbhiv)),sum(is.na(est$mort.nh.hi) & !is.na(est$tbhiv))))


### Missing data on CFR in year Y

print(paste("Missing data on CFR estimate:", sum(is.na(est$cfr) & est$inc>1e-5 & est$year==yr)))
print(paste("Missing data on CFR SE estimate:", sum(is.na(est$cfr.sd & est$inc>1e-5 & est$year==yr))))
print(paste("Missing data on CFR bounds:", sum(is.na(est$cfr.lo) & est$inc>1e-5 & est$year==yr),sum(is.na(est$cfr.hi) & est$inc>1e-5 & est$year==yr)))



### Missing data on attributable cases

load(here("inc_mort/analysis/attributable_cases.rda"))

# Countries having at least one estimate of attributable cases

att.lst=att[,unique(iso3)]
print(paste("Number of countries with at least one attributable cases RF estimated:",length(att.lst),"/215"))

risk_factors <- list(
  list(name = "HIV",         num_var = "inc.at.hiv.num",     lo_var = "inc.at.hiv.lo.num",     hi_var = "inc.at.hiv.hi.num"),
  list(name = "Diabetes",    num_var = "inc.at.dia.num.new", lo_var = "inc.at.dia.lo.num.new", hi_var = "inc.at.dia.hi.num.new"),
  list(name = "Alcohol",     num_var = "inc.at.alc.num",     lo_var = "inc.at.alc.lo.num",     hi_var = "inc.at.alc.hi.num"),
  list(name = "Smoking",     num_var = "inc.at.smk.num",     lo_var = "inc.at.smk.lo.num",     hi_var = "inc.at.smk.hi.num"),
  list(name = "Undernutrition", num_var = "inc.at.und.num.new", lo_var = "inc.at.und.lo.num.new", hi_var = "inc.at.und.hi.num.new")
)

for (factor_info in risk_factors) {
  
  num_col <- factor_info$num_var
  lo_col <- factor_info$lo_var
  hi_col <- factor_info$hi_var
  factor_name <- factor_info$name
  
  att_lst <- att[!is.na(get(num_col)), unique(iso3)]
  
  lo_bounds_count <- att[iso3 %in% att_lst, sum(!is.na(get(lo_col)))]
  
  hi_bounds_count <- att[iso3 %in% att_lst, sum(!is.na(get(hi_col)))]
  
  # --- Print Results ---
  
  # Print the number of countries with estimates for the current factor.
  print(paste("Number of countries with attributable cases to", factor_name, "estimated:", length(att_lst), "/215"))
  
  # Print the number of countries that also have lower-bound estimates.
  print(paste("Lower bounds for attributable cases to", factor_name, "estimated:", lo_bounds_count, "/", length(att_lst)))
  
  # Print the number of countries that also have higher-bound estimates.
  print(paste("Higher bounds for attributable cases to", factor_name, "estimated:", hi_bounds_count, "/", length(att_lst)))
  
  print("--------------------------------------------------------------------")
}





### Check RRTB estimates

load(here("drtb/output/db_dr_country.rda"))
db_dr_country <- db_dr_country[year==yr,]
rr.lst=db_dr_country[,unique(iso3)]
print(paste("Number of countries with RRTB estimates:",length(rr.lst),"/215"))




risk_factors <- list(
  list(name = "RRTB new cases",         num_var = "e_rr_prop_new",     lo_var = "e_rr_prop_new_lo",     hi_var = "e_rr_prop_new_hi"),
  list(name = "RRTB PTC",    num_var = "e_rr_prop_ret", lo_var = "e_rr_prop_ret_lo", hi_var = "e_rr_prop_ret_hi"),
  list(name = "RRTB Incidence",     num_var = "e_inc_rr_num",     lo_var = "e_inc_rr_num_lo",     hi_var = "e_inc_rr_num_hi")
)

for (factor_info in risk_factors) {
  
  num_col <- factor_info$num_var
  lo_col <- factor_info$lo_var
  hi_col <- factor_info$hi_var
  factor_name <- factor_info$name
  
  rr_lst <- db_dr_country[!is.na(get(num_col)), unique(iso3)]
  
  lo_bounds_count <- db_dr_country[iso3 %in% rr_lst, sum(!is.na(get(lo_col)))]
  
  hi_bounds_count <- db_dr_country[iso3 %in% rr_lst, sum(!is.na(get(hi_col)))]
  
  # --- Print Results ---
  
  # Print the number of countries with estimates for the current factor.
  print(paste("Number of countries with attributable cases to", factor_name, "estimated:", length(rr_lst), "/215"))
  
  # Print the number of countries that also have lower-bound estimates.
  print(paste("Lower bounds for attributable cases to", factor_name, "estimated:", lo_bounds_count, "/", length(rr_lst)))
  
  # Print the number of countries that also have higher-bound estimates.
  print(paste("Higher bounds for attributable cases to", factor_name, "estimated:", hi_bounds_count, "/", length(rr_lst)))
  
  print("--------------------------------------------------------------------")
}


### Check Disaggregated estimates

load(here("disaggregation/output/db_estimates_country_all.Rdata"))
db_estimates_country_all <- db_estimates_country_all[year==yr,]
dis.lst=db_estimates_country_all[,unique(iso3)]
print(paste("Number of countries with disaggregated estimates:",length(dis.lst),"/215"))



### End checks









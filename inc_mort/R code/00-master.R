###
### Global TB report 2026
### Master file
### 19/06/2026
### Author: Mathieu Bastard
### WHO/HTH department
###


library(here)


# Run the different scripts to produce the TB disease burden estimates
# For publication in the 2026 global tuberculosis report

# All library, functions and files used in the different scripts are loaded directly in the corresponding script

# Some of the code used was developed by Philippe Glaziou



# Cleaning UNAIDS data
# To run only once or if a new UNAIDS file is available

source(here('inc_mort/R code/01a-UNAIDS_cleaning.R'))


# Cleaning new GHE 2023 data
# To run only once 

source(here('inc_mort/R code/01b-GHE2023_cleaning.R'))


# Cleaning VR data from WHO/DDI and WHO/GTB database
# To run only once or if a new VR file is available
# And at each new snapshot of the WHO/GTB databases to capture data reported to GTB

source(here('inc_mort/R code/01c-VR_cleaning.R'))



# Create skeleton dataset for new year estimate

source(here('inc_mort/R code/01d-est_skeleton.R'))



# Prepare DRS dataset for Pete

source(here('inc_mort/R code/01e-drs.R'))



# Estimating HIV prevalence in TB from WHO/GTB database
# To run each time a new snapshot of the WHO/GTB databases is done

source(here('inc_mort/R code/02-tbhiv_prevalence.R'))


# Estimating TB incidence overall and disaggregate it by HIV
# To run each time a new snapshot of the WHO/GTB databases is done
# To run for each new round of estimates

source(here('inc_mort/R code/03-incidence.R'))


# Estimating TB mortality overall and disaggregated by HIV
# To run each time a new snapshot of the WHO/GTB databases is done
# To run for each new round of estimates
# Needs editing to use the last TB incidence estimate dataset (function has been written to used it)

source(here('inc_mort/R code/04-mortality.R'))

# Estimating TB incidence and mortality overall and disaggregated by HIV at global and regional level
# To run each time a new snapshot of the WHO/GTB databases is done
# To run for each new round of estimates

source(here('inc_mort/R code/05-aggregate.R'))


# Estimating attributable cases per country to the risk factors
# Estimating the global attributable cases to the risk factors
# To run each time a new snapshot of the WHO/GTB databases is done
# To run for each new round of estimates

#Import GHO data, malnutrition and diabetes (new 2025)

# source(here('import/save_undernutrition.R'))
# source(here('import/save_diabetes.R'))

source(here('inc_mort/R code/06a-attributable_cases_undernut.R'))
source(here('inc_mort/R code/06b-attributable_cases_diabetes.R'))
source(here('inc_mort/R code/06c-attributable_cases_rf.R'))
source(here('inc_mort/R code/06d-attributable_cases_aggregates.R'))





# Final checks of estimates
# Script checks if estimates are available for all countries at estimates year

source(here('inc_mort/R code/08-checks.R'))




### End






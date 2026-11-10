
library(dplyr)

# Assuming your dataset is named 'df' and contains a column named 'year'

library(dplyr)

# Assuming your dataset is named 'df'
df <- newtb %>%
  mutate(
    c_newinc = case_when(
      # For years before 2013
      year < 2013 ~ coalesce(tot_newrel, 0) + 
        coalesce(newret_oth, 0),
      
      # For years 2013 to 2024
      year >= 2013 & year <= 2024 ~ coalesce(new_labconf, 0) + 
        coalesce(new_clindx, 0) + 
        coalesce(new_ep, 0) + 
        coalesce(ret_rel_labconf, 0) + 
        coalesce(ret_rel_clindx, 0) + 
        coalesce(ret_rel_ep, 0),
      
      # For years 2025 onwards
      year >= 2025 ~ coalesce(pulm_labconf_new, 0) + 
        coalesce(pulm_labconf_rec, 0) + 
        coalesce(pulm_labconf_unk, 0) + 
        coalesce(pulm_clindx_new, 0) + 
        coalesce(pulm_clindx_rec, 0) + 
        coalesce(pulm_clindx_unk, 0) + 
        coalesce(ep_newinc, 0),
      
      TRUE ~ NA_real_
    )
  )


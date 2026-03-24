library(dplyr)
library(purrr)
#prepare results
#GAUSS
tab_gauss <- tab_gauss %>% 
  rename(primary_GAUSS = primary,
         suppressed_GAUSS = suppressed,
         lo_GAUSS = lo,
         up_GAUSS = up) %>% 
  select(starts_with("var"),response, n_contr,primary_GAUSS, suppressed_GAUSS,
         lomax_response, upmin_response, lo_GAUSS, up_GAUSS)
#MODULAR
out_tau_2 <- out_tau_2 %>% 
  rename(primary_modular = primary,
         suppressed_modular = suppressed,
         lo_modular = lo,
         up_modular = up) %>% 
  select(starts_with("var"),primary_modular, suppressed_modular,
         lo_modular, up_modular)
#Simple
out_simple_2 <- out_simple_2 %>% 
  rename(primary_simple = primary,
         suppressed_simple = suppressed,
         lo_simple = lo,
         up_simple = up) %>% 
  select(starts_with("var"),primary_simple, suppressed_simple,
         lo_simple, up_simple)

#Merge the three results ("tab_gauss", "out_tau_2", "out_simple_2")
result_list <- list(tab_gauss, out_tau_2, out_simple_2)
#merge with reduce()
df_merged <- reduce(result_list, left_join, by = vars)

#TODO
#calculate class/number of @s for importance_score

#Protectedness:
#For how many primary suppressed cells [lo, up] is OK, i.e  
#[lomax_response, upmin_response] is covered by [lo, up]?

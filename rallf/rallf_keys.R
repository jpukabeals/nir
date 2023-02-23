
library(tidyverse)



# Now I apply plot_harvest_year to the bottle codes



# key1 plot treatments--------------------------------------------------------------------

# read a csv in the nir folder




# plot treatment key
# https://docs.google.com/spreadsheets/d/14A64mmcHpf6ZhIt5dcRIOJ0lyA6a2RHpkwQKdTlLNII/edit#gid=1184137171

# read.delim(
#   "clipboard"
# ) -> df3

# df3 %>% 
#   rename_all(tolower) %>% 
#   rename(
#     site = location,
#     intensity = cuts,
#     variety = var
#     ) %>% 
#   dplyr::select(
#     plot,
#     site,
#     variety,
#     intensity
#   ) %>% 
#   mutate(
#     intensity = fct_recode(intensity,
#                            "35-day" = "5 cuts",
#                            "45-day" = "4 cuts"),
#     site = fct_recode(site,
#                       "rosemount" = "Rosemount",
#                       "st paul" = "St. Paul")
#   ) %>% 
#   mutate_if(
#     is.factor,as.character
#   ) %>% 
#   mutate(
#     rep = str_sub(plot,2,2)
#   ) -> key1

# key1 %>%
#   write.csv("rallf_key_plotTreatments.csv",
#             row.names = F)

read.csv("rallf_key_plotTreatments.csv") -> key1

# key 2 harvest code year ---------------------------------------------------


# 
# rep(1:5,
#     each=4) -> cut
# 
# # 20 is length
# 
# rep(
#   c("st paul",
#   "rosemount"),
#   10
# ) -> site
# 
# rep(
#   c(
#     "35-day",
#     "45-day"
#   ),
#   each=2,
#   5
# ) -> intensity
# 
# seq(1,20,1) -> harvest_code
# 
# tibble::tibble(
#   harvest_code,
#   site,
#   intensity,
#   cut
# ) -> df1
# 
# # now expand by year
# 
# 2021:2025 -> year
# 
# 
# df1 %>% 
#   expand_grid(year) %>% 
#   arrange(year) %>% 
#   relocate(year,.before=harvest_code) -> df2
# 
# # manually add dates if desired
# df2 %>%
#   write.csv(
#     "rallf_key_harvestCode.csv",
#     row.names = F
#   )
# 
# df2 -> key2

read.csv(
  "rallf_key_harvestCode.csv"
) -> key2



# key code to harvest -----------------------------------------------------

# https://docs.google.com/spreadsheets/d/1VY6EwF6AqK_G9cSJysfAWyfH9BS61m88rZPFHjsGmWs/edit#gid=99569789

read.csv("rallf_key_codeHarvest.csv") -> key3


# testing compatibility ---------------------------------------------------



# key -- harvest_code to date ---------------------------------------------



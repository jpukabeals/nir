


source("functions_nir.R")

library(tidyverse)

read.csv("nir_allHay_15Feb2023.csv") -> dat

# these are columns for all products, not just hay
# many of these columns are lists

# interestingly, if the column is read as a list, it seems it is a column that
# was for a equation that wasn't Hay, so we can filter by that

dat %>% 
  dplyr::select(
    -where( #note I'm using negative sign before where()
      is.logical
    )
  ) -> dat2


tidy.nir.report.with.periods2(dat2) %>% 
  # glimpse()
  calc.rfq.rfv() -> dat3

# Now I'm referencing the "nir_import-tidy_7Feb2023.R" to find a compiled treatment key
# source("nir_import-tidy_7Feb2023.R")
# dat3 is only dataframe preserved, others were overwritten when calling in
# just taking the treatment key rather than running source
read.csv("bottleCodes_compiled_2020to2023.csv") -> treatmentKey

treatmentKey %>% 
  mutate(code = `Sample.ID`) -> treatmentKey

# there is a lot of bottle codes that got read into treatment key that don't
# have corresponding info and should just be filtered out

treatmentKey %>% 
  # colnames()
  mutate(id = as.numeric(`Sample.ID`)) %>% 
  # filter(is.na(id)) %>% 
  drop_na(id) -> treatmentKey2
  
  
# when making id a numeric vector and dropping NAs, we lost clipping trial data,
# n=~80, that's it. Will need to come back to dig up that data if needed. 

# filtering out bottle codes that were read in but don't have have any ID info,
# at least as of 15Feb2023, this may need to be updated if more bottle codes are
# added to compiled treatment codes

treatmentKey2 %>% 
  filter(id<220787) %>%
  filter(id>=220000 | id<210975) %>% 
  filter(id<20389 | id>=210000) -> treatmentKey3

# we have 2148 bottle codes between 2020:2022

# we tack on the treatment key where we have matches, but keep all scan info
# dat3 %>% 
#   left_join(treatmentKey3) %>% 
#   # dim()
#   View()


# just show me nir data that has a match with treatment key
treatmentKey3 %>% 
  inner_join(dat3) -> dat4


# RALLF -------------------------------------------------------------------

# Lets get that RALLF data


dat4 %>% 
  mutate_all(tolower) %>% 
  filter_all(
    .,
    .vars_predicate = any_vars(
      str_detect(.,"rallf") #|
    )) %>% 
  # glimpse()
  # View()
  write.csv("dummy.csv")

# going to tidy in excel

# experiment details\
# each site, n=48
# each cutting schedule, n=2

# 2021 there are 3 cuts, 7/16, 8/20, 9/1, 10/18

library(googlesheets4)
read_sheet(
  "https://docs.google.com/spreadsheets/d/1VY6EwF6AqK_G9cSJysfAWyfH9BS61m88rZPFHjsGmWs/edit#gid=0"
) -> dat7

dat7 %>% 
  # group_by(Date, Location) %>% 
  # group_by(Location,Date) %>% 
  # group_by(Location,Harvest, Date) %>% 
  group_by(Location,Harvest) %>% 
  tally()

# harvest_point is cut number


# we are missing 2022 rosemount cut1

# we are missing harvest point 4 data for both sites

  
# first cutting should have n=48, rest should be n=24
# 
# read.csv("dummy.csv") %>% 
#   write.csv("RALLF_nirData_20Feb2023.csv")

read.csv("RALLF_nirData_20Feb2023.csv") -> dat5

# fixing st. paul naming issue
dat5 %>% 
  mutate(site = fct_recode(site,
                           "st paul" = "st. paul")) -> dat5


# dat5 %>% 
#   # glimpse()
#   distinct(harvest_point)
# # adding in more descriptive timepoint data
# 
# 
# seq(1,6,1) -> harvest_point
# c(
#   "~24May",
#   "~28Jun",
#   "~8Jul",
#   "~2Aug",
#   "~22Aug",
#   "~6Sep"
# ) -> harvest_timing

# tibble(
#   harvest_point,
#   harvest_timing
# ) %>% 
#   right_join(
#     dat5
#   ) %>% 
#   relocate(c(harvest_point,harvest_timing),
#            .after = variety) %>% 
#   dplyr::select(-c(X,code,datetime)) -> dat6


dat6 <- dat5
  
dat6 %>% 
  group_by(year,site,
           # harvest_timing
           harvest_point
           ) %>% 
  tally()

dat6 %>% 
  group_by(year,site,harvest_point,cut) %>% 
  tally() %>% 
  print(n=100)



# KODU --------------------------------------------------------------------


# now let's get the kodu data

# dat4 %>%
#   mutate_all(tolower) %>%
#   # View()
#   filter_all(
#     .,
#     .vars_predicate = any_vars(
#       str_detect(.,"kodu") #|
#         # str_detect(.,"orei")
#         )) %>%
#   # .$name -> filterout
#   dplyr::select(-all_of(filterout)) %>%
#   # glimpse()
#   # dim
#   # View()
#   write.csv("dummy.csv")

# I tidied stuff in excel

read.csv("kodu_nirData_15Feb2023.csv") -> dat5


# writing files to copy and paste into the google sheet
# https://docs.google.com/spreadsheets/d/1h_qAGjXJ6xGUbuScg_jzztqhuiOTQbL6AlZx9VFHuKA/edit?usp=sharing

# dat5 %>% 
#   # distinct(biomassType)
#   filter(year=="2022"  &
#          site != "ks" &
#            biomassType == "summer straw"
#          ) %>%
#   # print()
#   select(c(plot,quadrat,ADF, ndf48h,protein,rfv,rfq.grass, ndf)) %>% 
#   arrange(plot,quadrat) %>% 
#   # print()
#   write.csv("dummy2.csv")


# Making summary report for an email
dat5 %>% 
  group_by(site,year,biomassType) %>% 
  arrange(year) %>%  
  tally()

yr <- unique(dat5$year)
bt <- unique(dat5$biomassType)
st <- c("kansas", "minnesota")

expand.grid(
  st,yr,bt
) %>% 
  arrange(Var1,Var2,rev(Var3)) %>% 
  write.csv("clipboard",row.names = F)



# Imports the raw extensive reports and outputs processed data with necessary ID
# info and calculated forage quality parameters


# Useful functions and packages
source("functions_nir.R")
library(tidyverse)
library(googlesheets4)


# Data import -------------------------------------------------------------

# All Hay data from up until 15Feb. This data was already filtered to only
# contain rows where the Hay equation was used.
read.csv("nir_allHay_15Feb2023.csv") -> dat

# Adding in more NIRS data that was collected between 15Feb and 15Mar
read.csv(
  "nir_15Feb2023_To_15Mar2023.csv"
) %>% 
  filter(Product.Name == "Hay") %>% 
  bind_rows(dat) -> dat

# Adding in mor eNIRS data that was collected between 15Mar and 5Apr

read.csv(
  "nir_15Mar2023_To_5Apr2023.csv"
) %>% 
  filter(Product.Name == "Hay") %>% 
  mutate(Sample.ID = as.character(Sample.ID)) %>% 
  bind_rows(dat) -> dat

# dat dataset contains columns for the predicted values of all products (IWG,
# Pennycress etc.). This means there is a column for Eicosenoic Acid because
# this is predicted by pennycress equation. This is junk that we now will
# remove. 

# interestingly, if the column is read as a list, it seems it is a column that
# was for an equation that wasn't Hay, so we just filter out columns that are
# lists.

dat %>% 
  dplyr::select(
    -where( #note I'm using negative sign before where()
      is.logical
    )
  ) -> dat2


# Put forage quality on a dry matter basis
tidy.nir.report.with.periods2(dat2) %>% 
  # glimpse()
  calc.rfq.rfv() -> dat3

# Now I'm pulling in treatment key that was generated in
# "nir_import-tidy_7Feb2023.R" - see 7Feb EDA in RALLf repository for how I
# generated it. It may not be exhaustive. 
read.csv("bottleCodes_compiled_2020to2023.csv") -> treatmentKey

treatmentKey %>% 
  mutate(code = `Sample.ID`) -> treatmentKey

# there are a lot of bottle codes that got read into treatment key that don't
# have corresponding info and should just be filtered out

treatmentKey %>% 
  # colnames()
  mutate(id = as.numeric(`Sample.ID`)) %>% 
  # filter(is.na(id)) %>% 
  drop_na(id) -> treatmentKey2
  
# when making id a numeric vector and dropping NAs, we lost clipping trial data,
# n=~80, that's it. Will need to come back to dig up that data if needed. 

# filtering out bottle codes that were read in but don't have have any ID info,
# at least as of 15Mar2023, this may need to be updated if more bottle codes are
# added to compiled treatment codes

treatmentKey2 %>% 
  filter(id<220787) %>%
  filter(id>=220000 | id<210975) %>% 
  filter(id<20389 | id>=210000) -> treatmentKey3

# we have 2150 bottle codes between 2020:2022

# just show me nir data that has a match with treatment key
treatmentKey3 %>% 
  inner_join(dat3) -> dat4


# RALLF -------------------------------------------------------------------

# Lets get that RALLF data

# all we need is code,plot and year, the rest will be filled in with keys

dat4 %>% 
  mutate_all(tolower) %>% 
  filter_all(
    .,
    .vars_predicate = any_vars(
      str_detect(.,"rallf") #|
    )) %>% 
  dplyr::select(
    Sample.ID,
    ID4,
    ID1,
    36:55
  ) %>% 
  rename(
    plot = ID4,
    year = ID1,
    code = Sample.ID
  ) %>% 
  write.csv(
    "rallf_nirData_5Apr2023.csv",
    row.names = F
  )

# read.csv("RALLF_nirData_20Feb2023.csv") -> dat5

# fixing st. paul naming issue
# dat5 %>% 
  # mutate(site = fct_recode(site,
                           # "st paul" = "st. paul")) -> dat5


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


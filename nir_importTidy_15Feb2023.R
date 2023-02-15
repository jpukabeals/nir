
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
  filter(id>220000 | id<210975) %>% 
  filter(id<20389 | id>210000) -> treatmentKey3

# we have 2148 bottle codes between 2020:2022

# we tack on the treatment key where we have matches, but keep all scan info
dat3 %>% 
  left_join(treatmentKey3) %>% 
  # dim()
  View()


# just show me nir data that has a match with treatment key
treatmentKey3 %>% 
  inner_join(dat3) -> dat4
  

# now let's get the kodu data

dat4 %>% 
  mutate_all(tolower) %>% 
  # View()
  filter_all(
    .,
    .vars_predicate = any_vars(
      str_detect(.,"kodu") #|
        # str_detect(.,"orei")
        )) %>% 
  # summarise_all((~sum(is.na(.)))) %>% 
  # pivot_longer(cols = -0) %>% 
  # arrange(desc(value)) %>% 
  # filter(value==173) %>% 
  # .$name -> filterout
  dplyr::select(-all_of(filterout)) %>% 
  # glimpse()
  # dim
  # View()
  write.csv("dummy.csv")

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


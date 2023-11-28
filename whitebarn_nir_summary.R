

# Useful functions and packages
source("functions_nir.R")
library(tidyverse)
library(googlesheets4)

# read in white barn nirs data

read.csv("whitebarn_nirData_27Nov2023.csv") ->dat

# dat %>% 
#   glimpse()

tidy.nir.report.with.periods3(dat) -> dat2

dat2 %>% 
  glimpse()

# filter out data that's not desired

dat2 %>% 
  arrange(datetime)


# used large rotating tray instead of small plastic cup static for first 5
# bottle codes, then rescanned at end. I am removing the data scanned using
# incorrect tray

dat2 %>% 
  filter(datetime > "2023-11-27 01:57:02") %>%
  arrange(datetime) -> dat3

# We want 3 observations per bottle code, let's look at observations with more
# than 3 obesrvations. This is likely caused by accident or one of the values
# was weird so we rescanned

dat3 %>% 
  group_by(code) %>% 
  tally() %>% 
  arrange(desc(n))

# two codes have more than 3 observations. 
# 230498 is strawbale sample so I scanned it extra times to get more data
# 230480 should be investigated

dat3 %>% 
  filter(code==230480) 

# looking at protein content of first scan, it seems most different from others
# so we'll remove

dat3 %>% 
  filter(datetime != "2023-11-27 02:13:05") -> dat4

# read in treatment information

read.csv("2023 Bottle Codes.csv") -> bottleCodes

bottleCodes %>% 
  rename(code = Bottle.Code) %>% 
  right_join(dat4) -> dat5

# the id information is messed up

dat5 %>% 
  mutate(sample_type = Location,
         sample_location = Plot,
         plot = Sample.Timing,
         exclosure = Sample.Type,.before=datetime) %>% 
  select(-c(Date,Location,Plot,Sample.Timing,Sample.Type)) -> dat6

# NIR MASTER

calc.rfq.rfv.grass(dat6) -> dat7
  
dat7 %>% 
  # glimpse()
  group_by(sample_location,plot,exclosure) %>% 
  summarise(
    across(
      9:27,
      mean
    )
  ) %>% 
  arrange(desc(sample_location)) %>% 
  write.csv("whitebarn_nir_masteroutput.csv",
            row.names = F)

# report

# straw bale
dat7 %>% 
  filter(sample_type=="straw bale") -> dat8

write.csv(dat8, "whitebarn_strawbale.csv",
          row.names = F)
  
# protein
dat7 %>% 
  filter(sample_type !="straw bale") %>% 
  select(Experiment,sample_type,sample_location,exclosure,plot,
         protein, adf, ndf, rfv.sdsu) -> dat9


# moisture content

# https://docs.google.com/spreadsheets/d/1CtGkiFazbxG_Xeg4kQVQFkLK9TAsaLhy8RNksqq9BNo/edit#gid=126232655
read.csv("White Barn Master - forage.csv") -> dat10

# putting it into naming convention of this file
dat10 %>% 
  rename(exclosure = Trt,
         plot = Point,
         sample_location = Field,
         sample_type = Sample.timing,
         experiment = Location) %>% 
  select(-c(Experiment,Year)) -> dat11

# getting only the data we want and converting it to units we want
# used 30" x 30" quadrats
library(measurements)

dat11 %>% 
  select(experiment,sample_type,sample_location,plot,exclosure,
         wet_forage_biomass__with_bag_grams,
         bag_tare_grams,
         dry_forage_biomass_with_bag_grams) %>% 
  rename(wet = wet_forage_biomass__with_bag_grams,
         dry = dry_forage_biomass_with_bag_grams,
         tare = bag_tare_grams) %>% 
  mutate(wet = wet-tare,
         dry = dry-tare) %>% 
  drop_na(dry) %>% 
  select(-tare) %>% 
  mutate(water = round((wet-dry)/wet*100,1)) %>% 
  # converting units from grams per 30x30" quadrat to Mg ha
  mutate(
    wet_Mgha = conv_multiunit(
      wet/(30*30),
      from = "g / inch2", to = "Mg / hectare"
    ),
    dry_Mgha = conv_multiunit(
      dry/(30*30),
      from = "g / inch2", to = "Mg / hectare"
    )
  ) %>% 
  rename(water_content_percent = water)-> dat12
  

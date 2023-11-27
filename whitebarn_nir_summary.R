

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

# the id information is messed up, not going to mess with it

dat5 %>% 
  mutate(sample_type = Location,
         sample_location = Plot,
         plot = Sample.Timing,
         exclosure = Sample.Type,.before=datetime) %>% 
  select(-c(Date,Location,Plot,Sample.Timing,Sample.Type)) -> dat6

# NIR MASTER

calc.rfq.rfv.grass(dat6) -> dat7
  
dat7 %>% 
  write.csv("whitebarn_nir_masteroutput",
            row.names = F)

# report

# straw bale
dat7 %>% 
  filter(sample_type=="straw bale") -> dat8

  
# protein
dat7 %>% 
  filter(sample_type !="straw bale") %>% 
  select(Experiment,sample_type,sample_location,exclosure,plot,
         protein, adf, ndf, rfv.sdsu) -> dat9


# relative feed value


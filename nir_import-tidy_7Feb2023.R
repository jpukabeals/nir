# 7Feb2023
# compiling raw nir reports for Jake collected by Cole in winter 2023

library(tidyverse)
library(googlesheets4)

# read in compiled raw reports
url_compiledReports <- "https://docs.google.com/spreadsheets/d/1iCLw7HYw6CFCf7cdhy0fIYO3cu63O_wtKMC4FDtolII/edit#gid=0"
read_sheet(
  url_compiledReports,
  sheet = 1
) -> compiledReports


# read in bottle codes - treatment key
url_treatmentKey <- "https://docs.google.com/spreadsheets/d/1kS39V2XoRAYyYoxTVS-0RJ6DCPLuSfxMIejEqQ3Zb6s/edit#gid=0"

read_sheet(
  url_treatmentKey,
  sheet = 2
) -> treatmentKey

# Bottle Code is being read as a list
# When we unlist Bottle Code, it is size 1000, needs to be size 1021

treatmentKey %>% 
  drop_na(`Bottle Code`) %>% 
  mutate(`Sample ID` = unlist(`Bottle Code`),
         # year = unlist(Year),
         .before = `Bottle Code`) %>% 
  # full_join(compiledReports) %>% 
  left_join(compiledReports) %>% 
  mutate(
    across(where(is.list),
           unlist)
  ) %>%
  # View()
  write.csv("nir_raw-report-with-treatments_left-join.csv")

# full join raw reports with treatment key by bottle code

compiledReports %>% 
  dplyr::rename(bottle = `Sample ID`)
  full_join(treatmentKey)

# assess whether this method of joining NIR data works. 
rm(list=ls())

library(googlesheets4)

# all cuts probably start first week of May
# 8 unique harvest dates


read_sheet(
  "https://docs.google.com/spreadsheets/d/1VY6EwF6AqK_G9cSJysfAWyfH9BS61m88rZPFHjsGmWs/edit#gid=0",
  gs4_deauth()
)

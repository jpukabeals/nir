# Jesse
# 15Mar2022
# Obj: compile NIR excel data into a single file



# scanning occured over 4 dates
# each date has it's own excel file exported
# in this first block, I am combining the excel files into a single file
# the Perten should've done this, but I guess I will...grrrrr

# excel file names saved by perten to flashdrive
# I am also uploading copies of this data to the drive
# https://drive.google.com/drive/folders/1XRC5raKy2Pzmcvb0hjbKDGJcFLeIPlb8

mar15 <- "Extensive Report_All-Products_20220315_0900_To_20220315_1227_avg.xlsx"
mar14 <- "Extensive Report_All-Products_20220314_1400_To_20220314_1621_avg.xlsx"
mar11 <- "Extensive Report_All-Products_20220311_1200_To_20220311_1540_avg.xlsx"
mar10 <- "Extensive Report_All-Products_20220310_0000_To_20220310_1602_avg.xlsx"

# R is reading these files directly off of flash drive
setwd("E://") 
dat_mar15 <- readxl::read_xlsx(mar15)
dat_mar14 <- readxl::read_xlsx(mar14)
dat_mar11 <- readxl::read_xlsx(mar11)
dat_mar10 <- readxl::read_xlsx(mar10)

# returning working directory to my Downloads
setwd("C:/Users/pukab001/Downloads")

# writing the combined csv
dat_mar15 %>% 
  bind_rows(.,dat_mar14) %>% 
  bind_rows(.,dat_mar11) %>% 
  bind_rows(.,dat_mar10) %>% 
  write.csv("nir_orei-manure+kodu_all_raw.csv",
            row.names = F)
# upload to google drive
# https://drive.google.com/drive/folders/1XRC5raKy2Pzmcvb0hjbKDGJcFLeIPlb8
# copy and paste the csv contents too a google sheet
# https://docs.google.com/spreadsheets/d/1z2B9jrX-pGm9YaUfVRTFczP-mI7UCfepGMw21QiH8f8/edit


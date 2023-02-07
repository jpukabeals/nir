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

# I found this function that will convert list values from NULL to NA, then we
# can convert out of list without dropping rows and then filter

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

treatmentKey %>% 
  mutate(
    across(
      where(is.list),
      nullToNA
    )
  ) %>% 
  mutate(
    across(
      where(is.list),
      unlist
    )
  ) %>% 
  drop_na(`Bottle Code`) %>% 
  mutate(`Sample ID` = `Bottle Code`)-> treatmentKey2

# 2021 treatment keys
# In the future, we should combine all treatment keys in 1 sheet
url_treatmentKey3 <- "https://docs.google.com/spreadsheets/d/1CZfJTTfBaAa7B09sv2M8Xd8AndKjeuAIYVef38pgJAM/edit#gid=0"

read_sheet(
  url_treatmentKey3,
  sheet = 1
) -> treatmentKey3

treatmentKey3 %>% 
  mutate(
    across(
      where(is.list),
      nullToNA
    )
  ) %>% 
  mutate(
    across(
      where(is.list),
      unlist
    )
  ) %>% 
  drop_na(`Bottle Code`) %>% 
  mutate(`Sample ID` = `Bottle Code`) -> treatmentKey4


# 2020 treatment keys
url_treatmentKey5 <- "https://docs.google.com/spreadsheets/d/1sFwDptu4xwjWh9RyyUpcWZdVrjzy0dy_Uv5dAv1mnmg/edit#gid=2088314350"

read_sheet(
  url_treatmentKey5,
  sheet = 1
) -> treatmentKey5

treatmentKey5 %>% 
  mutate(
    across(
      where(is.list),
      nullToNA
    )
  ) %>% 
  mutate(
    across(
      where(is.list),
      unlist
    )
  ) %>% 
  drop_na(`Bottle Code`) %>% 
  mutate(`Sample ID` = `Bottle Code`) -> treatmentKey6


bind_rows(treatmentKey2,
          treatmentKey4,
          treatmentKey6) -> treatmentKeyMaster 
  
# full join raw reports with treatment key by bottle code just 2022 bottle codes
treatmentKey2 %>% 
  full_join(compiledReports) %>%
  # View()
  write.csv("nir_raw-report-with-treatments_full-join.csv")

# right join for 2020-2022 bottle code key
treatmentKeyMaster %>% 
  right_join(compiledReports) %>% 
  # View()
  write.csv("nir_raw-report-with-treatments_right-join.csv")

# It's ugly and bulky, but someone can filter through this csv file to find
# enough ID characteristics associated with a scan to get what they need


# Calculating RFQ-RFQ -----------------------------------------------------


source("functions_nir.R")

# Renaming columns to avoid future confusion
treatmentKeyMaster %>% 
  colnames()

colnames(treatmentKeyMaster) <- c("Sample ID", paste0(rep("ID",25),1:25))

# Output
tidy.nir.report.with.spaces.predicted.first(compiledReports) %>% 
  mutate(`Sample ID` = code) %>% 
  calc.rfq.rfv() %>%
  # colnames()
  dplyr::select(`Sample ID`,CP,NDF,ADF,rfv,rfq.grass,rfq.legume) %>% 
  left_join(treatmentKeyMaster) %>% 
  # colnames()
  mutate(
    across(8:32,
           as.character)
  ) %>% 
  mutate(
    across(8:32,
           ~replace_na(.,""))
  ) %>% 
  write.csv(
    "nir_processed-report-with-treatments_right-join.csv",
    row.names = F)


# 7Feb2023
# compiling raw nir reports for Jake collected by Cole in winter 2023

library(tidyverse)
library(googlesheets4)

# read in compiled raw reports
url_compiledReports <- "https://docs.google.com/spreadsheets/d/1iCLw7HYw6CFCf7cdhy0fIYO3cu63O_wtKMC4FDtolII/edit#gid=0"
read_sheet(
  url_compiledReports,
  gs4_deauth(),
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

rm(treatmentKey,treatmentKey2,treatmentKey3,treatmentKey4,treatmentKey5,treatmentKey6)

treatmentKeyMaster %>% 
  write.csv("bottleCodes_compiled_2020to2023.csv")
  
# full join raw reports with treatment key by bottle code just 2022 bottle codes
treatmentKeyMaster %>% 
  full_join(compiledReports) %>%
  # View()
  # dim()
  write.csv("nir_raw-report-with-treatments_full-join.csv")

# right join for 2020-2022 bottle code key
treatmentKeyMaster %>% 
  right_join(compiledReports) %>% 
  # dim()
  # View()
  write.csv("nir_raw-report-with-treatments_right-join.csv")

# It's ugly and bulky, but someone can filter through this csv file to find
# enough ID characteristics associated with a scan to get what they need

# Note that we have ~650 RALLF in full join and ~250 RALLF in right join


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
  dplyr::select(`Sample ID`,CP,ADF,NDF,NDFD,rfv,rfq.grass,rfq.legume) %>% 
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


# EDA ---------------------------------------------------------------------

tidy.nir.report.with.spaces.predicted.first(compiledReports) %>% 
  mutate(`Sample ID` = code) %>% 
  calc.rfq.rfv() %>%
  # colnames()
  dplyr::select(`Sample ID`,CP,ADF,NDF,NDFD,rfv,rfq.grass,rfq.legume) %>% 
  left_join(treatmentKeyMaster) %>% 
  # colnames()
  mutate(
    across(8:32,
           as.character)
  ) %>% 
  mutate(
    across(8:32,
           ~replace_na(.,""))
  ) -> dat

dat %>% 
  # colnames()
  filter(ID2 == "RALLF" |
           ID13 == "RALLF") -> dat2

dat2 %>% 
  colnames()

dat2 %>% 
  ggplot(aes(CP)) +
  stat_bin() +
  labs(
    caption = "CP range should be 10-20% for alfalfa"
  )

dat2 %>% 
  ggplot(aes(NDF)) +
  stat_bin() +
  labs(
    caption = "NDF range should be around 40"
  )

dat2 %>% 
  ggplot(aes(ADF)) +
  stat_bin() +
  labs(
    caption = "ADF should be around 25-30 for alfalfa"
  )

dat2 %>% 
  ggplot(aes(rfq.legume)) +
  stat_bin() +
  labs(
    caption = "RFQ should be around 125-225 for alfalfa, but it can up to 300"
  )

# I think we have super low ADF values which are causing our rfq values to be
# super high

# of RFQ>200, I bet ADF is lower than 20

dat2 %>% 
  dplyr::select(ADF,rfq.legume) %>% 
  filter(rfq.legume>200)
# yep, I was right

# let's see if this is associated with a treatment

dat2 %>% 
  filter(rfq.legume>200) %>% 
  distinct(ID9)

# Seems the May24 cutting was the source of the super low ADF and high rfq
# predictions

dat2 %>% 
  ggplot(aes(rfq.legume)) +
  stat_bin(aes(fill=ID9))

dat2 %>% 
  ggplot(aes(ADF)) +
  stat_bin(aes(fill=ID9))

dat2 %>% 
  ggplot(aes(CP)) +
  stat_bin(aes(fill=ID9))
# trend not as obvious for protein

# seems like alfalfa can range up to 300, so this is still acceptable
# https://fyi.extension.wisc.edu/forage/comparison-of-relative-forage-quality-rfq-to-relative-feed-value-rfv/#:~:text=In%20samples%20from%20the%20Worlds,213%20for%20RFV%20of%20175).



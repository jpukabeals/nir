
# loading in NIR functions
source("functions_nir.R")

# reading NIR file I made today after scanning kodu straw sample from flashdrive
readxl::read_xlsx("E://Extensive Report_All-Products_20220418_0000_To_20220418_1127_avg.xlsx") -> dat

dat %>% 
  glimpse()

tidy.nir.report(dat) -> dat_tidy
calc.rfq.rfv.grass(dat_tidy) -> dat_tidy_rfq


# google sheet with bottle codes
library(googlesheets4)
url_bottle_codes <- "https://docs.google.com/spreadsheets/d/1OFJeiWVaj7nya0DjGKpDd3f2XtV8cok1hwL7AezRR5A/edit"

bottle_codes <- read_sheet(url_bottle_codes,
                           sheet = 1,
                           gs4_deauth())

bottle_codes <- bottle_codes %>% 
  rename(code=bottle_code)

dat_tidy_rfq %>% 
  # glimpse()
  left_join(.,bottle_codes,
            by = "code") %>% 
  rowid_to_column() -> dat_tidy_treatments

dat_tidy_treatments %>% 
  select(experiment,site,year,plot,sample_biomass_type,
         rfq,rfv,protein,adf,ndf,ndf48h) -> dat_export

dat_export %>% 
  ggplot(aes(protein)) +
  stat_bin()

dat_export %>% 
  # glimpse()
  distinct(sample_biomass_type)
  



##making sure it makes sense before export

dat_export %>% 
  ggplot(aes(rfq)) +
  stat_bin()

dat_export %>% 
  write.csv("kodu_forage-quality-update_2022-04-18.csv")

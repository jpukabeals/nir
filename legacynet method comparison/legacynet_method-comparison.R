
# Compare UMN data with European data on same samples


# UMN data ----------------------------------------------------------------


source("functions_nir.R")

library(tidyverse)

# gathering only legacynet data from master file

read.csv("nir_allHay_15Feb2023.csv") -> dat

dat %>% 
  dplyr::select(
    -where( #note I'm using negative sign before where()
      is.logical
    )
  ) -> dat2

tidy.nir.report.with.periods2(dat2) %>% 
  calc.rfq.rfv() -> dat3

read.csv("bottleCodes_compiled_2020to2023.csv") -> treatmentKey

treatmentKey %>% 
  mutate(code = `Sample.ID`) -> treatmentKey

treatmentKey %>% 
  mutate(id = as.numeric(`Sample.ID`)) %>% 
  drop_na(id) -> treatmentKey2

treatmentKey2 %>% 
  filter(id<220787) %>%
  filter(id>=220000 | id<210975) %>% 
  filter(id<20389 | id>=210000) -> treatmentKey3

treatmentKey3 %>% 
  inner_join(dat3) -> dat4


dat4 %>% 
  filter(ID2 == "Legacy Net") %>% 
  dplyr::select(
    3:8,
    30:55
  ) -> dat5


# European ----------------------------------------------------------------


# European NIRS 
# Just filtering for the second cut of the follow-on phase. Note this is the same as "cut 6" in the UMN system

read.csv(
  "Forage Quality US1_Cut2+4 G 1-2 F.csv"
) %>% 
  filter(
    Stage == "F" &
      Harvest == "2"
  ) -> dat6


# Joining UMN and European ------------------------------------------------

# NDF ADF and CP are all that matter

dat5 %>% 
  mutate(
    method = "UMN"
  ) %>% 
  dplyr::select(
    method,ID4,CP,NDF,ADF
  ) %>% 
  rename(
    plot_official = ID4
  ) -> dat7

dat6 %>% 
  mutate(
    method = "Europe"
  ) %>% 
  dplyr::select(
    method,Treatment,cp,ndf,adf
  ) %>% 
  rename(
    plot_official = "Treatment",
    CP = cp,
    NDF = ndf,
    ADF = adf
  ) %>% 
  mutate(
    plot_official = as.character(plot_official)
  )-> dat8

dat7 %>% 
  bind_rows(dat8) -> dat9


# compare -----------------------------------------------------------------

dat9 %>% 
  ggplot(aes(CP,
             fill = method
             )) +
  geom_density(alpha=.5) +
  geom_vline(xintercept = 9) +
  labs(
    caption = "
    SxS second cut was 14Sep as seedhead emerged (Zadocks 50-60)
    Expected CP is 9%
    "
  )

dat9 %>% 
  ggplot(aes(ADF,
             fill = method
  )) +
  geom_density(alpha=.5) +
  geom_vline(xintercept = 40) +
  labs(
    caption = "
    SxS second cut was 14Sep as seedhead emerged (Zadocks 50-60)
    Expected ADF is ~40%
    "
  )


dat9 %>% 
  ggplot(aes(NDF,
             fill = method
  )) +
  geom_density(alpha=.5) +
  geom_vline(xintercept = 65) +
  labs(
    caption = "
    SxS second cut was 14Sep as seedhead emerged (Zadocks 50-60)
    Expected NDF is ~65%,
    UMN appears to be underestiming NDF
    "
  )

# Overall UMN appears to be underestimating CP ADF and NDF relative to Europe


# read in treatment information -------------------------------------------


getwd()
setwd("C:/Users/pukab001/Documents/R projects/legacynet")
read.csv(
  "trt.codes.csv"
) %>% 
  dplyr::select(
    plot_id_official,code
  ) %>% 
  mutate(
    plot_id_official = as.character(plot_id_official)
  ) %>% 
  rename(
    plot_official = plot_id_official
  ) -> codes


dat9 %>% 
  left_join(.,codes) -> dat10

setwd("C:/Users/pukab001/Documents/R projects/nir")



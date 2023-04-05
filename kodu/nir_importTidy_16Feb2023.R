# just uploaded NIRS data to the NIR spreadsheet, this is wet chem but it should
# fill in missing pieces of 2020 summer straw and 2021 spring cut


library(tidyverse)
library(googlesheets4)


# we are going to read in data
# url <- "https://docs.google.com/spreadsheets/d/1h_qAGjXJ6xGUbuScg_jzztqhuiOTQbL6AlZx9VFHuKA/edit#gid=2068542615"
# read_sheet(
#   url,
#   sheet=3
# ) -> dat
# 
# dat %>% 
#   write.csv("kodu_googlesheet_combiningData.csv")

setwd(
  "C:/Users/pukab001/Documents/R projects/nir/kodu"
)
getwd()
read.csv(
  "kodu_googlesheet_combiningData.csv"
) -> dat


dat %>% 
  rename(
    `sample biomass type` = sample.biomass.type
  ) %>% 
  mutate(`sample biomass type` = factor(`sample biomass type`,
                                        levels=c("spring cut", "summer straw", "fall cut"))) %>% 
  mutate(Year = as.factor(Year)) %>% 
  group_by(Year,`sample biomass type`,determinationMethod,Trt,Plot) %>% 
  summarise(
    ADF = mean(ADF, na.rm = T),
    NDFD = mean(NDFD, na.rm = T),
    CP = mean(CP, na.rm = T),
    RFV = mean(RFV, na.rm = T),
    RFQ = mean(RFQ, na.rm = T),
    NDF = mean(NDF, na.rm = T)
  ) -> dat2 
  # write.csv("kodu_googlesheet2.csv")

dat2 %>% 
  ggplot(aes(RFV)) +
  stat_bin(aes(fill=determinationMethod)) +
  facet_wrap(~`sample biomass type`*Year)

dat2 %>% 
  # glimpse()
  ggplot(aes(`sample biomass type`, RFV,
             # shape = determinationMethod,
             fill = Year)) +
  geom_boxplot(outlier.alpha = 0,
               position = position_dodge(.7)) 
  # stat_summary(geom = "point",

               # aes(shape = determinationMethod),
               # position = position_dodge(.7),
               # size=4) 


setwd(
  "C:/Users/pukab001/Documents/R projects/nir"
)

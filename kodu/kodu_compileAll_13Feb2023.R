# KODu getting all data for Jake, then adding to KODU sheet

# what are sample ID variables? Year, plot, biomass type?

# data we have already on the drive...

library(googlesheets4)
library(tidyverse)

# we have data I compiled and calculated on 18Apr2022

url_18Apr <- "https://docs.google.com/spreadsheets/d/1dOlfiVtFsoZNPH73_pXmxg_o8aBLGJ1zj0XNnHPtAag/edit#gid=284188972"

# there is also data from scanning in 2022-2023

url_2023 <- "https://docs.google.com/spreadsheets/d/1QYy8Z05UM5Tgi6BuWcwyphLdXoRsTxAI83ZP09yR8pI/edit#gid=2054155604"


# data assessment ---------------------------------------------------------

read_sheet(url_18Apr,
           gs4_deauth()) ->dat_18apr

read_sheet(url_2023,
           gs4_deauth()) -> dat_2023

dat_18apr %>% 
  View()
# the file I am reading off of the drive does not have quadrat number linked to
# bottle codes, but the file in the original bottle codes sheet does have this
# info

# for the purposes of this quick compilation, I will reassign quadrat number
# randomly as it is a random sampling point within a plot anyways

dat_18apr %>% 
  arrange(plot) %>% 
  mutate(quadrat_number = rep(1:2,16),
         .before=rfq) %>% 
  write.csv(
    "clipboard"
  )

dat_2023 %>% 
  filter(ID2=="Kodu") %>% 
  # glimpse()
  dplyr::select(1:13) %>%  
  mutate_if(
    is.list,
    unlist
  ) %>%  
  arrange(ID4) %>% 
  # glimpse()
  # View()
  write.csv(
    "clipboard",
    row.names = F
  )

# we have 2022 summer straw



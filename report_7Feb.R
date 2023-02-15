
# Run above path
# C:/Users/pukab001/Documents/R projects/nir/nir_import-tidy_7Feb2023.R

source("nir_import-tidy_7Feb2023.R")

dat %>% 
  # colnames()
  filter(ID2 == "RALLF") -> dat2

dat2 %>% 
  colnames()

dat2 %>% 
  ggplot(aes(CP)) +
  stat_bin() +
  labs(
    caption = "CP range should be 14-26% for alfalfa"
  )

dat2 %>% 
  ggplot(aes(NDF)) +
  stat_bin() +
  labs(
    caption = "NDF range should be around 25-52 for alfalfa"
  )

dat2 %>% 
  ggplot(aes(ADF)) +
  stat_bin() +
  labs(
    caption = "ADF should be around 20-43 for alfalfa"
  )

dat2 %>% 
  ggplot(aes(rfq.legume)) +
  stat_bin() +
  labs(
    caption = "RFQ should be around 100-225 for alfalfa, but it can up to 300. Should be around 150 most of the time"
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
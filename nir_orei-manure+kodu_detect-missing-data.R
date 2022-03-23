# Jesse
# 16Mar2022
# Obj: match NIR data with bottle code list to detect if we're missing data

library(googlesheets4)
library(tidyverse)

# NIR data
# googledrive link
url <- "https://docs.google.com/spreadsheets/d/1z2B9jrX-pGm9YaUfVRTFczP-mI7UCfepGMw21QiH8f8/edit#gid=0"
dat <- read_sheet(url,
                  gs4_deauth())

# tidying data
dat1 <- dat %>% 
  rename_all(.,tolower) %>% 
  filter(`product name`=="Hay") %>%
  mutate(datetime=`date/time of analysis`,
         code=`sample id`,
         drymatter=`dry matter %, predicted`,
         protein=`protein as is %, predicted`,
         adf=`adf as is %, predicted`,
         ndf=`ndf as is %, predicted`,
         ndf48h = `48dndfr as is %, predicted`) %>%
  select(filename,datetime, code, drymatter,protein,adf,ndf,ndf48h) %>%
  mutate(datetime=as.POSIXct(datetime,
                             format="%m/%d/%Y %H:%M:%S %p")) %>% 
  mutate(protein=protein*drymatter/100,
         adf=adf*drymatter/100,
         ndf=ndf*drymatter/100,
         ndf48h=ndf48h*drymatter/100)

# adding in rfq and rfv
dat2 <- dat1 %>% 
  mutate(DM=drymatter,
         CP=protein,
         NDF=ndf,
         NDFD=ndf48h,
         ADF=adf,
         EE=2.05, #2.05 is constant
         FA=EE-1,
         Ash=100-DM,
         NFC=100-((0.93*NDF)+CP+EE+Ash),
         NDFn=NDF*0.93, 
         NDFDp=22.7+0.664*NDFD,
         TDN=(NFC*.98)+(CP*.87)+(FA*.97*2.25)+(NDFn*NDFDp/100)-10,
         DMI=(-2.318)+(.442*CP)-(.01*CP^2)-(.0638*TDN)+(.000922*TDN^2)+
           (.18*ADF)-(0.00196*ADF^2)-(0.00529*CP*ADF),
         rfq=DMI*TDN/1.23,
         rfv=DMI*((89.8-(0.779*ADF)))/1.29)

# google sheet with bottle codes
url_bottle_codes <- "https://docs.google.com/spreadsheets/d/1OFJeiWVaj7nya0DjGKpDd3f2XtV8cok1hwL7AezRR5A/edit"

bottle_codes <- read_sheet(url_bottle_codes,
                           sheet = 1,
                           gs4_deauth())

bottle_codes <- bottle_codes %>% 
  rename(code=bottle_code)

# joining data on bottle codes
dat3 <- dat2 %>% 
  # glimpse()
  left_join(.,bottle_codes,
            by = "code") %>% 
  rowid_to_column();dat3

dat3 %>% 
  # distinct(code) #222 distinct codes
  # select(code) # 225 total codes, therefore there are 3 repeats?
  count(code) %>% 
  arrange(desc(n)) 

# 3 measurements for 20187
dat3 %>% 
  filter(code==20187) %>% 
  select(rowid,datetime,code)
# remove rowid 96 and 97, they were practices when we started

# 2 measurements for 20197
dat3 %>% 
  filter(code==20197) %>% 
  select(rowid,datetime,code)
# this looks like the code was entered incorrectly because measurements occured at very different times
# checking scanning records https://docs.google.com/spreadsheets/d/1hzLtILlHGJDY8efXNlf112IxE8ojpTs8WBtdCIizgSg/edit
# it appears 20197 was scanned twice, we'll just remove one of them randomly


# removing repeated measumrents
dat4 <- dat3 %>% 
  filter(rowid != 96) %>% 
  filter(rowid != 97) %>% 
  filter(rowid != 139)

dat4 %>% 
  count(code) %>% 
  arrange(desc(n))
# no more repeats! success

# we have 222 NIR observations
# Katherine said to expect 224
# calcs: 
# 16 observations for kodu fall 2020
# 32 observations for orei summer 2020
# 16 observations for orei fall 2020
# 64 observations orei summer 2021
# 16 observations orei fall 2021
# 64 observations kodu summer KS 2021
# 16 observations kodu fall 2021
16+32+16+64+16+64+16 #=224


## bad data detected - start fix ####
dat4 %>% 
  filter(is.na(experiment))
# we scanned a bottle code that isn't in bottle code list

bottle_codes %>% 
  filter(code==201568)

# not in the bottle_codes for orei-manure+kodu
# not in scanning records
# deciding this is a typo, should be 210568

# creating fix for rowid 197
fix_rowid_197 <- dat4 %>%
  # filter(is.na(experiment)) %>%
  filter(code==201568) %>% 
  mutate(code=210568) %>% 
  select(-c(site,experiment,year,plot,sample_biomass_type)) %>% 
  left_join(.,bottle_codes)

# removing bad values for rowid 197, then adding in correct values
dat5 <- dat4 %>% 
  filter(code!=201568) %>% 
  bind_rows(.,fix_rowid_197) %>% 
  arrange(rowid) 
## - end fix  with dat5 ####

# how many bottle codes are there for this experiment?
bottle_codes %>% 
  count(code) %>% 
  arrange(desc(n))
# there are 256 bottle codes

256-222 #missing 34 NIR observations

# what bottle codes is our dataset missing?
bottle_codes %>% 
  anti_join(.,dat5,
            by = "code") #%>%
  # count(code) # these are the 34 missing NIR observations
  # write.csv("kodu_orei-manure_missing-data.csv",
  #           row.names = F)
## missing data that was never scanned
# all 2021 MN data
# plot 203, orei-manure, MN, summer straw
# plot 202, kodu, MN, fall cutting
# 32 plots for kodu summer straw 2021

dat5 %>% 
  filter(code >= 210495 &
           code <= 210505)
# code 210496 would've been scanned ~11:30am on 3/10 by Jesse

dat5 %>% 
  filter(code >= 210625 &
           code <= 210635)
# code 210630 would've been scanned ~11"30 on 3/10 by Jesse

# possible these bottles were marked as scanned but never scanned
# Jesse physically checked bottles, could not find the 2 missing bottles
# Katherine confirmed these samples are missing


# adding in grazing and fertility treatments ------------------------------

dat5 %>% 
  glimpse()


# pull in orei-manure treatment codes
url_orei <- "https://docs.google.com/spreadsheets/d/1OFJeiWVaj7nya0DjGKpDd3f2XtV8cok1hwL7AezRR5A/edit#gid=121684698"

trt_codes_orei <- read_sheet(url_orei,
           gs4_deauth(),
           sheet = 2)


# checking correct import
trt_codes_orei %>% 
  # distinct(experiment)
  summary
# success

# left_join works!
dat5 %>% 
  left_join(.,trt_codes_orei) 

# Bowden's wajar to joni all treatment code sheets first

trt_codes_kodu_ks <- read_sheet(url_orei,
                             gs4_deauth(),
                             sheet = 3)

trt_codes_kodu_mn <- read_sheet(url_orei,
                                gs4_deauth(),
                                sheet = 4)

trt_codes_all <- trt_codes_kodu_ks %>% 
  bind_rows(.,trt_codes_kodu_mn) %>% 
  bind_rows(.,trt_codes_orei)

# left join dat5 with treatment_codes

dat6 <- dat5 %>% 
  left_join(.,trt_codes_all) 

dat6 %>% 
  glimpse()

dat6 %>% 
  ggplot(aes(rfq)) +
  # stat_density(aes(fill=graz_trt)) +
  stat_density(aes(fill=fert_trt),
               bw=4) +
  facet_grid(~sample_biomass_type*site)

dat6 %>% 
  # filter(sample_biomass_type=="summer" &
  #        site=="mn") %>%
  ggplot(aes(fert_trt,rfq)) +
  stat_summary()



dat6 %>% 
  ggplot(aes(fert_trt,rfq)) +
  geom_jitter(width = .1,
              aes(col=graz_trt)) +
  facet_wrap(~sample_biomass_type)

dat6 %>% 
  ggplot(aes(fert_trt,rfq,
             fill=graz_trt)) +
  stat_summary(geom = "bar",
               position = position_dodge(),
               col=1) +
  facet_wrap(~fert_trt,
             scales = "free_x") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  # scale_fill_ordinal()
  scale_fill_brewer(type="qual",
                    palette = 4)
  # scale_fill_grey()
  


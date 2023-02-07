

# Overview ----------------------------------------------------------------

# NIR data for Honken and Anderson 2021 from plots that were grazed


# import ------------------------------------------------------------------

library(tidyverse)
library(readxl)

setwd("C:/Users/pukab001/Downloads")


dat <- read_excel("Extensive Report_All-Products_20210617_0000_To_20210624_1408_avg.xlsx")

str(dat)

dat1 <- dat %>%
  mutate(datetime=`Date/Time of Analysis`,
         code=`Sample ID`,
         drymatter=`Dry matter %, Predicted`,
         protein=`Protein As is %, Predicted`,
         adf=`ADF As is %, Predicted`,
         ndf=`NDF As is %, Predicted`,
         ndf48h = `48dNDFr As is %, Predicted`,
         lignin=`Lignin As is %, Predicted`) %>%
  select(datetime, code, drymatter,protein,adf,ndf,ndf48h,lignin)


# Want to bring in plot number based on sample ID

codes <- read_excel("2021 Bottle Codes - Copy.xlsx")
codes <- codes %>%
  mutate(code=`Bottle Code`)

dat2 <- merge(codes,dat1, by="code")

# want to confirm dataset is good
view(dat2)
view(dat1)

# clean up data by timestamps ---------------------------------------------

library(lubridate)

dat2 <- dat2 %>%
  mutate(datetime=mdy_hms(datetime)) %>%
  mutate(time=hms::as_hms(datetime))

dat2 %>%
  arrange(time)

# for each code, I arrange them in order of time and 
# select the three plots that have the latest time stamp

dat2 %>%
  group_by(code) %>%
  tally() %>%
  arrange(desc(n))

dat2 %>%
  group_by(code) %>%
  arrange(desc(time)) %>%
  slice(1:3) %>%
  dplyr::select(code,time) %>%
  tally() %>%
  arrange(desc(n))

dat2 <- dat2 %>%
  group_by(code) %>%
  arrange(desc(time)) %>%
  slice(1:3) %>%
  ungroup()

# visualize w/ jake ---------------------------------------------------------------

library(lattice)

bwplot(~protein,dat2)
bwplot(~adf,dat2)
bwplot(~ndf,dat2)

bwplot(protein~factor(Treatment)|Site,dat2)
bwplot(adf~factor(Treatment)|Site,dat2)
bwplot(ndf~factor(Treatment)|Site,dat2)
# ADF and NDF seem lower in grazed plots at Anderson, strange...


# Outliers according to Perten --------------------------------------------

p.out <- c("210000","210018","210020","210021","210023","210026","210031","210032")


dat2 %>%
  ggplot(aes(x=fct_reorder(factor(code),protein))) +
  geom_point(aes(y=protein)) +
  geom_vline(xintercept = p.out,
             linetype=3) +
  labs(title="Orange outliers at dotted line")

ndf48h.out <- c("210000","210002","210006","210007","210008","210012","210013","210018","210020","210021","210023","210026","210027","210028","210030","210031","210032")

dat2 %>%
  ggplot(aes(x=fct_reorder(factor(code),ndf48h))) +
  geom_point(aes(y=ndf48h)) +
  geom_vline(xintercept = ndf48h.out,
             linetype=2) +
  labs(title="Red outliers at dashed line")
# ^ still unclear what triggers outliers, but seems it is simply when values
# are outside a pre-determined range


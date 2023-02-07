

# Overview ----------------------------------------------------------------

# NIR data for Honken and Anderson 2021 from plots that were grazed


# tli data ----------------------------------------------------------------


tli.dat0 <- read_excel("Extensive Report_All-Products_20220119_0000_To_20220119_1449_avg.xlsx")

tli.dat1 <- tli.dat0 %>%
  mutate(datetime=`Date/Time of Analysis`,
         code=`Sample ID`,
         drymatter=`Dry matter %, Predicted`,
         protein=`Protein As is %, Predicted`,
         adf=`ADF As is %, Predicted`,
         ndf=`NDF As is %, Predicted`,
         ndf48h = `48dNDFr As is %, Predicted`,
         lignin=`Lignin As is %, Predicted`) %>%
  select(datetime, code, drymatter,protein,adf,ndf,ndf48h,lignin)


# extract tli from sample recorded on 20210709

tli.dat2 <- read_excel("Extensive Report_All-Products_20210601_0000_To_20210709_1017_avg.xlsx")

tli.dat3 <- tli.dat2 %>%
  mutate(datetime=`Date/Time of Analysis`,
         code=`Sample ID`,
         drymatter=`Dry matter %, Predicted`,
         protein=`Protein As is %, Predicted`,
         adf=`ADF As is %, Predicted`,
         ndf=`NDF As is %, Predicted`,
         ndf48h = `48dNDFr As is %, Predicted`,
         lignin=`Lignin As is %, Predicted`) %>%
  select(datetime, code, drymatter,protein,adf,ndf,ndf48h,lignin)
str(tli.dat3)
View(tli.dat3)
# all 10 samples were taken on 6/24
# tli.dat3 %>%
#   filter(datetime %in% "6/24/2021")
# using rowid to select

tli.dat3 <- tli.dat3 %>%
  rowid_to_column() 

tli.dat3 <- tli.dat3 %>%
  slice(706:735)

# join the two datasets
# make columns the same

tli.dat3 <- tli.dat3 %>%
  select(-rowid) 

tli.dat4 <- bind_rows(tli.dat1,tli.dat3)

# only using first observation for all sample id's for consistency
tli.dat4 %>%
  arrange(datetime,code) %>%
  distinct(code,
           .keep_all=T) %>%
  arrange(datetime,code) %>%
  View()

tli.dat5 <- tli.dat4 %>%
  arrange(datetime,code) %>%
  distinct(code,
           .keep_all=T) %>%
  arrange(datetime,code)

# splitting out code character
tli.dat5 %>%
  mutate(code.letter = str_sub(code,1,1),
         code.number = str_sub(code,2,3)) %>%
  select(code,code.letter,code.number)

tli.dat6 <- tli.dat5 %>%
  mutate(code.letter = str_sub(code,1,1),
         code.number = str_sub(code,2,3),
         experiment="TLI") %>%
  relocate(code.letter,code.number,
           .after=code) %>%
  relocate(experiment,
           .before=datetime)

tli.dat6 %>%
  arrange(code.letter,code.number) %>%
  View()

tli.dat <- tli.dat6 %>%
  arrange(code.letter,code.number) %>%
  select(-datetime)

write.csv(tli.dat, "tli.nir.csv")


# import ------------------------------------------------------------------

library(tidyverse)
library(readxl)

setwd("C:/Users/pukab001/Downloads")


dat <- read_excel("Extensive Report_All-Products_20210601_0000_To_20210709_1017_avg.xlsx")

str(dat)

# dat %>%
#   filter(`Product Name`=="Hay") %>%
#   View() 
# all datapoints after 8Jun using Hay equation are ours

dat1 <- dat %>%
  mutate(datetime=`Date/Time of Analysis`,
         code=as.numeric(`Sample ID`),
         drymatter=`Dry matter %, Predicted`,
         protein=`Protein As is %, Predicted`,
         adf=`ADF As is %, Predicted`,
         ndf=`NDF As is %, Predicted`,
         ndf48h = `48dNDFr As is %, Predicted`,
         lignin=`Lignin As is %, Predicted`) %>%
  # filter(code>10000) %>%
  select(datetime, code, drymatter,protein,adf,ndf,ndf48h,lignin) 

# Want to bring in plot number based on sample ID

codes <- read_excel("onfarmgrazingcodes.xlsx")
codes <- codes %>%
  mutate(code=`Bottle Code`)

dat2 <- merge(codes,dat1, by="code")

# want to confirm dataset is good
# view(dat2)


# RFQ ---------------------------------------------------------------------

dat3 <- dat2 %>%
  mutate(DM=drymatter,
            CP=protein,
            NDF=ndf,
            NDFD=ndf48h,
            ADF=adf,
            EE=2.05,
            FA=EE-1,
            Ash=100-DM,
            NFC=100-((0.93*NDF)+CP+EE+Ash),
            NDFn=NDF*0.93,
            NDFDp=22.7+0.664*NDFD,
            TDN=(NFC*.98)+(CP*.87)+(FA*.97*2.25)+(NDFn*NDFDp/100)-10,
            DMI=(-2.318)+(.442*CP)-(.01*CP^2)-(.0638*TDN)+(.000922*TDN^2)+(.18*ADF)-(0.00196*ADF^2)-(0.00529*CP*ADF),
            RFQ=DMI*TDN/1.23,
            RFV=DMI*((89.8-(0.779*ADF)))/1.29) %>%
  select(code,Year,Site,Experiment,Plot,Paddock,Treatment,datetime,
         RFQ,RFV,protein,adf,ndf)

# clean up data by timestamps ---------------------------------------------

library(lubridate)

dat3 <- dat3 %>%
  mutate(datetime=mdy_hms(datetime)) %>%
  mutate(time=hms::as_hms(datetime))

dat3 %>%
  arrange(time)

# for each code, I arrange them in order of time and 
# select the three plots that have the latest time stamp

dat3 %>%
  group_by(code) %>%
  tally() %>%
  arrange(desc(n))

dat3 %>%
  group_by(code) %>%
  arrange(desc(time)) %>%
  slice(1:3) %>%
  dplyr::select(code,time) %>%
  tally() %>%
  arrange(desc(n))

dat3 <- dat3 %>%
  group_by(code) %>%
  arrange(desc(time)) %>%
  slice(1:3) %>%
  ungroup()


# visualize ---------------------------------------------------------------

library(lattice)


bwplot(~RFQ,dat3)
bwplot(~RFV,dat3)
bwplot(~protein,dat3)
bwplot(~adf,dat3)
bwplot(~ndf,dat3)


bwplot(RFQ~factor(Treatment)|Site,dat3)
bwplot(RFV~factor(Treatment)|Site,dat3)
bwplot(protein~factor(Treatment)|Site,dat3)
bwplot(adf~factor(Treatment)|Site,dat3)
bwplot(ndf~factor(Treatment)|Site,dat3)
# RFQ and RFV seem higher in grazed plots at Anderson
# ADF and NDF seem lower in grazed plots at Anderson...



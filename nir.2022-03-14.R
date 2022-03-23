# Jesse
# last updated: 2022-03-14
# obj: import and tidy data exported by Perten NIRS

## key points
# Perten predicts forage quality parameters from a HAY equation
# HAY equation was developed at UMN through validation with wet chemistry
# The predicted parameters of Perten also contain an "M-Distance"
# M-distance = Mahalanobis distance
# when M-distance is greater than 5, this may indicate an inaccurate prediction
# all Perten predictions are on an 'as is' basis
# relative forage quality (rfq) is calculated on a dry matter basis
# relative feed value can be calculated on 'as is' or dry matter basis

# filetype needed: Extensive Reports excel files from Perten

rm(list=ls())
library(tidyverse)
library(googlesheets4)


# orei-manure -------------------------------------------------------------



# example 2022-03-14 --------------------------------------------------

##practice file
# setwd("C:/Users/pukab001/Downloads")
# library(tidyverse)
# dat <- readxl::read_xlsx("Extensive Report_All-Products_20220126_0000_To_20220126_1513_avg.xlsx")

# read a google sheet with Perten outputs
# filename column is the name of the excel sheet the data is from
dat <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1VSV9gEvTs_igB9tfSlKk9h3KSdDSkB0yURe08qbr2BY/edit#gid=0")


# selecting data we want from Perten extensive report
# changing data to a % dry matter basis
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

# calculating rfv and rfq
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

# filtering to one datafile, visualizing range of responses
dat2 %>% 
  dplyr::filter(filename == "Extensive Report_All-Products_20220126_0000_To_20220126_1513_avg") %>%
  pivot_longer(cols = c("rfq","rfv"),
               names_to = "name",
               values_to = "value") %>% 
  ggplot(aes(value,
             col=name,
             fill=name)) +
  geom_density(alpha=.5,
               bw=6) +
  geom_vline(xintercept = 90,
             linetype=2)

# RFQ ranges #http://gsdc.com/news/all-hay-is-not-created-equal-using-the-relative-forage-quality-index
# RFQ ranges from 100:160


## M-distance can be used as criterion to filter data
# Mahalanobis distances are saved within extensive reports for reference
md_dat1 <- dat %>% 
  rename_all(.,tolower) %>% 
  filter(`product name`=="Hay") %>%
  mutate(datetime=`date/time of analysis`,
         code=`sample id`,
         drymatter=`dry matter %, predicted`,
         protein=`protein as is %, predicted`,
         adf=`adf as is %, predicted`,
         ndf=`ndf as is %, predicted`,
         ndf48h = `48dndfr as is %, predicted`,
         md_drymatter=`m-distance actual dry matter`,
         md_protein = `m-distance actual protein`,
         md_adf = `m-distance actual adf`,
         md_ndf = `m-distance actual ndf`,
         md_ndf48h = `m-distance actual 48dndfr`) %>%
  select(filename,datetime, code, drymatter,protein,adf,ndf,ndf48h,
         md_drymatter,md_protein,md_adf,md_ndf,md_ndf48h
         ) %>%
  mutate(datetime=as.POSIXct(datetime,
                             format="%m/%d/%Y %H:%M:%S %p")) %>% 
  mutate(protein=protein*drymatter/100,
         adf=adf*drymatter/100,
         ndf=ndf*drymatter/100,
         ndf48h=ndf48h*drymatter/100)

# calculating rfv and rfq
md_dat2 <- md_dat1 %>% 
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

md_dat2 %>% 
  # filter(md_ndf48h<5) %>% 
  ggplot(aes(md_ndf48h)) +
  stat_bin()
# we have some high ndf48h values

md_dat2 %>% 
  filter(md_ndf48h<5) %>%
  filter(md_drymatter<5) %>%
  filter(md_protein<5) %>%
  filter(md_adf<5) %>%
  # tally(rfq)
  ggplot(aes(rfq)) +
  stat_bin()
(15719-8948)/15719
# 43% of plots would be removed if mahalanobis<5 criteria was used


# pre 27feb ---------------------------------------------------------------

# D:// is flash drive
# dat <- readxl::read_xlsx(file.choose())
dat <- readxl::read_xlsx("D://Extensive Report_All-Products_20211201_0000_To_20220207_1529_avg.xlsx")
# Extensive Report_All-Products_20211201_0000_To_20220207_1529_avg

library(tidyverse)

dat1 <- dat %>%
  filter(`Product Name`=="Hay") %>%
  mutate(datetime=`Date/Time of Analysis`,
         code=`Sample ID`,
         drymatter=`Dry matter %, Predicted`,
         protein=`Protein As is %, Predicted`,
         adf=`ADF As is %, Predicted`,
         ndf=`NDF As is %, Predicted`,
         ndf48h = `48dNDFr As is %, Predicted`,
         lignin=`Lignin As is %, Predicted`) %>%
  select(datetime, code, drymatter,protein,adf,ndf,ndf48h,lignin) %>%
  mutate(datetime=as.POSIXct(datetime,
                             format="%m/%d/%Y %H:%M:%S %p"))

getwd()
setwd("C:/Users/pukab001/Downloads")
bottle_codes <- read.csv("maintenance_bottle-codes.csv",
                         header = T)

bottle_codes[,1]
bottle_codes <- bottle_codes %>%
  mutate(bottle_code=bottle_codes[,1]) 
# bottle_codes <- bottle_codes %>%
#   dplyr::select(-1)

dat1 %>%
  filter(datetime>as.POSIXct("2022-02-06")) %>%
  # dplyr::select(code) %>%
  mutate(code.num = str_sub(code,0,3),
         .after=code) %>%
  # View()
  mutate(code.side = str_sub(code,5),
         .after=code.num) %>%
  # distinct(code.side)
  arrange(code.num) %>%
  write.csv("nir_2022-02-07.csv")
# manually added in north and south to some
# 418 needs rescan for both north and south 
# 409 needs rescan for both north and south
# 401 is missing data, may need rescan for both

dat1 <- read.csv("nir_2022-02-07.csv") %>%
  mutate(datetime=as.POSIXct(datetime,
                             format="%m/%d/%Y %H:%M"));str(dat1)

dat1 <- dat1 %>%
  mutate(code.side=recode(code.side,
                          s="south",
                          n="north")) %>%
  mutate(plot_side=paste(code.num,code.side,sep = "."))

bottle_codes <- bottle_codes %>%
  # str()
  mutate(plot_side=paste(plot,sample_side,sep = "."),
         plot_side=str_to_lower(plot_side))


dat_maintenance <- dat1 %>%
  left_join(bottle_codes,dat1,by="plot_side") %>%
  dplyr::select(bottle_code,study,location,year,plot,sample_side,plot_side,
                main_plot_treatment,
                # datetime,
                drymatter,protein,adf,ndf,ndf48h,lignin) %>%
  # rename(datetime.of.scanning=datetime) %>%
  drop_na()



dat_maintenance %>%
  pivot_longer(cols=c(protein,adf,ndf,ndf48h,lignin),
               names_to = "forage.quality.factors",
               values_to = "NIRS.prediction") %>%
  ggplot(aes(main_plot_treatment,
             NIRS.prediction,
             group=forage.quality.factors,
             color=forage.quality.factors,
             shape=forage.quality.factors)) +
  stat_summary() +
  labs(y="NIRS prediction (% as is)")
# note that fall herbicide has high protein, low ndf and adf and ndfd
# RFQ weights ndfd more, so samples with lower ndfd have lower RFQ


# Equation used for FIG
# note all data in FIG was converted from 'as is' to dry matter basis prior to being read into R
# this equation will need to be adapted, but first just seeing general outputs
dat_maintenance %>%
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
         rfv=DMI*((89.8-(0.779*ADF)))/1.29) %>%
  select(rfq,rfv,bottle_code) %>%
  left_join(dat_maintenance,.) %>%
  pivot_longer(cols = c(rfq,rfv),
               names_to = "quality.metrics",
               values_to = "quality.prediction") %>%
  # dplyr::select(quality.metrics)
  ggplot(aes(main_plot_treatment)) +
  stat_summary(aes(y=quality.prediction,
                   col=quality.metrics))
# RFV and RFQ range is normal
# RFV: low adf and ndf should equal high RFV, as forage is more digestible
# RFQ: high ndfd and CP should result in high RFQ, as forage is more nutritious
# based on above criteria, strange that herbicides treatments don't have highest RFV


# going to recalculate RFV myself vs. using FIG code Jake gave me
# https://www.foragelab.com/Media/Relative_Forage_Quality.pdf
# use grass equation
# https://www.foragelab.com/Media/RFV_vs_RFQ-CVAS%20Perspective.pdf

dat_maintenance %>%
  mutate(DM=drymatter,
         NDF=ndf*DM/100,
         ADF=adf*DM/100,
         DMI=120/NDF,
         DDM= 88.9-(0.779*ADF),
         RFV=DMI*DDM/1.29) %>%
  ggplot()+
  stat_summary(aes(main_plot_treatment,
                   RFV))
# these values seem more correct for RFV
# I THINK SOMETHING MAY BE WRONG WITH CODE USED FOR CALCULATING RFQ AND RFV FROM LEGACYNET 
# need to review equation used from FIG


# 

dat_maintenance %>%
  mutate(DM=drymatter,
         NDF=ndf*DM/100,
         ADF=adf*DM/100,
         # DMI=120/NDF,
         DDM= 88.9-(0.779*(ADF)),
         # RFV=DMI*DDM/1.29,
         CP=protein*DM/100,
         NDFD=ndf48h*DM/100,
         EE=2.05, #2.05 is constant
         FA=EE-1,
         Ash=100-DM,
         NFC=100-((0.93*NDF)+CP+EE+Ash), #everything already %DM in this equation
         NDFn=NDF*0.93,
         NDFDp=22.7+0.664*NDFD,
         TDN=((NFC*.98)+(CP*.87)+(FA*.97*2.25)+(NDFn*NDFDp/100)-10),
         DMI=(-2.318)+(0.442*CP)-(.01*CP^2)-(.0638*TDN)+(.000922*TDN^2)+
           (.18*ADF)-(0.00196*ADF^2)-(0.00529*CP*ADF),
         RFQ=DMI*TDN/1.23,
         RFV=DMI*DDM/1.29) %>%
  # group_by(main_plot_treatment) %>%
  # summarise(dmi=mean(DMI),
  # TDN=mean(TDN))  %>%
  # arrange(TDN) # TDN values make sense, why is DMI so much lower
  # summarise(nfc=mean(NFC),
  #           cp=mean(CP),
  #           fa=mean(FA),
  #           NDFn=mean(NDFn),
  #           NDFDp=mean(NDFDp)) #NFC is the problem
  # summarise(ndf=mean(NDF),
  #           cp=mean(CP),
  #           ash=mean(Ash))
  ggplot()+
  stat_summary(aes(main_plot_treatment,
                   RFQ)) +
  stat_summary(aes(main_plot_treatment,
                   RFV),
               col="purple")
# RFV and RFQ values should be very closely correlated, and they are not
# RFQ is lowest for fall herbicide
# Fall herbicide treatment has the highest protein content
# Fall herbicide should be the highest RFQ
dat_maintenance %>%
  group_by(main_plot_treatment) %>%
  summarise(protein.mean=mean(protein),
            TDN.mean=mean(TDN)) %>%
  arrange(protein.mean)




# pre28Jan -------------------------------------------------------------

dat <- readxl::read_xlsx(file.choose())
# D:\
# Extensive Report_All-Products_20220119_0000_To_20220119_1449_avg

str(dat)

library(tidyverse)

dat1 <- dat %>%
  filter(`Product Name`=="Hay") %>%
  mutate(datetime=`Date/Time of Analysis`,
         code=`Sample ID`,
         drymatter=`Dry matter %, Predicted`,
         protein=`Protein As is %, Predicted`,
         adf=`ADF As is %, Predicted`,
         ndf=`NDF As is %, Predicted`,
         ndf48h = `48dNDFr As is %, Predicted`,
         lignin=`Lignin As is %, Predicted`) %>%
  select(datetime, code, drymatter,protein,adf,ndf,ndf48h,lignin)

dat1 %>%
  # str()
  ggplot() +
  # geom_boxplot(aes(drymatter)) # dry matter outliers maybe
  # geom_boxplot(aes(protein))
  # geom_boxplot(aes(adf)) 
  # geom_boxplot(aes(ndf48h))
  geom_boxplot(aes(lignin)) 

dat2 <- dat1 %>%
  mutate(DM=drymatter,
         CP=protein,
         NDF=ndf,
         NDFD=ndf48h,
         ADF=adf,
         EE=2.05,
         FA=EE-1,
         Ash=100-DM,
         NFC<-100-((0.93*NDF)+CP+EE+Ash),
         NDFn<-NDF*0.93,
         NDFDp<-22.7+0.664*NDFD,
         TDN<-(NFC*.98)+(CP*.87)+(FA*.97*2.25)+(NDFn*NDFDp/100)-10,
         DMI<-(-2.318)+(.442*CP)-(.01*CP^2)-(.0638*TDN)+(.000922*TDN^2)+
           (.18*ADF)-(0.00196*ADF^2)-(0.00529*CP*ADF),
         rfq=DMI*TDN/1.23,
         rfv=DMI*((89.8-(0.779*ADF)))/1.29) %>%
  select(rfq,rfv,code) %>%
  left_join(dat1,.);dat2

dat2 %>%
  ggplot() +
  geom_boxplot(aes(rfq))
  geom_boxplot(aes(rfv))

# all good, nothing that looks really wrong


# 24jan-28jan -------------------------------------------------------------

dat <- readxl::read_xlsx(file.choose())
# D:\
# Products_20220124_0000_To_20220128_1507_avg


# filter data to our project
# maybe try serial number, all ours are 5 or 6 digits
# Product Name = Hay


str(dat)

library(tidyverse)



dat1 <- dat %>%
  filter(`Product Name`=="Hay") %>%
  mutate(datetime=`Date/Time of Analysis`,
         code=`Sample ID`,
         drymatter=`Dry matter %, Predicted`,
         protein=`Protein As is %, Predicted`,
         adf=`ADF As is %, Predicted`,
         ndf=`NDF As is %, Predicted`,
         ndf48h = `48dNDFr As is %, Predicted`,
         lignin=`Lignin As is %, Predicted`) %>%
  select(datetime, code, drymatter,protein,adf,ndf,ndf48h,lignin)

dat1 %>%
  # str()
  ggplot() +
  # geom_boxplot(aes(drymatter))
  # geom_boxplot(aes(protein))
  # geom_boxplot(aes(adf)) #adf outlier
  # geom_boxplot(aes(ndf48h))
  geom_boxplot(aes(lignin)) #lignin outliers

dat2 <- dat1 %>%
  mutate(DM=drymatter,
         CP=protein,
         NDF=ndf,
         NDFD=ndf48h,
         ADF=adf,
         EE=2.05,
         FA=EE-1,
         Ash=100-DM,
         NFC<-100-((0.93*NDF)+CP+EE+Ash),
         NDFn<-NDF*0.93,
         NDFDp<-22.7+0.664*NDFD,
         TDN<-(NFC*.98)+(CP*.87)+(FA*.97*2.25)+(NDFn*NDFDp/100)-10,
         DMI<-(-2.318)+(.442*CP)-(.01*CP^2)-(.0638*TDN)+(.000922*TDN^2)+
           (.18*ADF)-(0.00196*ADF^2)-(0.00529*CP*ADF),
         rfq=DMI*TDN/1.23,
         rfv=DMI*((89.8-(0.779*ADF)))/1.29) %>%
  select(rfq,rfv,code) %>%
  left_join(dat1,.);dat2

dat2 %>%
  ggplot() +
  # geom_boxplot(aes(rfq))
  geom_boxplot(aes(rfv))


# outliers ----------------------------------------------------------------

# adf ---
dat2 %>%
  ggplot()+
  geom_boxplot(aes(adf))
# two high values
dat2 %>%
  dplyr::select(code,adf) %>%
  arrange(desc(adf)) %>%
  slice(1:2)
# 210469 is only outlier of concern

# lignin
dat2 %>%
  ggplot()+
  geom_boxplot(aes(lignin))

dat2 %>%
  dplyr::select(code,lignin) %>%
  arrange(desc(lignin)) %>%
  slice(1:2)
# 210469 is only outlier of concern

# rfq
dat2 %>%
  ggplot()+
  geom_boxplot(aes(rfq))

dat2 %>%
  dplyr::select(code,rfq) %>%
  arrange(rfq)
# 210336 very off
# other issues with ~332,329,334

# rfv
dat2 %>%
  dplyr::select(code,rfv) %>%
  ggplot()+
  geom_boxplot(aes(rfv))

dat2 %>%
  dplyr::select(code,rfv) %>%
  arrange(rfv) %>%
  count(code) %>%
  arrange(desc(n))
# ^confirming there aren't duplicate values


# conclusions -------------------------------------------------------------

# bottle code 210336 needs to be re-run
# bottle code 210469 would also benefit

dat2 %>%
  dplyr::select(datetime,code) %>%
  write.csv("scanning_jan_1.csv")



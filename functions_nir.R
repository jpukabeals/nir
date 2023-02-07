# Function to tidy NIR output data
# equations taken from Moore and Undersander 2002
# see https://www.foragelab.com/Media/Relative_Forage_Quality.pdf
# see http://www.southdakotaagriculturallaboratories.com/uploads/1/3/5/2/13521518/exex8149_understanding_rfv_and_rfq.pdf
# SDSU publication used for RFV caluclation



# library(googlesheets4)
library(tidyverse)

# NIR data
# googledrive link
# url <- "https://docs.google.com/spreadsheets/d/1z2B9jrX-pGm9YaUfVRTFczP-mI7UCfepGMw21QiH8f8/edit#gid=0"
# dat <- read_sheet(url,
#                   gs4_deauth())

tidy.nir.report.with.spaces <- function(perten_extensive_report) {
  # library(dplyr)
  perten_extensive_report %>% 
    rename_all(.,tolower) %>% 
    filter(`product name`=="Hay") %>%
    mutate(datetime=`date/time of analysis`,
           code=`sample id`,
           drymatter=`dry matter %, predicted`,
           protein=`protein as is %, predicted`,
           adf=`adf as is %, predicted`,
           ndf=`ndf as is %, predicted`,
           ndf48h = `48dndfr as is %, predicted`) %>%
    select(datetime, code, drymatter,protein,adf,ndf,ndf48h) %>%
    mutate(datetime=as.POSIXct(datetime,
                               format="%m/%d/%Y %H:%M:%S %p")) %>% 
    mutate(protein=protein*drymatter/100,
           adf=adf*drymatter/100,
           ndf=ndf*drymatter/100,
           ndf48h=ndf48h*drymatter/100) %>% 
    return()
}

tidy.nir.report.with.spaces.predicted.first <- function(perten_extensive_report) {
  # library(dplyr)
  perten_extensive_report %>% 
    rename_all(.,tolower) %>% 
    filter(`product name`=="Hay") %>%
    mutate(datetime=`date/time of analysis`,
           code=`sample id`,
           drymatter=`predicted dry matter %`,
           protein=`predicted protein as is %`,
           adf=`predicted adf as is %`,
           ndf=`predicted ndf as is %`,
           ndf48h = `predicted 48dndfr as is %`) %>%
    select(datetime, code, drymatter,protein,adf,ndf,ndf48h) %>%
    mutate(datetime=as.POSIXct(datetime,
                               format="%m/%d/%Y %H:%M:%S %p")) %>% 
    mutate(protein=protein*drymatter/100,
           adf=adf*drymatter/100,
           ndf=ndf*drymatter/100,
           ndf48h=ndf48h*drymatter/100) %>% 
    return()
}


tidy.nir.report.with.periods <- function(perten_extensive_report) {
  # library(dplyr)
  perten_extensive_report %>% 
    rename_all(.,tolower) %>% 
    filter(`product.name`=="Hay") %>%
    mutate(datetime=`date.time.of.analysis`,
           code=`sample.id`,
           drymatter=dry.matter....predicted,
           protein=`protein.as.is....predicted`,
           adf=`adf.as.is....predicted`,
           ndf=`ndf.as.is....predicted`,
           ndf48h = x48dndfr.as.is....predicted) %>%
    select(datetime, code, drymatter,protein,adf,ndf,ndf48h) %>%
    mutate(datetime=as.POSIXct(datetime,
                               format="%m/%d/%Y %H:%M:%S %p")) %>% 
    mutate(protein=protein*drymatter/100,
           adf=adf*drymatter/100,
           ndf=ndf*drymatter/100,
           ndf48h=ndf48h*drymatter/100) %>% 
    return()
}

# Do not use this function on forages containing legumes!
calc.rfq.rfv.grass <- function(nir_tidy_data) {
  nir_tidy_data %>% 
    mutate(DM=drymatter,
           CP=protein,
           NDF=ndf,
           NDFD=ndf48h,
           ADF=adf,
           EE=2.05, #2.05 is constant, extractable ether
           FA=EE-1,
           Ash=100-DM,
           NFC=100-((0.93*NDF)+CP+EE+Ash),
           NDFn=NDF*0.93, 
           NDFDp=22.7+0.664*NDFD,
           TDN.grass=(NFC*.98)+(CP*.87)+(FA*.97*2.25)+(NDFn*NDFDp/100)-10,
           DMI.grass=(-2.318)+(.442*CP)-(.01*CP^2)-(.0638*TDN)+(.000922*TDN^2)+
             (.18*ADF)-(0.00196*ADF^2)-(0.00529*CP*ADF),
           rfq.grass=DMI.grass*TDN.grass/1.23,
           DDM.rfv = 88.9-(.779*ADF),
           DMI.rfv = 120/NDF,
           rfv= DDM.rfv*DMI.rfv/1.29,
           rfv.old=DMI.grass*((89.8-(0.779*ADF)))/1.29)
}

calc.rfq.rfv.legume <- function(nir_tidy_data) {
  nir_tidy_data %>% 
    mutate(DM=drymatter,
           CP=protein,
           NDF=ndf,
           NDFD=ndf48h,
           ADF=adf,
           EE=2.05, #2.05 is constant, extractable ether
           FA=EE-1,
           Ash=100-DM,
           NFC=100-((0.93*NDF)+CP+EE+Ash),
           NDFn=NDF*0.93, 
           NDFDp=22.7+0.664*NDFD,
           TDN.legume=(NFC*.98)+(CP*.93)+(FA*.97*2.25)+(NDFn*NDFD/100)-7,
           DMI.legume=(120/NDF) + (NDFD - 45) * .374/1350*100,
           rfq.legume=DMI.legume*TDN/1.23,
           DDM.rfv = 88.9-(.779*ADF),
           DMI.rfv = 120/NDF,
           rfv= DDM.rfv*DMI.rfv/1.29,
           rfv.old=DMI.grass*((89.8-(0.779*ADF)))/1.29)
}




# Overview ----------------------------------------------------------------

# NIR data for Honken and Anderson 2021 from plots that were grazed


# import ------------------------------------------------------------------

library(tidyverse)
library(readxl)

dat <- read_excel("Summary Report_All-Products_20210617_0000_To_20210617_1137_avg.xlsx")

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

codes <- read_excel("2021 Bottle Codes.xlsx")
codes <- codes %>%
  mutate(code=`Bottle Code`)

dat2 <- merge(codes,dat1, by="code")

# want to confirm dataset is good
unique(dat2$Treatment)


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
  arrange(desc(n)) %>%
  slice(1:4) %>%
  select(code)


# dat2 %>%
#   select(code,time) %>%
#   filter(code==210023 & time >= 11:11:00)
# ^ couldn't figure out how to filter by time objects in lubridate

# dirtier option

dat2 <- dat2 %>%
  arrange(time) %>%
  mutate(id=row_number(), 
         timen=as.numeric(time)) 

# 210023
dat2 %>%
  filter(code==210023) %>%
  arrange(desc(timen)) %>%
  select(code,time,timen,id)
# 36:38

# 210009
dat2 %>%
  filter(code==210009) %>%
  arrange(desc(timen)) %>%
  select(code,time,timen,id)
# 10:11 

# 210027
dat2 %>%
  filter(code==210027) %>%
  arrange(desc(timen)) %>%
  select(code,time,timen,id)
# 42

# 210028
dat2 %>%
  filter(code==210028) %>%
  arrange(desc(timen)) %>%
  select(code,time,timen,id)
# 46

dat2 %>%
  filter(id!=46&id!=42&id!=10&id!=11&id!=36&id!=37&id!=38) %>%
  tally()

dat2 %>%
  tally()
# ^worked!

dat2 <- dat2 %>%
  filter(id!=46&id!=42&id!=10&id!=11&id!=36&id!=37&id!=38)


# visualize w/ jake ---------------------------------------------------------------

dat2 %>%
  ggplot(aes(factor(Plot))) +
  geom_point(aes(y=protein)) +
  facet_wrap(~Site)

# high is 304H, 105H
# low is 205H


dat2 %>%
  ggplot(aes(factor(Plot))) +
  geom_point(aes(y=adf))+
  facet_wrap(~Site)

# high is 205H
# Low is A306 A105

dat2 %>%
  ggplot(aes(factor(Plot))) +
  geom_point(aes(y=ndf))+
  facet_wrap(~Site)

dat2 %>%
  ggplot(aes(factor(code))) +
  geom_point(aes(y=ndf48h))

# high H205, H106
# low A306. H105

dat2 %>%
  ggplot(aes(factor(Plot))) +
  geom_point(aes(y=lignin))

dat2 %>%
  ggplot()+
  geom_density(aes(x=lignin))

dat %>%
  ggplot()+
  geom_density(aes(x=`Lignin As is %, Predicted`))

dat %>%
  ggplot()+
  geom_point(aes(x=`Product Name`,y=`Lignin As is %, Predicted`))


# mutate a plot+site variable ---------------------------------------------

library(stringr)

dat2 <- dat2 %>%
  mutate(s=str_sub(Site,1,1),
         p=Plot,
         site_plot=str_c(s,p))


# Outliers according to Perten --------------------------------------------

dat2 %>%
  ggplot(aes(x=fct_reorder(factor(code),protein))) +
  geom_point(aes(y=protein)) +
  geom_vline(xintercept = c("210021","210023"),
             linetype=3) +
  labs(title="Orange outliers at dotted line")

dat2 %>%
  ggplot(aes(x=fct_reorder(factor(code),ndf48h))) +
  geom_point(aes(y=ndf48h)) +
  geom_vline(xintercept = c("210021","210023","210027","210028"),
             linetype=2) +
  labs(title="Red outliers at dashed line")
# ^ still unclear what triggers outliers, but seems it is simply when values
# are outside a pre-determined range


# High Low and middle plots -----------------------------------------------


dat2 %>%
  ggplot(aes(x=fct_reorder(factor(site_plot),protein))) +
  geom_point(aes(y=protein))
# L:H205
# H:H105,H304,A205

dat2 %>%
  ggplot(aes(x=fct_reorder(factor(site_plot),adf))) +
  geom_point(aes(y=adf))
# L:A306,A105,H105
# H:H205,H106

dat2 %>%
  ggplot(aes(x=fct_reorder(factor(site_plot),ndf))) +
  geom_point(aes(y=ndf))
# L: A306,H105
# H: H206,H106,H104

dat2 %>%
  ggplot(aes(x=fct_reorder(factor(site_plot),ndf48h))) +
  geom_point(aes(y=ndf48h))
# L:A306,A105
# H:H106,H205,H104

# Extreme samples: H205(n=3,L1H2), H105(n=3,L2H1),A306(n=3,L3),H106(n=3,H3)
# Extreme sample final: A306 for Low, H106 for High

# lignin
dat2 %>%
  ggplot(aes(x=fct_reorder(factor(site_plot),lignin))) +
  geom_point(aes(y=lignin))

dat2 %>%
  filter(site_plot=="A105") %>%
  select(lignin,code)
# ^no outlier alert occured despite negative lignin values
# Change low from A306 to A105 because of lignin value

# Middle values
# A305,H204,A106,H306
# Low value = A105
# High value = H106


# nmds --------------------------------------------------------------------

# install.packages("vegan")
library(vegan)

ord.values <- dat2 %>%
  select(drymatter,protein,adf,ndf,ndf48h,lignin)

ord.e.k2 <- metaMDS(ord.values, k=2, dist="euclidean",
                    trymax = 50)

ord.env <- dat2 %>%
  select(site_plot)

library(RVAideMemoire)

# pairwise.factorfit(ord.e.k2, ord.env$site_plot, 
#                    nperm = 999, p.method = "none")


plot(ord.e.k2, type="p", main="NIR dissimilarity") 
plot(ord.e.k2, type="t", main="NIR dissimilarity") 
plot(ord.e.k2, type="n", main="NIR dissimilarity") 


ordispider(ord.e.k2, groups=ord.env$site_plot, label=T)

ordiellipse(ord.e.k2, groups=ord.env$site_plot, label=T)

plot(ord.e.k2, type="n", main="NIR dissimilarity") 
ordiellipse(ord.e.k2, groups=ord.env$site_plot,kind="sd",label=T)


plot(ord.e.k2, type="n", main="NIR dissimilarity") 
ordibar(ord.e.k2, groups=ord.env$site_plot, label=T,col = 0)

# ^boom!

# plot(envfit(ord.e.k2 ~ site_plot, data=ord.env, permu=999), p.max=0.10, col="blue")


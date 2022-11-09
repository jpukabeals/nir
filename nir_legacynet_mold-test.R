
# Intro -------------------------------------------------------------------

# We took subsamples from plots after our last forage harvest (Sep2022).
# we dried these samples in paper bags at 140F, but they molded
# to what extent does the mold impact the NIR predcitions?

# we randomly selected 3 bags that contained rotted biomass
# on the top and edges of the forage mass the sorghum sudangrass had not molded
# in the center there was mold
# in between the edges and middle there was a mixture of mold

# There are 3 levels of mold. 

# 1 = no mold. These were the dried green leaves at edge of sample that dried down
# fast and did not mold

# 2 = mixture of mold and no mold. If we had left these samples in the oven for weeks, the
# sample would've eventually dried and the center would've been blacker in color
# and the outside greener. I would've just grabbed a mix of these samples, this
# is what 2 represents

# 3 = mold. Sample was taken from center. Dark in color

# There are 3 plots, randomly selected. All had been dried for 1 week at 140F
# and shuffled a number of times and still molded


# Import data -------------------------------------------------------------

read.csv("nir_legacynet_20Sep2022.csv") -> dat

source("functions_nir.R")

tidy.nir.report.with.periods(dat) -> dat_tidy

library(stringr)

dat_tidy %>% 
  # glimpse()
  dplyr::select(code) %>% 
  mutate(plot = str_sub(code,1,2),
         quality.num = str_sub(code,-1)) %>% 
  mutate(quality.num = as.factor(quality.num)) %>% 
  # glimpse()
  mutate(quality.text = fct_recode(quality.num,
                           "no mold" = "1",
                           "some mold" = "2",
                           "moldy" = "3")) %>% 
  left_join(.,dat_tidy) -> dat2

calc.rfq.rfv.grass(dat2) -> dat3


# EDA ---------------------------------------------------------------------


dat3 %>% 
  ggplot(aes(rfq)) +
  stat_bin()

dat3 %>% 
  ggplot(aes(rfq)) +
  geom_boxplot()

dat3 %>% 
  ggplot(aes(rfq)) +
  geom_boxplot(aes(fill=quality.text))

dat3 %>% 
  ggplot(aes(rfq)) +
  geom_boxplot(aes(fill=quality.text)) +
  facet_wrap(~plot)

dat3 %>% 
  ggplot(aes(quality.text,rfq)) +
  stat_summary(aes(color=plot),
               position = position_dodge(.5)) 


# Graph output ------------------------------------------------------------
source("ggplot_custom_theme.R")

theme_set(theme_jpb.light())

dat3 %>% 
  ggplot(aes(quality.text,rfq)) +
  stat_summary(aes(color=plot),
               position = position_dodge(.5)) +
  labs(x="",
       y="Relative forage quality") +
  theme(legend.title = element_text(size = 14),
        legend.title.align = .5) 

ggsave("rfq_legacynet-subsample.png",
       dpi = 400,
       width = 6,
       height = 4,
       units = "in")

dat3 %>% 
  ggplot(aes(quality.text,rfv)) +
  stat_summary(aes(color=plot),
               position = position_dodge(.5)) +
  labs(x="",
       y="Relative feed value") +
  theme(legend.title = element_text(size = 14),
        legend.title.align = .5)

ggsave("rfv_legacynet-subsample.png",
       dpi = 400,
       width = 6,
       height = 4,
       units = "in")

dat3 %>% 
  ggplot(aes(quality.text,protein)) +
  stat_summary(aes(color=plot),
               position = position_dodge(.5)) +
  labs(x="",
       y="Protein (%, predicted)") +
  theme(legend.title = element_text(size = 14),
        legend.title.align = .5)

ggsave("protein_legacynet-subsample.png",
       dpi = 400,
       width = 6,
       height = 4,
       units = "in")

dat3 %>% 
  ggplot(aes(quality.text,adf)) +
  stat_summary(aes(color=plot),
               position = position_dodge(.5)) +
  labs(x="",
       y="ADF (%, predicted)") +
  theme(legend.title = element_text(size = 14),
        legend.title.align = .5)

ggsave("adf_legacynet-subsample.png",
       dpi = 400,
       width = 6,
       height = 4,
       units = "in")

dat3 %>% 
  ggplot(aes(quality.text,ndf)) +
  stat_summary(aes(color=plot),
               position = position_dodge(.5)) +
  labs(x="",
       y="NDF (%, predicted)") +
  theme(legend.title = element_text(size = 14),
        legend.title.align = .5)

ggsave("ndf_legacynet-subsample.png",
       dpi = 400,
       width = 6,
       height = 4,
       units = "in")

dat3 %>% 
  ggplot(aes(quality.text,ndf48h)) +
  stat_summary(aes(color=plot),
               position = position_dodge(.5)) +
  labs(x="",
       y="NDFD 48h (%, predicted)") +
  theme(legend.title = element_text(size = 14),
        legend.title.align = .5)

ggsave("ndfd48h_legacynet-subsample.png",
       dpi = 400,
       width = 6,
       height = 4,
       units = "in")

dat3 %>% 
  glimpse()

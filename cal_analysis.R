# TESTING VPR MEASUREMENT WORKFLOW ---- 

# This workflow is meant as a sample of
# the VPR measurement process. It is meant to accompany an ImageJ workflow where
# ROI images are measured. This section of the workflow brings those
# measurements in for processing and analaysis. The data chosen for this example
# are vpr10 and vpr34 from COR2019002. The data were chosen to be representative
# of different Calanus species distributions. Where vpr10 is mostly Chyp, and
# vpr34 is mostly Cfin. In this analysis I will attempt to differentiate species
# composition based on size analysis of VPR ROIs


# E. OGrady July 2022

librarian::shelf(vprr, tidyverse, oce, ggplot2, multimode, plyr)

# identify calanus from sample stations ----
# # create fake aid files for each station
# cgr16_dir <- 'c:/data/cruise_COR2019002/rois/vpr34/'
# w44_dir <- 'c:/data/cruise_cor2019002/rois/vpr10'
# 
# aiddat_cgr16 <- list.files(cgr16_dir, recursive= TRUE, pattern = '.tif', full.names = TRUE)
# aiddat_w44 <- list.files(w44_dir, recursive = TRUE, pattern = '.tif', full.names = TRUE)
# 
# fn_cgr16 <- 'data/CGR1-6/aid/unknown_aid.d0.h0'
# fn_w44 <- 'data/W44/aid/unknown_aid.d0.h0'
# 
# write.table(
#   file = fn_cgr16,
#   aiddat_cgr16,
#   quote = FALSE,
#   col.names = FALSE, 
#   row.names = FALSE
# )
# 
# write.table(
#   file = fn_w44,
#   aiddat_w44,
#   quote = FALSE,
#   col.names = FALSE, 
#   row.names = FALSE
# )
# 
# # create new categories (calanus and not calanus)
# vpr_category_create('calanus', basepath = 'data/CGR1-6/')
# vpr_category_create('not_calanus', basepath = 'data/CGR1-6/')
# vpr_category_create('calanus', basepath = 'data/W44/')
# vpr_category_create('not_calanus', basepath = 'data/W44/')
# 
# # copy image folders into unknown rois folder in autoid path
# 
# 
# # manually sort ROIs
# vpr_manual_classification(day = 0, hour = 0,
#                           basepath = 'c:/Users/ChisholmE/Documents/GitHub/vpr_meas/data/W44/autoid/',
#                           taxa_of_interest = 'unknown',
#                           gr = FALSE)

# pull pre-classified Calanus ----
# all aid files
aid_files <- list.files('c:/NAS/data/COR2019002/autoid_COR2019002/Calanus/', recursive = TRUE, pattern = 'new_aid', full.names = TRUE)
# select by station (d/h)
valid_dh <- c('d242.h10', 'd242.h11', 'd225.h04', 'd225.h05')
valid_aids <- list()
for(dh in valid_dh){
  valid_aids[[dh]] <- grep(aid_files, pattern = dh, value = TRUE)
}

# copy images into folders
for(i in 1:length(valid_aids)){
  dh <- names(valid_aids[i])
  aid_fn <- valid_aids[i][[1]][1]
  
  aidlist <- read.table(aid_fn)
  
  if(dh %in% c('d242.h10', 'd242.h11')){outdir <- 'data\\CGR1-6'}
  if(dh %in% c('d225.h04', 'd225.h05')){outdir <- 'data\\W44'}
  
  foldername <- 'calanus_rois'
  full_dir <- file.path('c:\\users\\chisholme\\documents\\github\\vpr_meas\\', outdir, foldername, fsep = '\\')
  dir.create(full_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (ii in 1:length(aidlist$V1)){
    cmd1 <- paste("copy", aidlist$V1[ii], full_dir, sep = " ")
    shell(cmd1)
  }
  }

# load in imageJ results----

#cgr1-6 results
cgr16_dat <- read.csv('data/CGR1-6/d0.h0meas_results.csv')
w44_dat <- read.csv('data/w44/d0.h0meas_results.csv')

# isolate roi number
cgr16_dat$Label <- as.character(readr::parse_number(x = cgr16_dat$Label, trim_ws = TRUE, locale = locale(grouping_mark = '.')))
w44_dat$Label <- as.character(readr::parse_number(x = w44_dat$Label, trim_ws = TRUE, locale = locale(grouping_mark = '.')))

cgr16_dat <- cgr16_dat %>%
  group_by(Label) %>%
  filter(Area == max(Area)) 

w44_dat <- w44_dat %>%
  group_by(Label) %>%
  filter(Area == max(Area)) 

# plot ----
# CGR1-6 (Calanus finmarchicus)
ggplot(cgr16_dat)+
  geom_histogram(aes(x = Major), binwidth = 1)+
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

# Histogram overlaid with kernel density curve
ggplot(cgr16_dat, aes(x=Major)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

ggplot(cgr16_dat, aes(x=Area)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=50,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot


#W44 (Calanus hyperboreus)
ggplot(w44_dat)+
  geom_histogram(aes(x = Major), binwidth = 1)+
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

# Histogram overlaid with kernel density curve
ggplot(w44_dat, aes(x=Major)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
ggplot(w44_dat, aes(x=Area)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=50,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

# both stations
cgr16_dat <- cgr16_dat %>%
  dplyr::mutate(., station = 'CGR1-6')
w44_dat <- w44_dat %>%
  dplyr::mutate(., station = 'W44')


all_dat <- full_join(cgr16_dat, w44_dat )

ggplot(all_dat)+
  geom_histogram(aes(x = Major, fill = station), colour = 'black', binwidth = 1)+
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

ggplot(all_dat)+
  geom_histogram(aes(x = Area, fill = station), colour = 'black', binwidth = 50, alpha = 0.5)+
  theme(panel.background = element_blank(),
        panel.grid = element_blank())


# http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
# Overlaid histograms
ggplot(all_dat, aes(x=Major, fill=station)) +
  geom_histogram(binwidth=5, alpha=.5, position="identity")
ggplot(all_dat, aes(x=Area, fill=station)) +
  geom_histogram(binwidth=50, alpha=.5, position="identity")

# Density plots with semi-transparent fill


ggplot(all_dat, aes(x=Major, fill=station)) + geom_density(alpha=.3)
ggplot(all_dat, aes(x=Area, fill=station)) + geom_density(alpha=.3)
ggplot(all_dat, aes(x=Feret, fill=station)) + geom_density(alpha=.3)


# Find the mean of each group
cdat <- ddply(all_dat, "station", summarise, major.mean=mean(Major))

# Overlaid histograms with means
ggplot(all_dat, aes(x=Major, fill=station)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity") +
  geom_vline(data=cdat, aes(xintercept=major.mean,  colour=station),
             linetype="dashed", size=1)
cdat <- ddply(all_dat, "station", summarise, area.mean=mean(Area))

ggplot(all_dat, aes(x=Area, fill=station)) +
  geom_histogram(binwidth=50, alpha=.5, position="identity") +
  geom_vline(data=cdat, aes(xintercept=area.mean,  colour=station),
             linetype="dashed", size=1)
# Density plots with means
ggplot(all_dat, aes(x=Major, colour=station)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=major.mean,  colour=station),
             linetype="dashed", size=1)

# statistical tests ----
# find multiple modes in w44 data
# install.packages("multimode")
library(multimode)

w44_modes <- locmodes(w44_dat$Major, mod0 = 2 ,display = TRUE)
all_modes <- locmodes(all_dat$Major, mod0= 2, display = TRUE)
# not really able to find second mode (artificial mode in tail, try adjusting upper limit?)

# test for statistically significant difference in distribution between stations
# 

# add station W54 ----
# all aid files
aid_files <- list.files('c:/NAS/data/COR2019002/autoid_COR2019002/Calanus/', recursive = TRUE, pattern = 'new_aid', full.names = TRUE)
# select by station (d/h)
valid_dh <- c('d225.h22', 'd225.h23')
valid_aids <- list()
for(dh in valid_dh){
  valid_aids[[dh]] <- grep(aid_files, pattern = dh, value = TRUE)
}

# copy images into folders

for(i in 1:length(valid_aids)){
  dh <- names(valid_aids[i])
  aid_fn <- valid_aids[i][[1]][1]
  
  aidlist <- read.table(aid_fn)
  
  outdir <- 'data\\W54'

  foldername <- 'calanus_rois'
  full_dir <- file.path('c:\\users\\chisholme\\documents\\github\\vpr_meas\\', outdir, foldername, fsep = '\\')
  dir.create(full_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (ii in 1:length(aidlist$V1)){
    cmd1 <- paste("copy", aidlist$V1[ii], full_dir, sep = " ")
    shell(cmd1)
  }
}

# read in W54 ImageJ data ----

w54_dat <- read.csv('data/w54/d0.h0meas_results.csv')

# isolate roi number
w54_dat$Label <- as.character(readr::parse_number(x = w54_dat$Label, trim_ws = TRUE, locale = locale(grouping_mark = '.')))

w54_dat <- w54_dat %>%
  group_by(Label) %>%
  filter(Area == max(Area)) 

# plot w54 -----
# Histogram overlaid with kernel density curve
ggplot(w54_dat, aes(x=Major)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

ggplot(w54_dat, aes(x=Area)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=50,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

w54_dat <- w54_dat %>%
  dplyr::mutate(., station = 'W54')

# plot all 3 stations ----
all_dat <- full_join(all_dat, w54_dat)

# Overlaid histograms
ggplot(all_dat, aes(x=Major, fill=station)) +
  geom_histogram(binwidth=5, alpha=.5, position="identity") +
  scale_x_continuous(limits = c(0, 150))
ggplot(all_dat, aes(x=Area, fill=station)) +
  geom_histogram(binwidth=50, alpha=.5, position="identity")


ggplot(all_dat, aes(x=Major, fill=station)) + geom_density(alpha=.3)+
  scale_x_continuous(limits = c(0, 150))
ggplot(all_dat, aes(x=Area, fill=station)) + geom_density(alpha=.3)


# try to find threshold for chyp
# if everything above 100 (Major) is chyp, what percentage of calanus does that represent? does it match with the net percentages?

w54_dat_100 <- w54_dat %>%
  filter(., Major >= 100)
(length(w54_dat_100$Label)/ length(w54_dat$Label)) *100
# 16 percent of data is >100 Major axis
# w54 net data shows ~ 30% Chyp

w54_dat_90 <- w54_dat %>%
  filter(., Major >= 90)
(length(w54_dat_90$Label)/ length(w54_dat$Label)) *100                                                                                                                                                                              
# ~ 28% calanus >90px Major axis
# better match to net percentage (30%)

cgr16_dat_90 <- cgr16_dat %>%
  filter(., Major >= 90)
(length(cgr16_dat_90$Label)/ length(cgr16_dat$Label)) *100
# 10% of cfin cgr16 data is >90 px Major

w44_dat_90 <- w44_dat %>%
  filter(., Major >= 90)
(length(w44_dat_90$Label)/ length(w44_dat$Label)) *100   


# 90 as threshold is best match for chyp proportions but is likely still an overestimate, catchiong some larger cfin
# applying this threshold to w44 station, still only get ~30%, where w44 should have ~50% chyp
# I don't think this is actually working how I want it to :(

# convert to mm ----
#px to mm conversion factor
frame_mm <- 42
mm_px <- frame_mm/1024 #1024 is resolution of VPR images (p.4 DAVPR manual)
mm2_px2 <- (mm_px)^2

all_dat_mm <- all_dat %>%
  mutate(., area_mm = Area*mm2_px2)%>%
  mutate(., perim_mm = Perim.*mm_px)%>%
  mutate(., major_mm = Major*mm_px)%>%
  mutate(., feret_mm = Feret*mm_px)%>%
  select(Label, area_mm, perim_mm, major_mm, feret_mm, station)

ggplot(all_dat_mm)+
  geom_histogram(aes(x = major_mm, fill = station), alpha = 0.5)+
  geom_vline(xintercept = 4.0)+
  theme(panel.background = element_blank())

ggplot(all_dat_mm, aes(x=major_mm, fill=station)) + geom_density(alpha=.3)+
  scale_x_continuous(limits = c(0, 7.5))+
  geom_vline(xintercept = 4)

# try to find threshold for chyp
# if everything above 4.0 (Major) is chyp, what percentage of calanus does that represent? does it match with the net percentages?

all_chyp <- all_dat_mm %>%
  filter(major_mm >4.0) %>%
  group_by(station)

chyp_cgr16 <- all_chyp %>%
  filter(station == 'CGR1-6')
(length(chyp_cgr16$Label)/length(all_dat_mm$Label)) *100
# <1% chyp at CGR1-6
chyp_w44 <- all_chyp %>%
  filter(station == 'W44')
(length(chyp_w44$Label)/length(all_dat_mm$Label)) *100
# 8.2% chyp at w44
chyp_w54 <- all_chyp %>%
  filter(station == 'W54')
(length(chyp_w54$Label)/length(all_dat_mm$Label)) *100
# 9.8% chyp at w44
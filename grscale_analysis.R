# E OGrady 2022
# This script is an analysis of the growth scale parameter used in autodeck.
# Using imageJ to measure images allow a functionality where objects touching
# the edge are excluded from measurements. The idea was to confirm the best
# growth scale setting in AutoDeck to maximize valid image measurements in
# ImageJ without significanlty increasing the level of background material
# detected.

librarian::shelf(vprr, tidyverse, oce, ggplot2, multimode, plyr)

# read in various image sets ----
# test on various growth scales
# day1, hour 25 (adeck_25 set - grscale= 100), exclude on edges
# day2, hour 25 (adeck_25 set - grscale= 100), NO exclude on edges

# day1, hour 26 (adeck_26 set - grscale= 200), exclude on edges
# day2, hour 26 (adeck_26 set - grscale= 200), NO exclude on edges

# day1, hour 27 (adeck_27 set - grscale= 300), exclude on edges
# day2, hour 27 (adeck_27 set - grscale= 300), NO exclude on edges

# day1, hour 28 (adeck_28set - grscale= 400), exclude on edges
# day2, hour 28 (adeck_28 set - grscale= 400), NO exclude on edges

# run through imageJ process with and without 'exclude on edges' parmaeter ----
readAndFix <- function(file){
  dat <- read.csv(file)
  dat$Label <- as.character(readr::parse_number(x = dat$Label, trim_ws = TRUE, locale = locale(grouping_mark = '.')))
  return(dat)
}

adeck_folders <- list.files(path = 'data', pattern = 'adeck', include.dirs = TRUE, full.names = TRUE)
res_fn <- list.files(adeck_folders, pattern = '.csv', full.names = TRUE)

dat <- lapply(res_fn, readAndFix)
dsnames <- list.files(adeck_folders, pattern = '.csv')
dsnames <- substr(dsnames, start = 1, stop = 6)
names(dat) <- dsnames

# number of images in each set that actually get measured ----
ltab <- lapply(dat, dim)
ltab <- lapply(ltab, `[[`, 1)

day <- substr(names(ltab), start = 2, stop = 2)
hr <- substr(names(ltab), start = 5, stop = 6)
grscale <- c(100, 200, 300, 400, 100, 200, 300, 400)
ee <- c(T, T, T, T, F, F, F, F)
num_obj <- unlist(ltab)
# get unique objects measured per image
dd <- lapply(dat, count, 'Label')
ddavg <- unlist(lapply(dd, function(x) {
  mean(x$freq, na.rm = TRUE)
}))
num_img <- lapply(dd, dim)
num_img <- unlist(lapply(num_img, `[[`, 1))


dat_tab <- data.frame(day, hr, grscale, ee, num_obj, ddavg, num_img)

dat_m <- dat_m %>%
  mutate(., day = substr(id, 2, 2)) %>%
  mutate(., hr = substr(id, 5, 6))

 dat2 <- dat_m %>%
   dplyr::full_join(dat_tab) %>%
   mutate(grscale = as.character(grscale))

# PLOT ----

ggplot(dat_tab)+
  geom_point(aes(x = grscale, y = num_obj, shape = ee, size = ddavg, col = num_img))+
  labs(shape = 'Exclude on Edges', y = 'Number of objs measured', size = 'Avg number of unique objects per image', col = 'Number of images measured')

ggplot(dat_m)+
  geom_point(aes(x = Label, y = Major, col = id))
# there is some expected variation between sample runs

ggplot(dat2)+
  geom_density(aes(x = Major, fill = ee), alpha = 0.3)
# higher probability of small objs, if exclude is false

ggplot(dat2)+
  geom_point(aes(x = Label, y = Major, col = ddavg, shape = grscale))
# variation between runs seems to be pretty random and not associated with any sp. factors

ggplot(dat2)+
  geom_point(aes(x = grscale, y = num_img, shape = ee), size = 5)
# lose some images with exclude on edges when grscale < 200

ggplot(dat2)+
  geom_point(aes(x = grscale, y = ddavg, shape = ee), size = 5)
# exclude on edges reduces multiple objects per image

# recc = grscale 200

all_dat <- dat2 %>%
  group_by(Label) %>%
  filter(Area == max(Area)) 

all_dat <- all_dat %>%
  mutate(., dvar = range(Major)) # quantify variance in meas between sample runs?

ggplot(all_dat[1:10,])+
  geom_point(aes(x = Label, y = Major, col = id), alpha = 0.1, size = 5)

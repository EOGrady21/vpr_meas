# cfin analysis
librarian::shelf(vprr, tidyverse, oce, ggplot2, multimode, plyr)

# attempt to distinguish consistent cfin distribution so that removasl of this distribution can leave chyp data behind

# copy images ----

# all aid files
aid_files <- list.files('c:/NAS/data/COR2019002/autoid_COR2019002/Calanus/', recursive = TRUE, pattern = 'new_aid', full.names = TRUE)
# select by station (d/h)
stations <- c('CGR 1-4', '7.6_1', '7.6_2', 'P2-5', 'W39')
# read in station info file
stinfo <- read.csv('c:/VPR_PROJECT/COR2019002/SCRIPTS/station_names_COR2019002.csv')
valid_dh <- stinfo$day_hour[stinfo$station %in% stations]

valid_aids <- list()
for(dh in valid_dh){
  valid_aids[[dh]] <- grep(aid_files, pattern = dh, value = TRUE)
}

# copy images into folders
for(i in 1:length(valid_aids)){
  dh <- names(valid_aids[i])
  aid_fn <- valid_aids[i][[1]][1]
  
  aidlist <- read.table(aid_fn)
  st <- stinfo$station[stinfo$day_hour == dh]
  st <- gsub(pattern = ' ', replacement = '', x = st)
  outdir <- file.path('data', st , fsep = '\\')
  foldername <- 'calanus_rois'
  full_dir <- file.path('c:\\users\\chisholme\\documents\\github\\vpr_meas\\', outdir, foldername, fsep = '\\')
  dir.create(full_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (ii in 1:length(aidlist$V1)){
    cmd1 <- paste("copy", aidlist$V1[ii], full_dir, sep = " ")
    shell(cmd1)
    cat( ii, '/', length(aidlist$V1), '\n')
  }
  cat('---- station: ', st, 'complete! ---- \n')
}


# read in ImageJ data ----

# gather all csv files
res_fn <- list.files(path = 'data/', pattern = '.csv', recursive = TRUE, full.names = TRUE)

readAndFix <- function(file){
  dat <- read.csv(file)
  dat$Label <- as.character(readr::parse_number(x = dat$Label, trim_ws = TRUE, locale = locale(grouping_mark = '.')))
  return(dat)
}

dat_7.6_1 <- readAndFix(res_fn[1])
dat_7.6_2 <- readAndFix(res_fn[2])
dat_cgr14 <- readAndFix(res_fn[3])
dat_p25 <- readAndFix(res_fn[5])
dat_w39 <- readAndFix(res_fn[6])


dat_7.6_1 <- dat_7.6_1 %>%
  dplyr::mutate(., station = '7.6_1')
dat_7.6_2 <- dat_7.6_2 %>%
  dplyr::mutate(., station = '7.6_2')
dat_cgr14 <- dat_cgr14 %>%
  dplyr::mutate(., station = 'CGR 1-4')
dat_p25 <- dat_p25 %>%
  dplyr::mutate(., station = 'P2-5')
dat_w39 <- dat_w39 %>%
  dplyr::mutate(., station = 'W39')

all_dat <- merge(merge(merge(merge(merge(
  dat_7.6_1,
  dat_7.6_2, all = TRUE),
  dat_cgr14, all = TRUE),
  dat_p25, all = TRUE),
  dat_w39, all = TRUE),
  cgr16_dat, all = TRUE)

all_dat <- all_dat %>%
  group_by(Label) %>%
  filter(Area == max(Area)) 


# plot ----
ggplot(all_dat, aes(x=Major, fill=station)) + geom_density(alpha=.3)
ggplot(all_dat, aes(x=Area, fill=station)) + geom_density(alpha=.3)
ggplot(all_dat, aes(x=Feret, fill=station)) + geom_density(alpha=.3)

# get an average distribution of cfin
# plot average over all stations(dashed line)
ggplot(all_dat)+
  geom_density(aes(x= Major), alpha = 0.2)

ggplot(all_dat_cf)+ 
  geom_density(aes(x = Major, fill = station), alpha = 0.2)+
  geom_density(aes(x = Major), linetype = 'dashed', size = 2)+
  geom_density(data = all_dat, aes(x = Major), alpha = 0.5, fill = 'black')


all_dat_cf <- all_dat
all_dat <- full_join(w54_dat, w44_dat)

ggplot()+
  geom_density(data = all_dat_cf, aes(x = Major), linetype = 'dashed', size = 2)+
  geom_density(data = all_dat, aes(x = Major, fill = station), alpha = 0.3)

ggplot()+
  geom_density(data = all_dat_cf, aes(x = Area, fill = station), linetype = 'dashed', size = 2, alpha = 0.1)+
  geom_density(data = all_dat, aes(x = Area),fill = 'black', alpha = 0.3, size = 5)

ggplot()+
  geom_density(data = all_dat_cf, aes(x = Feret), linetype = 'dashed', size = 2, alpha = 0.1, fill= 'green')+
  geom_density(data = all_dat, aes(x = Feret),fill = 'black', alpha = 0.3, size = 5)


ggplot()+
  geom_histogram(data = all_dat_cf, aes(x = Major, fill = station), binwidth = 10)+
  geom_histogram(data = all_dat, aes(x = Major), fill = 'black', alpha = 0.7)


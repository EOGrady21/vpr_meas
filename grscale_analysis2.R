# grscale analysis 2

# adeck 34 (gr scale 150)

# adeck 25 (gr scale 100)

# adeck26 (grscale 200)


# read in meas data ----

readAndFix <- function(file){
  dat <- read.csv(file)
  dat$Label <- as.character(readr::parse_number(x = dat$Label, trim_ws = TRUE, locale = locale(grouping_mark = '.')))
  return(dat)
}

adeck_folders <- list.files(path = 'data', pattern = 'adeck', include.dirs = TRUE, full.names = TRUE)
adeck_folders <- adeck_folders[c(1, 2, 5)]
res_fn <- list.files(adeck_folders, pattern = '.csv', full.names = TRUE)
res_fn <- grep(res_fn, pattern = 'd3', value = TRUE)

dat <- lapply(res_fn, readAndFix)
dsnames <- res_fn
dsnames <- substr(dsnames, start = 15, stop = 20)
names(dat) <- dsnames

# merge data
dat_m <- bind_rows(dat, .id = 'id')

ltab <- lapply(dat, dim)
ltab <- lapply(ltab, `[[`, 1)

day <- substr(names(ltab), start = 2, stop = 2)
hr <- substr(names(ltab), start = 5, stop = 6)
grscale <- c(100, 200, 150)
num_obj <- unlist(ltab)
# get unique objects measured per image
dd <- lapply(dat, count, 'Label')
ddavg <- unlist(lapply(dd, function(x) {
  mean(x$freq, na.rm = TRUE)
}))
num_img <- lapply(dd, dim)
num_img <- unlist(lapply(num_img, `[[`, 1))


dat_tab <- data.frame(day, hr, grscale, num_obj, ddavg, num_img)

# plot
ggplot(dat_m)+
  geom_density(aes(x = Major, fill = id))

ggplot(dat_tab)+
  geom_point(aes(x = grscale, y = num_img, col = num_obj-num_img), size = 5)+
  geom_hline(yintercept = 714)

# functions for measuring vpr images, reading and analyzing imagej data
# e ogrady august 2022


#' Read and tidy ImageJ results data
#' Designed to read and clean up the results file output from vpr_batch.ijm. Reads in csv then cleans up ROI labels.
#' 
#' @param file a csv file of ImageJ results
#'
#' @return a dataframe of ImageJ results
#' @export
#'
#' @examples
#' adeck_folders <- list.files(path = 'data', pattern = 'adeck', include.dirs = TRUE, full.names = TRUE)
#' 
#' res_fn <- list.files(adeck_folders, pattern = '.csv', full.names = TRUE)
#' 
#' dat <- lapply(res_fn, readAndFix)
#' 

readAndFix <- function(file){
  dat <- read.csv(file)
  dat$Label <- as.character(readr::parse_number(x = dat$Label, trim_ws = TRUE, locale = locale(grouping_mark = '.')))
  return(dat)
}

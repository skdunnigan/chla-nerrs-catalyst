# ---- Read in ISCO data from files
library(readxl)
library(purrr)
library(dplyr)
library(janitor)

# read in all data files as a list

dat.files  <- list.files(path = here::here('analysis'),
                         pattern = "chla_template*",
                         full.names = TRUE)


in_path <- here::here("metadata", "user_defined_inputs.xlsx")

# read in qaqc options
opts_qaqc <- read_excel(in_path, sheet = "qaqc_codes")


# pull out and bind `isco` data




# pull out and bind `tank` data
















readisco <- function(f) {
  dat.is <- readxl::read_xlsx(f, sheet = "isco")
}

readtank <- function(f) {
  dat.ta <- readxl::read_xlsx(f, sheet = "tank") 
}

isco.data.files <- sapply(dat.files, readxl::read_xlsx, simplify = FALSE)
isco.data.files2 <- purrr::map(dat.files, readisco)

tank.data.files <- sapply(dat.files, readtank, simplify = FALSE)

# this should work when there are actually data populating the template
isco.dat <- isco.data.files %>% 
  dplyr::bind_rows(.id = "id") %>%
  janitor::clean_names()

tank.dat <- tank.data.files %>% 
  dplyr::bind_rows(.id = "id") %>%
  janitor::clean_names()




read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

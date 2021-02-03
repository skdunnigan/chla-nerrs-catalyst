# ---- Read in ISCO data from files
library(readxl)
library(purrr)
library(dplyr)
library(janitor)

# read in all data files as a list

dat.files  <- list.files(path = here::here('data'),
                         recursive = TRUE,
                         pattern = "chla_template*",
                         full.names = TRUE)

readisco <- function(f) {
  dat.is <- readxl::read_xlsx(f, sheet = "isco") 
}

readtank <- function(f) {
  dat.ta <- readxl::read_xlsx(f, sheet = "tank") 
}

isco.data.files <- sapply(dat.files, readisco, simplify = FALSE)
# isco.data.files2 <- purrr::map(dat.files, readisco)

tank.data.files <- sapply(dat.files, readtank, simplify = FALSE)

# this should work when there are actually data populating the template
isco.dat <- isco.data.files %>% 
  dplyr::bind_rows(.id = "id") %>%
  janitor::clean_names()

tank.dat <- tank.data.files %>% 
  dplyr::bind_rows(.id = "id") %>%
  janitor::clean_names()

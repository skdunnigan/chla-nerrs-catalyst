library(readxl)


dat.files  <- list.files(path=here::here('data'),
                         recursive=T,
                         pattern="chla_template*"
                         ,full.names=T)

readDatFile <- function(f) {
  dat.fl <- readxl::read_xlsx(f, sheet = "isco") 
}

data.files <- sapply(dat.files, readDatFile, simplify = FALSE) 

dat <- data.files[[2]]


%>%
  dplyr::bind_rows(.id = "id") %>%
  janitor::clean_names()
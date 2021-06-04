# 00 load packages ----
source('R/00_load-packages.R')

# 01 all data files ready for analysis ----
files <- list.files(path = here::here('analysis'), 
                    pattern = ".xlsx")

# 02 read in isco files ----

isco_files <- list()

for (i in 1:length(files)){
  
  tempdf <- readxl::read_excel(path = here::here('analysis', files[i]), 
                               sheet = 'isco') %>% 
            janitor::clean_names() %>% 
            dplyr::mutate(reserve_code = toupper(reserve_code),
                          sample_code = paste0(isco_deployment_no, '_', sample_no, '_', reserve_code), 
                          method = "isco") %>% 
            dplyr::rename(fdom_qsu = f_domqsu,
                          fdom_rfu = f_domrfu)
  
  isco_files[[i]] <- tempdf # add file to opened list()
  
  # names[[i]] <- paste0(unique(tempdf$reserve_code))
  
  name <- unique(tempdf$reserve_code) # pull out the name you want of the file
  
  assign(paste0(name, "_", 'isco'), tempdf) 
  
  rm(tempdf)
}

names(isco_files) <- paste0(substr(c(files),10,12), "_isco") # rename all the list files by reserve code and '_isco'

isco_files # view the complete list of files read in for isco

rm('_isco')

# 03 read in tank files ----

tank_files <- list()

for (i in 1:length(files)){
  
  tempdf <- readxl::read_excel(path = here::here('analysis', files[i]), 
                               sheet = 'tank') %>% 
    janitor::clean_names() 
  
  tank_files[[i]] <- tempdf # add file to opened list()
  
  # names[[i]] <- paste0(unique(tempdf$reserve_code))
  
  name <- unique(tempdf$reserve_code) # pull out the name you want of the file
  
  assign(paste0(name, "_", 'tank'), tempdf) 
  
  rm(tempdf)
}

names(tank_files) <- paste0(substr(c(files),10,12), "_tank") # rename all the list files by reserve code and '_isco'

tank_files # view the complete list of files read in for isco

rm('_tank')

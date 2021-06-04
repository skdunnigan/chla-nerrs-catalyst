
# load packages -----------------------------------------------------------

library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)
library(ggpubr)
library(readxl)
library(here)
library(cowplot)
library(plotly)


# load-isco-data ---------------------------------------------------------------

# isco files
# elk, gtm, pdb, wel

elk_isco <- readxl::read_xlsx(here::here('analysis', 'chla_2.0_elk.xlsx'),
                              sheet = 'isco') %>% 
  janitor::clean_names() %>% 
  dplyr::filter(qaqc < 1) %>% 
  dplyr::glimpse() 

gtm_isco <- readxl::read_xlsx(here::here('analysis', 'chla_2.0_gtm.xlsx'),
                              sheet = 'isco') %>% 
  janitor::clean_names() %>% 
  dplyr::filter(qaqc < 1) %>% 
  dplyr::glimpse()

pdb_isco <- readxl::read_xlsx(here::here('analysis', 'chla_2.0_pdb.xlsx'),
                              sheet = 'isco') %>% 
  janitor::clean_names() %>% dplyr::glimpse()

wel_isco <- readxl::read_xlsx(here::here('analysis', 'chla_2.0_wel.xlsx'),
                              sheet = 'isco') %>% 
  janitor::clean_names() %>% dplyr::glimpse()

owc_isco <- readxl::read_xlsx(here::here('analysis', 'chla_2.0_owc.xlsx'),
                              sheet = 'isco') %>% 
  janitor::clean_names() %>% dplyr::glimpse() 

# bind all into one
isco <- bind_rows(elk_isco, gtm_isco, pdb_isco, wel_isco, owc_isco) %>% 
  dplyr::mutate(reserve_code = toupper(reserve_code),
                sample_code = paste0(isco_deployment_no, '_', sample_no, '_', reserve_code), 
                method = "isco") %>% 
  rename(fdom_qsu = f_domqsu,
         fdom_rfu = f_domrfu)

glimpse(isco)

rm(elk_isco, gtm_isco, pdb_isco, wel_isco, owc_isco)


# load-tank-data ----------------------------------------------------------

# tank files
# gtm, hee, mar, pdb, sap

gtm_tank <- readxl::read_xlsx(here::here('analysis', 'chla_2.0_gtm.xlsx'),
                              sheet = 'tank') %>% 
  janitor::clean_names() %>% 
  mutate(remarks = as.character(remarks), 
         level = as.numeric(level),
         f_depth = as.character(f_depth),
         f_level = as.character(f_level),
         f_do_pct = as.character(f_do_pct),
         f_do_mgl = as.character(f_do_mgl),
         f_sp_cond = as.character(f_sp_cond),
         f_turb = as.character(f_turb),
         f_sal = as.character(f_sal),
         f_p_h = as.character(f_p_h),
         f_temp = as.character(f_temp),
         f_chlorophyll_rfu = as.character(f_chlorophyll_rfu),
         f_chl_fluor = as.character(f_chl_fluor)) %>% 
  dplyr::glimpse()

hee_tank <- readxl::read_xlsx(here::here('analysis', 'chla_2.0_hee.xlsx'),
                              sheet = 'tank') %>% 
  janitor::clean_names() %>% 
  mutate(remarks = as.character(remarks), 
         level = as.numeric(level),
         f_depth = as.character(f_depth),
         f_level = as.character(f_level),
         f_do_pct = as.character(f_do_pct),
         f_do_mgl = as.character(f_do_mgl),
         f_sp_cond = as.character(f_sp_cond),
         f_turb = as.character(f_turb),
         f_sal = as.character(f_sal),
         f_p_h = as.character(f_p_h),
         f_temp = as.character(f_temp),
         f_chlorophyll_rfu = as.character(f_chlorophyll_rfu),
         f_chl_fluor = as.character(f_chl_fluor)) %>% 
  dplyr::glimpse()

mar_tank <- readxl::read_xlsx(here::here('analysis', 'chla_2.0_mar.xlsx'),
                              sheet = 'tank') %>% 
  janitor::clean_names() %>% 
  mutate(remarks = as.character(remarks), 
         level = as.numeric(level),
         f_depth = as.character(f_depth),
         f_level = as.character(f_level),
         f_do_pct = as.character(f_do_pct),
         f_do_mgl = as.character(f_do_mgl),
         f_sp_cond = as.character(f_sp_cond),
         f_turb = as.character(f_turb),
         f_sal = as.character(f_sal),
         f_p_h = as.character(f_p_h),
         f_temp = as.character(f_temp),
         f_chlorophyll_rfu = as.character(f_chlorophyll_rfu),
         f_chl_fluor = as.character(f_chl_fluor)) %>%  
  dplyr::glimpse()

pdb_tank <- readxl::read_xlsx(here::here('analysis', 'chla_2.0_pdb.xlsx'),
                              sheet = 'tank') %>% 
  janitor::clean_names() %>% 
  mutate(remarks = as.character(remarks), 
         level = as.numeric(level),
         f_depth = as.character(f_depth),
         f_level = as.character(f_level),
         f_do_pct = as.character(f_do_pct),
         f_do_mgl = as.character(f_do_mgl),
         f_sp_cond = as.character(f_sp_cond),
         f_turb = as.character(f_turb),
         f_sal = as.character(f_sal),
         f_p_h = as.character(f_p_h),
         f_temp = as.character(f_temp),
         f_chlorophyll_rfu = as.character(f_chlorophyll_rfu),
         f_chl_fluor = as.character(f_chl_fluor)) %>%  
  dplyr::glimpse()

sap_tank <- readxl::read_xlsx(here::here('analysis', 'chla_2.0_sap.xlsx'),
                              sheet = 'tank') %>% 
  janitor::clean_names() %>% 
  mutate(remarks = as.character(remarks), 
         level = as.numeric(level),
         depth = as.numeric(depth)
         ) %>% 
  dplyr::glimpse()

# bind all into one
tank <- bind_rows(gtm_tank, hee_tank, mar_tank, pdb_tank, sap_tank) %>% 
  dplyr::mutate(reserve_code = toupper(reserve_code),
                sample_code = paste0(sample_no, '_', reserve_code), 
                method = "tank") %>% 
  rename(fdom_qsu = f_domqsu,
         fdom_rfu = f_domrfu,
         chla_ugl = chla_ug_l)

glimpse(tank)

rm(gtm_tank, hee_tank, mar_tank, pdb_tank, sap_tank)


# qaqc --------------------------------------------------------------------





# combine into one --------------------------------------------------------

a <- isco %>% 
  select(-contains("f_"))

b <- tank %>% 
  select(-contains("f_"))

all <- bind_rows(a,b)

rm(a, b)

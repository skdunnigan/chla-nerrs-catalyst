# 00 load packages ----
# source('R/00_load-packages.R')

# 01 read in data ----
source('R/02_load_data.R')

# 02 wrangle for site-specific differences in data ----

## 02.1 Mission Aransas (MAR)----
# takes subsamples of replicates. Need to create two columns for replicates to separate out the subsamples

MAR_tank <- MAR_tank %>% 
              dplyr::filter(rep == 1.1) %>% 
              tidyr::separate(rep,
                            into = c("rep", "subsample"),
                            sep = "[.]") %>% 
              dplyr::select(-subsample, -additional_notes)

## 02.2 Old Woman Creek (OWC)----
# takes replicate of last isco sample. 

OWC_isco <- OWC_isco %>% 
              tidyr::separate(sample_no,
                              into = c("sample_no", "rep_isco"),
                              sep = "[.]") %>% 
              dplyr::filter(rep_isco == 1 | is.na(rep_isco)) %>% # keep only the NA and 1 values
              dplyr::select(-rep_isco)


# 03 combine files --------------------------------------------------------

# first, compare the files, make adjustments, then combine

## 03.1 isco files ----

# compare all isco dfs
# only can compare two at a time, so just interchange which two dfs to use
janitor::compare_df_cols_same(PDB_isco, WEL_isco, 
                              bind_method = "bind_rows") 
                              
ELK_isco <- ELK_isco %>% 
            dplyr::mutate(depth = as.numeric(depth),
                          f_depth = as.character(f_depth),
                          f_level = as.character(f_level),
                          fdom_rfu = as.numeric(fdom_rfu),
                          f_chlorophyll_rfu = as.character(f_chlorophyll_rfu)
                          )
GTM_isco <- GTM_isco %>% 
            dplyr::mutate(f_level = as.character(f_level),
                          level = as.numeric(level),
                          f_depth = as.character(f_depth),
                          f_chlorophyll_rfu = as.character(f_chlorophyll_rfu)
                          )
PDB_isco <- PDB_isco %>% 
            dplyr::mutate(f_level = as.character(f_level),
                          level = as.numeric(level),
                          f_depth = as.character(f_depth),
                          f_chlorophyll_rfu = as.character(f_chlorophyll_rfu)
                          )
OWC_isco <- OWC_isco %>% 
            dplyr::mutate(f_level = as.character(f_level),
                          level = as.numeric(level),
                          f_depth = as.character(f_depth),
                          f_chlorophyll_rfu = as.character(f_chlorophyll_rfu),
                          sample_no = as.numeric(sample_no)
                          )
WEL_isco <- WEL_isco %>% 
            dplyr::mutate(f_level = as.character(f_level),
                          remarks = as.character(remarks)
                          )

# bind all into one
isco <- dplyr::bind_rows(ELK_isco, 
                         GTM_isco, 
                         PDB_isco, 
                         WEL_isco, 
                         OWC_isco) %>% 
        dplyr::mutate(f_do_pct = as.character(f_do_pct),
                      f_chl_fluor = as.character(f_chl_fluor),
                      f_do_mgl = as.character(f_do_mgl),
                      f_p_h = as.character(f_p_h),
                      f_sal = as.character(f_sal),
                      f_sp_cond = as.character(f_sp_cond),
                      f_temp = as.character(f_temp),
                      f_turb = as.character(f_turb)
                      ) 

rm(ELK_isco, GTM_isco, PDB_isco, WEL_isco, OWC_isco, HEE_isco)

## 03.2 tank files ----

# compare all tank dfs
# only can compare two at a time, so just interchange which two dfs to use
janitor::compare_df_cols_same(MAR_tank, HEE_tank, 
                              bind_method = "bind_rows") 


GTM_tank <- GTM_tank %>% 
            dplyr::mutate(remarks = as.character(remarks),
                          f_chl_fluor = as.character(f_chl_fluor),
                          f_chlorophyll_rfu = as.character(f_chlorophyll_rfu),
                          f_depth = as.character(f_depth),
                          f_do_mgl = as.character(f_do_mgl),
                          f_do_pct = as.character(f_do_pct),
                          f_p_h = as.character(f_p_h),
                          f_sal = as.character(f_sal),
                          f_sp_cond = as.character(f_sp_cond),
                          f_temp = as.character(f_temp),
                          f_turb = as.character(f_turb)
                          )
PDB_tank <- PDB_tank %>% 
            dplyr::mutate(f_chl_fluor = as.character(f_chl_fluor),
                          f_chlorophyll_rfu = as.character(f_chlorophyll_rfu),
                          f_depth = as.character(f_depth),
                          f_do_mgl = as.character(f_do_mgl),
                          f_do_pct = as.character(f_do_pct),
                          f_sal = as.character(f_sal),
                          f_sp_cond = as.character(f_sp_cond),
                          f_temp = as.character(f_temp)
                          )
MAR_tank <- MAR_tank %>% 
            dplyr::mutate(rep = as.numeric(rep),
                          f_chl_fluor = as.character(f_chl_fluor),
                          f_chlorophyll_rfu = as.character(f_chlorophyll_rfu),
                          f_depth = as.character(f_depth),
                          f_do_mgl = as.character(f_do_mgl),
                          f_do_pct = as.character(f_do_pct),
                          f_p_h = as.character(f_p_h),
                          f_sal = as.character(f_sal),
                          f_sp_cond = as.character(f_sp_cond),
                          f_temp = as.character(f_temp),
                          f_turb = as.character(f_turb)
                          ) 
SAP_tank <- SAP_tank %>% 
            dplyr::mutate(chl_fluor = as.numeric(chl_fluor),
                          depth = as.numeric(depth),
                          field_notes = as.character(field_notes),
                          remarks = as.character(remarks),
                          station_code = as.character(station_code)
                          )
HEE_tank <- HEE_tank %>% 
            dplyr::mutate(remarks = as.character(remarks),
                          chla_rfu = as.numeric(chla_rfu),
                          f_chl_fluor = as.character(f_chl_fluor),
                          f_chlorophyll_rfu = as.character(f_chlorophyll_rfu),
                          f_depth = as.character(f_depth),
                          f_do_mgl = as.character(f_do_mgl),
                          f_do_pct = as.character(f_do_pct),
                          f_p_h = as.character(f_p_h),
                          f_sal = as.character(f_sal),
                          f_sp_cond = as.character(f_sp_cond),
                          f_temp = as.character(f_temp),
                          f_turb = as.character(f_turb)
                          )

# bind all into one
tank <- dplyr::bind_rows(GTM_tank, 
                         HEE_tank, 
                         MAR_tank, 
                         PDB_tank, 
                         SAP_tank) %>% 
        dplyr::mutate(f_level = as.character(f_level),
                      level = as.numeric(level)) %>% 
        dplyr::rename(chla_ugl = chla_ug_l,
                      fdom_qsu = f_domqsu,
                      fdom_rfu = f_domrfu) 

rm(GTM_tank, HEE_tank, MAR_tank, PDB_tank, SAP_tank)

## 03.3 all files ----

janitor::compare_df_cols_same(isco, tank, 
                              bind_method = "bind_rows")

all <- dplyr::bind_rows(isco, tank) %>% 
       dplyr::filter(rep == 1 | is.na(rep)) # only keep rep 1 from tank, and NAs, which would be the isco data


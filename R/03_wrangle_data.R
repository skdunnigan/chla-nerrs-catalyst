# 00 load packages ----
# source('R/00_load-packages.R')

# 01 read in data ----
# source('R/02_load_data.R')

# 02 wrangle for site-specific differences in data ----

## 02.1 Mission Aransas (MAR)----
# takes subsamples of replicates for 'method' replicates. Need to create two columns for replicates to separate out the subsamples

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

## 02.3 Lake Superior (LKS) ----
# takes replicate samples for tank

LKS_tank <- LKS_tank %>% 
              tidyr::separate(sample_no,
                              into = c("sample_no", "rep_tank"),
                              sep = "[.]") %>%
              dplyr::filter(rep_tank == 0 | is.na(rep_tank)) %>% # keep only the NA and 0 values because there is 28.0 in addition to 28.1
              dplyr::select(-rep_tank)
# 03 combine files --------------------------------------------------------

# first, compare the files, make adjustments, then combine

## 03.1 isco files ----

# compare all isco dfs
# only can compare two at a time, so just interchange which two dfs to use
janitor::compare_df_cols_same(ELK_isco, WEL_isco, 
                              bind_method = "bind_rows") 
                              
ELK_isco <- ELK_isco %>% 
            dplyr::mutate(depth = as.numeric(depth),
                          f_depth = as.character(f_depth),
                          remarks = as.character(remarks)
                          )
GND_isco <- GND_isco %>% 
            dplyr::mutate(chla_rfu = as.numeric(chla_rfu),
                          depth = as.numeric(depth),
                          f_depth = as.character(f_depth),
                          level = as.numeric(level),
                          remarks = as.character(remarks)
                          )
HEE_isco <- HEE_isco %>% 
            dplyr::mutate(chla_rfu = as.numeric(chla_rfu),
                          chlorophyll_rfu = as.numeric(chlorophyll_rfu),
                          fdom_qsu = as.numeric(fdom_qsu),
                          fdom_rfu = as.numeric(fdom_rfu),
                          level = as.numeric(level),
                          remarks = as.character(remarks)
                          )
LKS_isco <- LKS_isco %>% 
            dplyr::mutate(chla_rfu = as.numeric(chla_rfu),
                          f_depth = as.character(f_depth),
                          level = as.numeric(level),
                          f_level = as.character(f_level)
                          )
NIW_isco <- NIW_isco %>% 
            dplyr::mutate(level = as.numeric(level),
                          remarks = as.character(remarks)
            )
OWC_isco <- OWC_isco %>% 
            dplyr::mutate(level = as.numeric(level),
                          sample_no = as.numeric(sample_no)
                          )
PDB_isco <- PDB_isco %>% 
            dplyr::mutate(f_level = as.character(f_level),
                          level = as.numeric(level),
            )
WEL_isco <- WEL_isco %>% 
            dplyr::mutate(remarks = as.character(remarks)
                          )

# bind all into one
isco <- dplyr::bind_rows(ELK_isco,
                         GND_isco,
                         GTM_isco,
                         HEE_isco,
                         LKS_isco,
                         NIW_isco, 
                         OWC_isco,
                         PDB_isco, 
                         WEL_isco) %>% 
        dplyr::rename(f_fdom_qsu = f_f_domqsu,
                      f_fdom_rfu = f_f_domrfu
                      ) 

rm(ELK_isco,
   GND_isco,
   GTM_isco,
   HEE_isco,
   LKS_isco,
   NIW_isco, 
   OWC_isco,
   PDB_isco, 
   WEL_isco)

## 03.2 tank files ----

# compare all tank dfs
# only can compare two at a time, so just interchange which two dfs to use
janitor::compare_df_cols_same(GTM_tank, PDB_tank, 
                              bind_method = "bind_rows") 

GND_tank <- GND_tank %>% 
            dplyr::mutate(chla_rfu = as.numeric(chla_rfu),
                          field_notes = as.character(field_notes)
                          )
GTM_tank <- GTM_tank %>% 
            dplyr::mutate(remarks = as.character(remarks)
                          )
HEE_tank <- HEE_tank %>% 
            dplyr::mutate(remarks = as.character(remarks),
                          chla_rfu = as.numeric(chla_rfu)
                          )
LKS_tank <- LKS_tank %>% 
            dplyr::mutate(chla_rfu = as.numeric(chla_rfu),
                          sample_no = as.numeric(sample_no)
                          )
MAR_tank <- MAR_tank %>% 
            dplyr::mutate(rep = as.numeric(rep)
                          )
NIW_tank <- NIW_tank %>% 
            dplyr::mutate(do_mgl = as.numeric(do_mgl), 
                          do_pct = as.numeric(do_pct),
                          p_h = as.numeric(p_h), 
                          remarks = as.character(remarks)
                          )

SAP_tank <- SAP_tank %>% 
            dplyr::mutate(chl_fluor = as.numeric(chl_fluor),
                          depth = as.numeric(depth),
                          field_notes = as.character(field_notes),
                          remarks = as.character(remarks),
                          station_code = as.character(station_code)
                          )

# bind all into one
tank <- dplyr::bind_rows(GND_tank,
                         GTM_tank, 
                         HEE_tank,
                         LKS_tank,
                         MAR_tank,
                         NIW_tank,
                         PDB_tank, 
                         SAP_tank) %>% 
        dplyr::mutate(level = as.numeric(level)) %>% 
        dplyr::rename(fdom_qsu = f_domqsu,
                      fdom_rfu = f_domrfu,
                      f_fdom_qsu = f_f_domqsu,
                      f_fdom_rfu = f_f_domrfu,
                      chla_ugl = chla_ug_l)
        

rm(GND_tank,
   GTM_tank, 
   HEE_tank,
   LKS_tank,
   MAR_tank,
   NIW_tank,
   PDB_tank, 
   SAP_tank)

## 03.3 all files ----

janitor::compare_df_cols_same(isco, tank, 
                              bind_method = "bind_rows")

all <- dplyr::bind_rows(isco, tank) %>% 
       dplyr::filter(rep == 1 | is.na(rep)) %>%  # only keep rep 1 from tank, and NAs, which would be the isco data
       dplyr::mutate(sample_no = as.character(sample_no),
                     isco_deployment_no = as.character(isco_deployment_no),
                     date_collected = as.Date(datetime_collected))

# calculate all lm equations, all/tank/isco -------------------------------


# # all
# 
# lm_out <- all %>%
#   dplyr::group_by(reserve_code) %>%
#   do(broom::tidy(lm(chla_ugl ~ chlorophyll_rfu, data = .)))
# 
# diag <- dat2 %>%
#   group_by(dilution_percent) %>%
#   do(broom::glance(lm(chla_rfu ~ delta_T, data = .)))
# 
# ##check out model outputs
# lm_out
# diag
# 
# ## pull out slope and intercept
# m <- lm_out %>%
#   dplyr::filter(term == 'delta_T') %>%
#   dplyr::rename(m = estimate) %>%
#   dplyr::select(-term)
# 
# b <- lm_out %>%
#   dplyr::filter(term == '(Intercept)') %>%
#   dplyr::select(dilution_percent, estimate) %>%
#   dplyr::rename(b = estimate)

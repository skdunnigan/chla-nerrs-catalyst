# 00 load packages ----
# source('R/00_load-packages.R')

# 01 read in data ----
# source('R/02_load_data.R')

# 02 wrangle for site-specific differences in data ----

## 02.1 Mission Aransas (MAR)----
# takes subsamples of replicates for 'method' replicates. Need to create two columns for replicates to separate out the subsamples
# removing method replicates, but keeping first method rep of each sample replicate.

MAR_tank <- MAR_tank %>%
              tidyr::separate(rep,
                            into = c("rep", "subsample"),
                            sep = "[.]") %>% 
              dplyr::filter(subsample == 1 | is.na(subsample)) %>% 
              dplyr::select(-subsample) %>% 
              dplyr::mutate(rep = as.numeric(rep))

## 02.2 Old Woman Creek (OWC)----
# takes replicate of last isco sample. 

OWC_isco <- OWC_isco %>% 
              tidyr::separate(sample_no,
                              into = c("sample_no", "rep_isco"),
                              sep = "[.]") %>% 
              dplyr::filter(rep_isco == 1 | is.na(rep_isco)) %>% # keep only the NA and 1 values
              dplyr::select(-rep_isco) %>% 
              dplyr::mutate(sample_no = as.numeric(sample_no))

## 02.3 Lake Superior (LKS) ----
# takes replicate samples for tank

LKS_tank <- LKS_tank %>% 
              tidyr::separate(sample_no,
                              into = c("sample_no", "rep_tank"),
                              sep = "[.]") %>%
              dplyr::filter(rep_tank == 0 | is.na(rep_tank)) %>% # keep only the NA and 0 values because there is 28.0 in addition to 28.1
              dplyr::select(-rep_tank)

## 02.4 Great Bay (GRB)----
# takes subsamples of replicates for 'method' replicates. Need to create two columns for replicates to separate out the subsamples

GRB_tank <- GRB_tank %>%
               tidyr::separate(rep,
                               into = c("rep", "subsample"),
                               sep = "[.]") %>% 
               dplyr::filter(subsample == 1 | is.na(subsample)) %>% # only keep the first subsample
               dplyr::select(-subsample) %>% 
               dplyr::mutate(rep = as.numeric(rep))

# takes replicate of last isco sample. 
GRB_isco <- GRB_isco %>% 
               tidyr::separate(sample_no,
                               into = c("sample_no", "rep_isco"),
                               sep = "[.]") %>% 
               dplyr::filter(rep_isco == 1 | is.na(rep_isco)) %>% # keep only the NA and 1 values
               dplyr::select(-rep_isco) %>% 
               dplyr::mutate(sample_no = as.numeric(sample_no))


# 03 combine files --------------------------------------------------------

# first, compare the files, make adjustments, then combine

## 03.1 isco files ----

ELK_isco <- ELK_isco %>% 
   dplyr::mutate(depth = as.numeric(depth),
                 f_depth = as.character(f_depth),
                 remarks = as.character(remarks)
   )

# compare all isco dfs
# # # only can compare two at a time, so just interchange which two dfs to use
# janitor::compare_df_cols_same(ELK_isco, OWC_isco,
#                               bind_method = "bind_rows")
                              

GND_isco <- GND_isco %>% 
            dplyr::mutate(chla_rfu = as.numeric(chla_rfu),
                          level = as.numeric(level),
                          remarks = as.character(remarks)
                          )

GRB_isco <- GRB_isco %>% 
            dplyr::mutate(fdom_rfu = as.numeric(fdom_rfu)
                          )

HEE_isco <- HEE_isco %>% 
            dplyr::mutate(chla_rfu = as.numeric(chla_rfu),
                          fdom_qsu = as.numeric(fdom_qsu),
                          fdom_rfu = as.numeric(fdom_rfu),
                          level = as.numeric(level),
                          remarks = as.character(remarks)
                          )
LKS_isco <- LKS_isco %>% 
            dplyr::mutate(chla_rfu = as.numeric(chla_rfu),
                          level = as.numeric(level)
                          )
NIW_isco <- NIW_isco %>% 
            dplyr::mutate(level = as.numeric(level),
                          remarks = as.character(remarks)
            )
OWC_isco <- OWC_isco %>% 
            dplyr::mutate(level = as.numeric(level)
                          )
PDB_isco <- PDB_isco %>% 
            dplyr::mutate(level = as.numeric(level),
            )
WEL_isco <- WEL_isco %>% 
            dplyr::mutate(remarks = as.character(remarks)
                          )

# bind all into one
isco <- dplyr::bind_rows(ELK_isco,
                         GND_isco,
                         GRB_isco,
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
   GRB_isco,
   GTM_isco,
   HEE_isco,
   LKS_isco,
   NIW_isco, 
   OWC_isco,
   PDB_isco, 
   WEL_isco)

## 03.2 tank files ----

GTM_tank <- GTM_tank %>% 
   dplyr::mutate(remarks = as.character(remarks)
   )

# compare all tank dfs
# only can compare two at a time, so just interchange which two dfs to use
# janitor::compare_df_cols_same(GTM_tank, GRB_tank,
#                               bind_method = "bind_rows")

GND_tank <- GND_tank %>% 
            dplyr::mutate(chla_rfu = as.numeric(chla_rfu)
                          )
GRB_tank <- GRB_tank %>% 
            dplyr::mutate(f_domrfu = as.numeric(f_domrfu)
                          )
HEE_tank <- HEE_tank %>% 
            dplyr::mutate(remarks = as.character(remarks),
                          chla_rfu = as.numeric(chla_rfu)
                          )
LKS_tank <- LKS_tank %>% 
            dplyr::mutate(chla_rfu = as.numeric(chla_rfu),
                          sample_no = as.numeric(sample_no)
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
                          station_code = as.character(station_code)
                          )

# bind all into one
tank <- dplyr::bind_rows(GND_tank,
                         GRB_tank,
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
   GRB_tank,
   HEE_tank,
   LKS_tank,
   MAR_tank,
   NIW_tank,
   PDB_tank, 
   SAP_tank)

## 03.3 all files ----

# janitor::compare_df_cols_same(isco, tank, 
#                               bind_method = "bind_rows")

all <- dplyr::bind_rows(isco, tank) %>% 
       dplyr::filter(rep == 1 | is.na(rep)) %>%  # only keep rep 1 from tank, and NAs, which would be the isco data
       dplyr::mutate(sample_no = as.character(sample_no),
                     isco_deployment_no = as.character(isco_deployment_no),
                     date_collected = as.Date(datetime_collected)) 

## 04 export ----

# write.xlsx(all, file = here::here('output', '2021_chla-catalyst_data_all_2.0.xlsx'),
#            sheetName = "raw",
#            showNA = FALSE)

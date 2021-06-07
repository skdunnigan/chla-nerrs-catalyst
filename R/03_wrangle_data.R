# 00 load packages ----
# source('R/00_load-packages.R')

# 01 read in data ----
# source('R/02_load_data.R')

# 02 wrangle for site-specific differences in data ----

## 02.1 Mission Aransas (MAR)----
# takes subsamples of replicates. Need to create two columns for replicates to separate out the subsamples

MAR_tank %>% 
  dplyr::filter(rep == 1.1) %>% 
  tidyr::separate(rep,
                into = c("rep", "subsample"),
                sep = "[.]") %>% 
  dplyr::select(-subsample)

## 02.2 Old Woman Creek (OWC)----
# takes replicate of last isco sample. 

OWC_isco %>% 
  tidyr::separate(sample_no,
                  into = c("sample_no", "rep_isco"),
                  sep = "[.]") %>% 
  dplyr::filter(rep_isco == 1 | is.na(rep_isco)) %>% # keep only the NA and 1 values
  dplyr::select(-rep_isco)


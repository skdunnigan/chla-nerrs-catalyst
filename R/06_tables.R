
# summary table for individual reserve and all runs -----------------------


reserve_sum_table_all <- function(site) {
  all %>%
      dplyr::filter(reserve_code == site) %>% 
      select(chla_ugl, chla_rfu, fdom_qsu, do_mgl, sal, turb, temp, p_h, chlorophyll_rfu) %>%
      psych::describe() %>%
      as_tibble(rownames="parameter")  %>%
      select(-vars) 
  
}

# a <- reserve_sum_table('SAP')


# summary table for individual reserve and only ISCO ----------------------


# just isco data
reserve_sum_table_isco <- function(site) {
  isco %>%
    dplyr::filter(reserve_code == site) %>% 
    select(chla_ugl, chla_rfu, fdom_qsu, do_mgl, sal, turb, temp, p_h, chlorophyll_rfu) %>%
    psych::describe() %>%
    as_tibble(rownames="parameter")  %>%
    select(-vars) 
  
}

# reserve_sum_table_isco("GTM")


# summary table for individual reserve and only tank runs -----------------

# just tank data
reserve_sum_table_tank <- function(site) {
  tank %>%
    dplyr::filter(reserve_code == site & rep == 1) %>% 
    select(chla_ugl, chla_rfu, fdom_qsu, do_mgl, sal, turb, temp, p_h, chlorophyll_rfu) %>%
    psych::describe() %>%
    as_tibble(rownames="parameter")  %>%
    select(-vars) 
  
}

# reserve_sum_table_tank("GTM")


# make summary table of all reserves --------------------------------------


reserves <- unique(all$reserve_code)

sum_tables <- list()

for (i in 1:length(reserves)){

# reserve_sum_table_all <- function(site) {
  
 tempdf <- all %>%
             dplyr::filter(reserve_code == reserves[i]) %>% 
             select(chla_ugl, chla_rfu, fdom_qsu, do_mgl, sal, turb, temp, p_h, chlorophyll_rfu) %>%
             psych::describe() %>%
             as_tibble(rownames="parameter")  %>%
             select(-vars) 
  
  sum_tables[[i]] <- tempdf # add file to opened list()
  
  name <- reserves[i] # pull out the name you want of the file
  
  # names[[i]] <- paste(name)
  
  rm(tempdf)
}

names(sum_tables) <- paste0(c(reserves))

sum_table_alpha <- purrr::map_df(sum_tables, ~as.data.frame(.x), .id="id")

sum_table_beta <- sum_table_alpha %>% 
                  dplyr::mutate(item = paste0(round(min, digits = 2), "-", round(max, digits = 2), " (", round(mean, digits = 2), "\U00B1", round(sd, digits = 2), ")"))

sum_table_final <- sum_table_beta %>% 
                    dplyr::select(id, parameter, item) %>% 
                    dplyr::filter(item != 'Inf--Inf (NaN±NA)') %>% 
                    tidyr::pivot_wider(id_cols = c('id'),
                                       names_from = 'parameter',
                                       values_from = 'item') %>% 
                    dplyr::rename(Reserve = id,
                                  `Extracted CHLA (ugL)` = chla_ugl,
                                  `Extracted CHLA (RFU)` = chla_rfu,
                                  `fDOM (QSU)` = fdom_qsu,
                                  `Dissolved Oxygen (mg/L)` = do_mgl,
                                  `Salinity (psu)` = sal,
                                  `Turbidity (FNU)` = turb,
                                  `Temperature (C)` = temp,
                                  `pH` = p_h,
                                  `CHLA (RFU) EXO` = chlorophyll_rfu)

rm(sum_tables, 
   sum_table_alpha, 
   sum_table_beta,
   reserves)
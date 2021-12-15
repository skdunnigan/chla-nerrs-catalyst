# make summary table of all reserves --------------------------------------
reserves <- unique(all$reserve_code)

sum_tables <- list()

for (i in 1:length(reserves)){
  
  # reserve_sum_table_all <- function(site) {
  
  tempdf_1 <- all %>%
    dplyr::filter(reserve_code == reserves[i]) %>% 
    select(chla_ugl, fdom_qsu, sal, turb, p_h, chlorophyll_rfu) %>%
    psych::describe() %>%
    as_tibble(rownames="parameter")  %>%
    select(-vars) 
  tempdf_2 <- all %>%
    dplyr::filter(reserve_code == reserves[i] & method == "isco") %>% 
    select(temp, do_mgl) %>%
    psych::describe() %>%
    as_tibble(rownames="parameter")  %>%
    select(-vars)
  
  tempdf <- dplyr::bind_rows(tempdf_1, tempdf_2)
  
  
  sum_tables[[i]] <- tempdf # add file to opened list()
  
  name <- reserves[i] # pull out the name you want of the file
  
  # names[[i]] <- paste(name)
  
  rm(tempdf_1,
     tempdf_2,
     tempdf)
}

names(sum_tables) <- paste0(c(reserves))

sum_table_alpha <- purrr::map_df(sum_tables, ~as.data.frame(.x), .id="id")

sum_table_beta <- sum_table_alpha %>%
  filter(n != 0) %>% 
  dplyr::mutate(item = paste0(round(min, digits = 2), "-", round(max, digits = 2), " (", round(mean, digits = 2), "\U00B1", round(sd, digits = 2), ")"))

sum_table_final <- sum_table_beta %>% 
                      dplyr::select(id, parameter, item) %>% 
                      dplyr::filter(item != 'Inf--Inf (NaN±NA)') %>% 
                      tidyr::pivot_wider(id_cols = c('id'),
                                         names_from = 'parameter',
                                         values_from = 'item') %>%
                      dplyr::rename(Reserve = id,
                                    `Extracted CHLA (ugL)` = chla_ugl,
                                    `fDOM (QSU)` = fdom_qsu,
                                    `Dissolved Oxygen (mg/L)` = do_mgl,
                                    `Salinity (psu)` = sal,
                                    `Turbidity (FNU)` = turb,
                                    `Temperature (C)` = temp,
                                    `pH` = p_h,
                                    `CHLA (RFU) EXO` = chlorophyll_rfu) %>% 
                      dplyr::arrange(Reserve)

rm(sum_tables, 
   sum_table_alpha, 
   sum_table_beta,
   reserves)
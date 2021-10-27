
# summary table for individual reserve and all runs -----------------------


reserve_sum_table_all <- function(site) {
  all %>%
      dplyr::filter(reserve_code == site) %>%
      select(chla_ugl, chla_rfu, fdom_qsu, sal, turb, p_h, chlorophyll_rfu) %>%
      psych::describe() %>%
      as_tibble(rownames="parameter")  %>%
      select(-vars)
}

# a <- reserve_sum_table_all('SAP')


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
    select(chla_ugl, chla_rfu, fdom_qsu, sal, turb, p_h, chlorophyll_rfu) %>%
    psych::describe() %>%
    as_tibble(rownames="parameter")  %>%
    select(-vars) 
  
}

# reserve_sum_table_tank("GTM")



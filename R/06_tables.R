# create tables

# params <- c('chla_ugl', 'chla_rfu', 'fdom_qsu', 'do_mgl', 'sal', 'turb', 'temp', 'p_h', 'chlorophyll_rfu')
# 
# for (i in 1:length(params)){
#   
#   min_max <- all %>% 
#     group_by(reserve_code) %>% 
#     select(reserve_code, chla_ugl, chla_rfu, fdom_qsu, do_mgl, sal, turb, temp, p_h, chlorophyll_rfu) %>% 
#     summarise(across(everything(), list(min = min, max = max)))
#   
#   mean <- all %>% 
#     group_by(reserve_code) %>% 
#     select(reserve_code, chla_ugl, chla_rfu, fdom_qsu, do_mgl, sal, turb, temp, p_h, chlorophyll_rfu) %>% 
#     summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
#   
#   sd <- all %>% 
#     group_by(reserve_code) %>% 
#     select(reserve_code, chla_ugl, chla_rfu, fdom_qsu, do_mgl, sal, turb, temp, p_h, chlorophyll_rfu) %>% 
#     summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))
#   
#   combo <- left_join(min_max, mean, by = "reserve_code") %>% left_join(sd, by = "reserve_code")
#   
#   tempdf <- combo %>% 
#     select(reserve_code, contains(params[i])) %>% 
#     dplyr::mutate(range = paste0(min))
#   
#   name <- params[i] # pull out the name you want of the file
#   
#   assign(paste0(name, "_", 'sum'), tempdf) 
#   
#   rm(tempdf)
# }
# 
# a <- all %>% 
#       group_by(reserve_code) %>% 
#       summarise_all()

# reserve <- unique(all$reserve_code)
# 
# for (i in 1:length(reserve)){
# 
# tempdf <- all %>%
#                       dplyr::filter(reserve_code == reserve[i]) %>% 
#                       select(chla_ugl, chla_rfu, fdom_qsu, do_mgl, sal, turb, temp, p_h, chlorophyll_rfu) %>%
#                       psych::describe(quant=c(.25,.75)) %>%
#                       as_tibble(rownames="rowname")  %>%
#                       print()
#   name <- reserve[i] # pull out the name you want of the file
#   
#   assign(paste0(name, "_", 'sum'), tempdf) 
#   
#   rm(tempdf)
# }

reserve_sum_table_all <- function(site) {
  all %>%
      dplyr::filter(reserve_code == site) %>% 
      select(chla_ugl, chla_rfu, fdom_qsu, do_mgl, sal, turb, temp, p_h, chlorophyll_rfu) %>%
      psych::describe() %>%
      as_tibble(rownames="parameter")  %>%
      select(-vars) 
  
}

# a <- reserve_sum_table('SAP')

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

# just tank data
reserve_sum_table_tank <- function(site) {
  tank %>%
    dplyr::filter(reserve_code == site) %>% 
    select(chla_ugl, chla_rfu, fdom_qsu, do_mgl, sal, turb, temp, p_h, chlorophyll_rfu) %>%
    psych::describe() %>%
    as_tibble(rownames="parameter")  %>%
    select(-vars) 
  
}

# reserve_sum_table_tank("GTM")
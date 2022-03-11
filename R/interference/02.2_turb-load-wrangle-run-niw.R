# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
# source('R/00_load-packages.R')
# source('R/00_vis_custom.R')

# 01 load-data ---------------------------------------------------------------

dat_1 <- readxl::read_xlsx(here::here('analysis', 'interference-data', 'turb_niw_1.xlsx'), 
                           sheet = 'Averages data') %>%
          janitor::clean_names() %>% 
          dplyr::mutate(run = 1) %>% 
          dplyr::rename(avg_turbidity_fnu = average_turb_fnu,
                        avg_chl_rfu = average_chl_rfu,
                        avg_fdom_qsu = average_fdom_qsu)

dat_2 <- readxl::read_xlsx(here::here('analysis', 'interference-data', 'turb_niw_2.xlsx'), 
                           sheet = 'Averages data') %>%
          janitor::clean_names() %>% 
          dplyr::mutate(run = 2) 
        
dat_3 <- readxl::read_xlsx(here::here('analysis', 'interference-data', 'turb_niw_3.xlsx'), 
                           sheet = 'Averages data') %>%
          janitor::clean_names() %>% 
          dplyr::mutate(run = 3)   

turb <- bind_rows(dat_1, dat_2, dat_3) %>% 
        dplyr::mutate(run = forcats::as_factor(run))
  
rm(dat_1,
   dat_2,
   dat_3)

## get models
lm_out <- turb %>%
          dplyr::group_by(run) %>%
          do(broom::tidy(lm(avg_chl_rfu ~ avg_turbidity_fnu, data = .)))

diag <- turb %>%
          group_by(run) %>%
          do(broom::glance(lm(avg_chl_rfu ~ avg_turbidity_fnu, data = .)))

##check out model outputs
lm_out
diag

## examine slope:intercept
m <- lm_out %>%
      dplyr::filter(term == 'avg_turbidity_fnu') %>%
      dplyr::rename(m = estimate) %>%
      dplyr::select(-term)

b <- lm_out %>%
      dplyr::filter(term == '(Intercept)') %>%
      dplyr::select(run, estimate) %>%
      dplyr::rename(b = estimate)

equations <- dplyr::left_join(m, b, by = "run") %>%
             dplyr::left_join(diag, by = "run") %>% 
             dplyr::mutate(equation = paste0('y =', round(m, digits = 7), 'x + ', round(b, digits = 3)),
                           values = paste0('R2 = ',round(r.squared, digits = 3), ' p', pvalue(p.value.x))
                           )


run_1_eq <- equations[[1,19]]
run_2_eq <- equations[[2,19]]
run_3_eq <- equations[[3,19]]

run_1_stat <- equations[[1,20]]
run_2_stat <- equations[[2,20]]
run_3_stat <- equations[[3,20]]

turb_interf_niw <- turb %>% 
                      ggplot(aes(x = avg_turbidity_fnu, y = avg_chl_rfu)) +
                        geom_point(position = "jitter") +
                        stat_smooth(method = "lm", aes(color = run),
                                    se = FALSE,
                                    fullrange = T) +
                        theme_bw() +
                        theme(axis.text = element_text(size = 12, color = 'black'),
                              axis.title = element_text(size = 12)) +
                        labs(y = chla_RFU_title,
                             x = 'Turbidity (FNU)') +
                        annotate("text",
                                 x = 200,
                                 y = 3.5,
                                 size = 3.5,
                                 label = run_3_eq) +
                        annotate("text",
                                 x = 200,
                                 y = 3.4,
                                 size = 3.5,
                                 label = run_3_stat) +
                        annotate("text",
                                 x = 200,
                                 y = 2.4,
                                 size = 3.5,
                                 label = run_2_eq) +
                        annotate("text",
                                 x = 200,
                                 y = 2.3,
                                 size = 3.5,
                                 label = run_2_stat) +
                        annotate("text",
                                 x = 200,
                                 y = 1.6,
                                 size = 3.5,
                                 label = run_1_eq) +
                        annotate("text",
                                 x = 200,
                                 y = 1.5,
                                 size = 3.5,
                                 label = run_1_stat)
  
rm(equations, turb, m, b, lm_out, diag, 
   run_1_eq, run_1_stat,
   run_2_eq, run_2_stat,
   run_3_eq, run_3_stat)
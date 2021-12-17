# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
# source('R/00_load-packages.R')
# source('R/00_vis_custom.R')

# 01 load-data ---------------------------------------------------------------

turb <- readxl::read_xlsx(here::here('analysis', 'interference-data', 'turb_mar_1.xlsx'), 
                           sheet = 'Avgs') %>%
          janitor::clean_names()  


## get models
lm_out <- turb %>%
          do(broom::tidy(lm(average_chl_rfu ~ average_turb_fnu, data = .)))

diag <- turb %>%
          do(broom::glance(lm(average_chl_rfu ~ average_turb_fnu, data = .)))

##check out model outputs
lm_out
diag

## examine slope:intercept
m <- lm_out %>%
      dplyr::filter(term == 'average_turb_fnu') %>%
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

run_1_stat <- equations[[1,20]]
run_2_stat <- equations[[2,20]]


turb_interf_mar <- turb %>% 
                      ggplot(aes(x = average_turb_fnu, y = average_chl_rfu)) +
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
                                 y = 0.32,
                                 size = 3.5,
                                 label = run_1_eq) +
                        annotate("text",
                                 x = 200,
                                 y = 0.3,
                                 size = 3.5,
                                 label = run_1_stat) +
                        annotate("text",
                                 x = 200,
                                 y = 0.62,
                                 size = 3.5,
                                 label = run_2_eq) +
                        annotate("text",
                                 x = 200,
                                 y = 0.6,
                                 size = 3.5,
                                 label = run_2_stat) 

  
rm(equations, turb, m, b, lm_out, diag, 
   run_1_eq, run_1_stat,
   run_2_eq, run_2_stat)
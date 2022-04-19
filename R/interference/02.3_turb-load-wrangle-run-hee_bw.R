# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
# source('R/00_load-packages.R')
# source('R/00_vis_custom.R')

# 01 load-data ---------------------------------------------------------------

hee_turb <- bind_rows((readxl::read_xlsx(here::here('analysis', 'interference-data', 'turb_hee_1.xlsx'), 
                           sheet = 'Sheet1') %>%
          janitor::clean_names() %>% 
          dplyr::mutate(run = 1)),
(readxl::read_xlsx(here::here('analysis', 'interference-data', 'turb_hee_2.xlsx'), 
                           sheet = 'Sheet1') %>%
          janitor::clean_names() %>% 
          dplyr::mutate(run = 2))) %>% dplyr::mutate(run = forcats::as_factor(run))

## get models
lm_out <- hee_turb %>%
          dplyr::group_by(run) %>%
          do(broom::tidy(lm(average_chl_rfu ~ average_turb_fnu, data = .)))

diag <- hee_turb %>%
          group_by(run) %>%
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
             dplyr::mutate(equation = paste0('y = ', round(m, digits = 7), 'x + ', round(b, digits = 3)),
                           values = paste0('R2 = ',round(r.squared, digits = 3), ' p', pvalue(p.value.x))
                           )


run_1_eq <- equations[[1,19]]
run_2_eq <- equations[[2,19]]

run_1_stat <- equations[[1,20]]
run_2_stat <- equations[[2,20]]


turb_interf_hee <- hee_turb %>% 
                      ggplot(aes(x = average_turb_fnu, y = average_chl_rfu,
                                 shape = run, linetype = run)) +
                        geom_point(position = "jitter") +
                        stat_smooth(method = "lm", color = "black",
                                    se = FALSE,
                                    fullrange = T) +
                        theme_classic() +
                        theme(axis.text = element_text(size = 12, color = 'black'),
                              axis.title = element_text(size = 12),
                              plot.title = element_text(face = "bold")) +
                        labs(y = chla_RFU_title,
                             x = 'Turbidity (FNU)',
                             title = "HEE",
                             shape = "Run",
                             linetype = "Run") +
                        annotate("text",
                                 x = 320,
                                 y = 0.33,
                                 size = 3,
                                 label = run_1_eq) +
                        annotate("text",
                                 x = 320,
                                 y = 0.3,
                                 size = 3,
                                 label = run_1_stat) +
                        annotate("text",
                                 x = 300,
                                 y = 0.73,
                                 size = 3,
                                 label = run_2_eq) +
                        annotate("text",
                                 x = 300,
                                 y = 0.7,
                                 size = 3,
                                 label = run_2_stat) 

ggsave(turb_interf_hee, filename = here('output', 'bw', 'turb_interf_hee.png'),
       dpi = 300, height = 4, width = 5, units = "in")
  
rm(equations, hee_turb, m, b, lm_out, diag, 
   run_1_eq, run_1_stat,
   run_2_eq, run_2_stat)
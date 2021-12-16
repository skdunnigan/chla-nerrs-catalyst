# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
# source('R/00_load-packages.R')


# 01 load-data ---------------------------------------------------------------

dat_niw <- readxl::read_xlsx(here::here('analysis', 'interference-data', 'fdom_niw.xlsx'), 
                           sheet = 'averaged data') %>%
          janitor::clean_names() %>% 
  dplyr::mutate(reserve = "niw",
                run = 1)

dat_gtm <- readxl::read_xlsx(here::here('analysis', 'interference-data', 'fdom_gtm_1.xlsx'), 
                             sheet = 'Analysis') %>%
  janitor::clean_names() %>% 
  dplyr::mutate(reserve = "gtm", 
                run = 1)

dat_lks <- bind_rows((readxl::read_xlsx(here::here('analysis', 'interference-data', 'fdom_lks_1.xlsx'), 
                                        sheet = 'averaged data') %>%
                      janitor::clean_names() %>% 
                      dplyr::mutate(reserve = "lks",
                                    run = 1)), 
                     (readxl::read_xlsx(here::here('analysis', 'interference-data', 'fdom_lks_2.xlsx'), 
                                        sheet = 'averaged data') %>%
                      janitor::clean_names() %>% 
                      dplyr::mutate(reserve = "lks",
                                    run = 2)))

dat_owc <- bind_rows((readxl::read_xlsx(here::here('analysis', 'interference-data', 'fdom_owc_1.xlsx'), 
                                        sheet = 'averaged data_Lake') %>%
                      janitor::clean_names() %>% 
                      dplyr::mutate(reserve = "owc",
                                    run = 1)), 
                     (readxl::read_xlsx(here::here('analysis', 'interference-data', 'fdom_owc_2.xlsx'), 
                                        sheet = 'averaged data_Lake') %>%
                      janitor::clean_names() %>% 
                      dplyr::mutate(reserve = "owc",
                                    run = 2)),
                     (readxl::read_xlsx(here::here('analysis', 'interference-data', 'fdom_owc_3.xlsx'), 
                                        sheet = 'averaged data_Lake') %>%
                      janitor::clean_names() %>% 
                      dplyr::mutate(reserve = "owc",
                                    run = 3))) %>% 
          mutate(avg_start_time = as.POSIXct(avg_start_time, origin = "2021-01-01"))

janitor::compare_df_cols_same(dat_niw, dat_owc,
                              bind_method = "bind_rows")


fdom_all <- bind_rows(dat_niw, dat_gtm, dat_lks, dat_owc) %>% mutate(reserve_run = paste(reserve, "_", run))



fdom_interf <- fdom_all %>% 
                ggplot(aes(x = avg_fdom_qsu, y = avg_chl_rfu, 
                           color = reserve,
                           group = reserve_run)) +
                  geom_point(position = "jitter") +
                  stat_smooth(method = "lm",
                              se = FALSE,
                              fullrange = F) +
                  # geom_point(aes(y = corrected_avg_chl_rfu), color = "red") +
                  # stat_smooth(aes(y = corrected_avg_chl_rfu),
                  #             method = "lm",
                  #             color = 'red',
                  #             se = FALSE,
                  #             fullrange = T) +
                  # ggpubr::stat_regline_equation(label.y = 3.4) +
                  # ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                  #                  label.y = 3.3) + # add R2 and p value
                  theme_bw() +
                  theme(axis.text = element_text(size = 12, color = 'black'),
                        axis.title = element_text(size = 12)) +
                  labs(y = chla_RFU_title,
                       x = 'fDOM (QSU)')
  

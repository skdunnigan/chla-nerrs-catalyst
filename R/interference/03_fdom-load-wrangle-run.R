# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
source('R/00_load-packages.R')
source('R/00_vis_custom.R')

# load data version 2, adding variables "Matrix" and "Standard" to data. ----

ambient_fdom <- dplyr::bind_rows(
                  (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                                'fdom_std-gtm_ambient-gtm_1.xlsx'), 
                                    sheet = 'Sheet1') %>%
                    janitor::clean_names() %>% 
                    dplyr::mutate(standard = "GTM",
                                  run = 1)
                  ),
                  (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                                'fdom_std-gtm_ambient-gtm_2.xlsx'), 
                                     sheet = 'Sheet1') %>%
                     janitor::clean_names() %>% 
                     dplyr::mutate(standard = "GTM",
                                   run = 2)
                  ),
                  (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                                'fdom_std-suwannee_ambient-gtm_1.xlsx'), 
                                     sheet = 'Analysis') %>%
                     janitor::clean_names() %>% 
                     dplyr::mutate(standard = "Suwannee",
                                   run = 1)
                  ),
                  (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                                'fdom_std-lks_ambient-lks_1.xlsx'), 
                                     sheet = 'averaged data') %>%
                     janitor::clean_names() %>% 
                     dplyr::mutate(standard = "LKS",
                                   run = 1)
                  ),
                  (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                                'fdom_std-lks_ambient-gtm.xlsx'), 
                                     sheet = 'Sheet1') %>%
                     janitor::clean_names() %>% 
                     dplyr::mutate(standard = "LKS",
                                   run = 2)
                  ),
                  (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                                'fdom_std-niw_ambient-niw.xlsx'), 
                                     sheet = 'averaged data') %>%
                     janitor::clean_names() %>% 
                     dplyr::mutate(standard = "NIW",
                                   run = 1)
                  ),
                  (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                                'fdom_std-niw_ambient-gtm.xlsx'), 
                                     sheet = 'Sheet1') %>%
                     janitor::clean_names() %>% 
                     dplyr::mutate(standard = "NIW",
                                   run = 2)
                  ),
                  (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                                'fdom_std-owc_ambient-owc_DI_1.xlsx'), 
                                     sheet = 'averaged data_Lake') %>%
                     janitor::clean_names() %>% 
                     dplyr::mutate(standard = "OWC",
                                   run = 1,
                                   avg_start_time = as.POSIXct(avg_start_time, origin = "2021-01-01"))
                  ),
                  (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                                'fdom_std-owc_ambient-owc_2.xlsx'), 
                                     sheet = 'averaged data_Lake') %>%
                     janitor::clean_names() %>% 
                     dplyr::mutate(standard = "OWC",
                                   run = 2,
                                   avg_start_time = as.POSIXct(avg_start_time, origin = "2021-01-01"))
                  ),
                  (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                                'fdom_std-owc_ambient-owc_3.xlsx'), 
                                     sheet = 'averaged data_Lake') %>%
                     janitor::clean_names() %>% 
                     dplyr::mutate(standard = "OWC",
                                   run = 3,
                                   avg_start_time = as.POSIXct(avg_start_time, origin = "2021-01-01"))
                  )
                ) %>% 
                dplyr::mutate(matrix = "ambient", 
                              standard_run = paste(standard, "-", run))
  
DI_fdom <- dplyr::bind_rows(
              (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                            'fdom_std-owc_ambient-owc_DI_1.xlsx'), 
                                 sheet = 'averaged data_DI') %>%
                 janitor::clean_names() %>% 
                 dplyr::mutate(standard = "OWC",
                               run = 1,
                               avg_start_time = as.POSIXct(avg_start_time, origin = "2021-01-01"))
              ),
              (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                            'fdom_std-owc_DI_2.xlsx'), 
                                 sheet = 'averaged data_DI') %>%
                 janitor::clean_names() %>% 
                 dplyr::mutate(standard = "OWC",
                               run = 2,
                               avg_start_time = as.POSIXct(avg_start_time, origin = "2021-01-01"))
              ),
              (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                            'fdom_std-owc_DI_3.xlsx'), 
                                 sheet = 'averaged data_DI') %>%
                 janitor::clean_names() %>% 
                 dplyr::mutate(standard = "OWC",
                               run = 2,
                               avg_start_time = as.POSIXct(avg_start_time, origin = "2021-01-01"))
              ),
              (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                            'fdom_std-lks_DI_1.xlsx'), 
                                 sheet = 'averaged data') %>%
                 janitor::clean_names() %>% 
                 dplyr::mutate(standard = "LKS",
                               run = 1)
              ),
              (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                            'fdom_std-niw_DI.xlsx'), 
                                 sheet = 'averaged data') %>%
                 janitor::clean_names() %>% 
                 dplyr::mutate(standard = "NIW",
                               run = 1)
              )
              ) %>% 
            dplyr::mutate(standard_run = paste(standard, "-", run))

stds_gtm <- dplyr::bind_rows(
              (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                            'fdom_std-lks_ambient-gtm.xlsx'), 
                                 sheet = 'Sheet1') %>%
                 janitor::clean_names() %>% 
                 dplyr::mutate(standard = "LKS",
                               run = 1)
              ),
              (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                            'fdom_std-niw_ambient-gtm.xlsx'), 
                                 sheet = 'Sheet1') %>%
                 janitor::clean_names() %>% 
                 dplyr::mutate(standard = "NIW",
                               run = 1)
              ),
              (readxl::read_xlsx(here::here('analysis', 'interference-data', 
                                            'fdom_std-gtm_ambient-gtm_2.xlsx'), 
                                 sheet = 'Sheet1') %>%
                 janitor::clean_names() %>% 
                 dplyr::mutate(standard = "GTM",
                               run = 1)
              )
              ) %>% 
            mutate(standard_run = paste(standard, "-", run))

# 02 plotting ----  

a <- DI_fdom %>% 
      ggplot(aes(x = avg_fdom_qsu, y = corrected_avg_chl_rfu,
                 color = standard,
                 group = standard_run)) +
      geom_point() +
      stat_smooth(method = "lm",
                  se = FALSE,
                  fullrange = F) +
  # ylim(0,6.5) +
      scale_color_manual(name = "Standard", values = standard_colors) +
      theme_bw() +
      theme(axis.text = element_text(size = 12, color = 'black'),
            axis.title = element_text(size = 12)) +
      labs(y = chla_RFU_title,
           x = 'fDOM (QSU)')
  
b <- stds_gtm %>% 
      ggplot(aes(x = avg_fdom_qsu, y = corrected_avg_chl_rfu,
                 color = standard,
                 group = standard_run)) +
      geom_point() +
      stat_smooth(method = "lm",
                  se = FALSE,
                  fullrange = F) +
      scale_color_manual(name = "Standard", values = standard_colors) +
      theme_bw() +
      theme(axis.text = element_text(size = 12, color = 'black'),
            axis.title = element_text(size = 12)) +
      labs(y = chla_RFU_title,
           x = 'fDOM (QSU)')

c <- ambient_fdom %>% 
      ggplot(aes(x = avg_fdom_qsu, y = corrected_avg_chl_rfu,
                 color = standard,
                 group = standard_run)) +
      geom_point() +
      stat_smooth(method = "lm",
                  se = FALSE,
                  fullrange = F) +
      scale_color_manual(name = "Standard", values = standard_colors) +
      theme_bw() +
      theme(axis.text = element_text(size = 12, color = 'black'),
            axis.title = element_text(size = 12)) +
      labs(y = chla_RFU_title,
           x = 'fDOM (QSU)')

fdom_multiplot <- ((a + 
    theme(legend.position = "none") + 
    labs(title = "A", x = "")) + 
    (b + 
       theme(legend.position = "none") +
       labs(title = "B", x = "", y = ""))
  ) / 
  (c + theme(legend.position = "bottom") +
     labs(title = "C")) +
  plot_annotation(caption = "Note different scales for y axis.")
  

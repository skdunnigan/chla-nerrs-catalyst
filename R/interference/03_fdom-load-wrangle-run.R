# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
# source('R/00_load-packages.R')
# source('R/00_vis_custom.R')

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
                                                'fdom_std-humic_ambient-gtm_1.xlsx'), 
                                     sheet = 'Analysis') %>%
                     janitor::clean_names() %>% 
                     dplyr::mutate(standard = "Humic",
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

# color
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
  
# b&w

a_bw <- DI_fdom %>% 
        ggplot(aes(x = avg_fdom_qsu, y = corrected_avg_chl_rfu,
                   linetype = standard,
                   shape = standard,
                   group = standard_run)) +
        geom_point() +
  # xlim(0,300) +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = F,
                    color = "black",
                    size = 0.5) +
        scale_shape_manual(name = "Standard", values = standard_shapes) +
        scale_linetype_manual(name = "Standard", values = standard_linetypes) +
  scale_x_continuous(limits = c(0, 300), breaks = seq(0, 300, by = 50)) +
        theme_classic() +
        theme(axis.text = element_text(size = 12, color = "black"),
              axis.title = element_text(size = 12, color = "black"),
              plot.title = element_text(color = "black",
                                   face = "bold"),
              legend.text = element_text(size = 12, color = "black"),
              legend.title = element_text(size = 12, color = "black")
        ) +
        labs(y = chla_RFU_title,
             x = 'fDOM (QSU)')

b_bw <- stds_gtm %>% 
        ggplot(aes(x = avg_fdom_qsu, y = corrected_avg_chl_rfu,
                   linetype = standard,
                   shape = standard,
                   group = standard_run)) +
        geom_point(size = 2) +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = F,
                    color = "black") +
        scale_shape_manual(name = "Standard", values = standard_shapes) +
        scale_linetype_manual(name = "Standard", values = standard_linetypes) +
        theme_bw() +
        theme(axis.text = element_text(size = 12, color = 'black'),
              axis.title = element_text(size = 12)) +
        labs(y = chla_RFU_title,
             x = 'fDOM (QSU)')

c_bw <- ambient_fdom %>%
        filter(standard != "Humic") %>% 
        ggplot(aes(x = avg_fdom_qsu, y = corrected_avg_chl_rfu,
                   linetype = standard,
                   shape = standard,
                   group = standard_run)) +
        geom_point() +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = F,
                    color = "black",
                    size = 0.5) +
        scale_shape_manual(name = "Standard", values = standard_shapes) +
  scale_x_continuous(breaks = seq(0, 300, by = 50), limits = c(0, 300)) +
        scale_linetype_manual(name = "Standard", values = standard_linetypes) +
        theme_classic() +
        theme(axis.text = element_text(size = 12, color = "black"),
              axis.title = element_text(size = 12, color = "black"),
              plot.title = element_text(color = "black",
                                   face = "bold"),
              legend.text = element_text(size = 12, color = "black"),
              legend.title = element_text(size = 12, color = "black")
              ) +
        labs(y = chla_RFU_title,
             x = 'fDOM (QSU)')

# to make one y -axix
# fdom_p4 <- ggplot(data.frame(l = a_bw$labels$y, x = 1, y = 1)) +
#   geom_text(aes(x, y, label = l), angle = 90, size = 5) + 
#   theme_void() +
#   coord_cartesian(clip = "off")

fdom_multiplot_bw <- (a_bw + 
                      theme(legend.position = "none", axis.text.x = element_blank()) + 
                      labs(title = "A", x = "")) / 
  (c_bw + theme(legend.position = "bottom") +
     labs(title = "B"))

ggsave(fdom_multiplot_bw, filename = here('output', 'bw', 'fdom_multiplot.png'),
       dpi = 300, height = 6, width = 5, units = "in")
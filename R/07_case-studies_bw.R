# for case studies data

# ---- 01 import and wrangle data ----
elk <- readxl::read_xlsx(here::here('final-project-files', 
                                    'case-studies',
                                    'ELK_case-studies.xlsx'),
                         sheet = "data_skd") %>% 
  mutate(reserve = "ELK") %>% 
  janitor::clean_names()
gtm <- readxl::read_xlsx(here::here('final-project-files', 
                                    'case-studies',
                                    'GTM_case-studies.xlsx'),
                         sheet = "data_skd") %>% 
  mutate(reserve = "GTM",
         Chla_RFU = as.numeric(Chla_RFU),
         FDOM_QSU = as.numeric(FDOM_QSU),
         FDOM_RFU = as.numeric(FDOM_RFU),
         Temp_C = as.numeric(Temp_C),
         Turb_FNU = as.numeric(Turb_FNU)) %>% 
  janitor::clean_names()

owc <- readxl::read_xlsx(here::here('final-project-files', 
                                    'case-studies',
                                    'OWC_case-studies.xlsx'),
                         sheet = "R") %>% 
  mutate(reserve = "OWC") %>% 
  janitor::clean_names()

# case_dat <- bind_rows(elk, gtm, owc) %>% janitor::clean_names()
# rm(elk, gtm, owc)

# ---- 02 plots ----
lims <- as.POSIXct(strptime(c("2021-01-01 00:00","2021-12-31 23:45"), 
                            format = "%Y-%m-%d %H:%M"))

elk_plot <- elk %>% 
              ggplot(aes(x = datetime_collected, y = chla_ugl)) +
              geom_point(data = filter(elk, type == "Sonde"), 
                         aes(shape = "Sonde"),
                         color = "grey70") +
              geom_point(data = filter(elk, type == "ISCO"),
                         aes(shape = "Diel"),
                         size = 2) +
              geom_point(data = filter(elk, type == "SWMP Grab"),
                         aes(shape = "Grab"), 
                         size = 3) +
              scale_shape_manual(name = '', 
                                 values = c('Grab'= 8, 
                                            'Diel'= 19, 
                                            'Sonde'= 1)) +
              scale_x_datetime(date_breaks = "month", date_labels = "%b",
                               limits = lims) +
              scale_y_continuous(expand = c(0,0)) +
              theme_classic() +
              theme(axis.text = element_text(color = "black", size = 12),
                    axis.title = element_text(color = "black", size = 12),
                    legend.text = element_text(color = "black", size = 12)) +
              labs(x = "",
                   y = chla_extr_title2)
  
gtm_plot <- gtm %>% 
              ggplot(aes(x = datetime_collected, y = chla_ugl)) +
              geom_point(data = filter(gtm, type == "Sonde"), 
                         aes(shape = "Sonde"), 
                         color = "grey70") +
              geom_point(data = filter(gtm, type == "ISCO"),
                         aes(shape = "Diel"),
                         size = 2) +
              geom_point(data = filter(gtm, type == "SWMP Grab"),
                         aes(shape = "Grab"),
                         size = 3) +
              scale_shape_manual(name = '', 
                                 values = c('Grab'= 8, 
                                            'Diel'= 19, 
                                            'Sonde'= 1)) +
              scale_x_datetime(date_breaks = "month", date_labels = "%b") +
              scale_y_continuous(expand = c(0,0)) +
              theme_classic() +
              theme(axis.text = element_text(color = "black", size = 12),
                    axis.title = element_text(color = "black", size = 12),
                    legend.text = element_text(color = "black", size = 12)) +
              labs(x = "",
                   y = chla_extr_title2)

gtm_plot_may2020 <- gtm %>% 
              ggplot(aes(x = datetime_collected, y = chla_ugl)) +
              geom_point(data = filter(gtm, type == "Sonde" & datetime_collected > "2020-05-01" & datetime_collected <"2020-06-01"), 
                         aes(shape = "Sonde"), 
                         color = "grey70") +
              geom_point(data = filter(gtm, type == "ISCO" & datetime_collected > "2020-05-01" & datetime_collected <"2020-06-01"),
                         aes(shape = "Diel"),
                         size = 2) +
              geom_point(data = filter(gtm, type == "SWMP Grab" & datetime_collected > "2020-05-01" & datetime_collected <"2020-06-01"),
                         aes(shape = "Grab"),
                         size = 3) +
              scale_shape_manual(name = '', 
                                 values = c('Grab'= 8, 
                                            'Diel'= 19, 
                                            'Sonde'= 1)) +
              scale_x_datetime(date_breaks = "day", date_labels = "%d") +
              scale_y_continuous(expand = c(0,0)) +
              theme_classic() +
              theme(axis.text = element_text(color = "black", size = 12),
                    axis.title = element_text(color = "black", size = 12),
                    legend.text = element_text(color = "black", size = 12)) +
              labs(x = "",
                   y = chla_extr_title2)

gtm_plot_oct2020 <- gtm %>% 
                      ggplot(aes(x = datetime_collected, y = chla_ugl)) +
                      geom_point(data = filter(gtm, type == "Sonde" & datetime_collected > "2020-10-01" & datetime_collected <"2020-11-01"), 
                                 aes(shape = "Sonde"), 
                                 color = "grey70") +
                      geom_point(data = filter(gtm, type == "ISCO" & datetime_collected > "2020-10-01" & datetime_collected <"2020-11-01"),
                                 aes(shape = "Diel"),
                                 size = 2) +
                      geom_point(data = filter(gtm, type == "SWMP Grab" & datetime_collected > "2020-10-01" & datetime_collected <"2020-11-01"),
                                 aes(shape = "Grab"),
                                 size = 3) +
                      scale_shape_manual(name = '', 
                                         values = c('Grab'= 8, 
                                                    'Diel'= 19, 
                                                    'Sonde'= 1)) +
                      scale_x_datetime(date_breaks = "day", date_labels = "%d") +
                      scale_y_continuous(expand = c(0,0)) +
                      theme_classic() +
                      theme(axis.text = element_text(color = "black", size = 12),
                            axis.text.x = element_text(angle = 90, vjust = 0.5),
                            axis.title = element_text(color = "black", size = 12),
                            legend.text = element_text(color = "black", size = 12)) +
                      labs(x = "",
                           y = chla_extr_title2)

owc_plot <- owc %>% 
              ggplot(aes(x = datetime_collected, y = chla_ugl)) +
              geom_point(data = filter(owc, type == "Sonde"), 
                         aes(shape = "Sonde"), 
                         color = "grey70") +
              geom_point(data = filter(owc, type == "ISCO"),
                         aes(shape = "Diel"),
                         size = 2) +
              geom_point(data = filter(owc, type == "SWMP Grab"),
                         aes(shape = "Grab"),
                         size = 3) +
              scale_shape_manual(name = '', 
                                 values = c('Grab'= 8, 
                                            'Diel'= 19, 
                                            'Sonde'= 1)) +
              scale_x_datetime(date_breaks = "month", date_labels = "%b",
                               limits = lims) +
              scale_y_continuous(expand = c(0,0)) +
              theme_classic() +
              theme(axis.text = element_text(color = "black", size = 12),
                    axis.title = element_text(color = "black", size = 12),
                    legend.text = element_text(color = "black", size = 12)) +
              labs(x = "",
                   y = chla_extr_title2)

multiplot_a <- (elk_plot + theme(legend.position = "none",
                  axis.text.x = element_blank()) + 
    labs(title = "Elkhorn Slough, CA")) / 
  (gtm_plot + theme(legend.position = "none",
                    axis.text.x = element_blank()) +
     labs(title = "Guana Tolomato Matanzas, FL")) / 
  (owc_plot + theme(legend.position = "bottom") +
     labs(title = "Old Woman Creek, OH"))

multiplot_b <- (elk_plot + theme(legend.position = "none",
                  axis.text.x = element_blank()) + 
    labs(title = "A")) / 
  (gtm_plot + theme(legend.position = "none",
                    axis.text.x = element_blank()) +
     labs(title = "B")) / 
  (owc_plot + theme(legend.position = "bottom") +
     labs(title = "C"))

multiplot_d <- (gtm_plot_may2020 + 
                   theme(legend.position = "none",
                         axis.text.x = element_blank()) +
                   labs(title = "May 2020")) / 
                  (gtm_plot_oct2020 + 
                     theme(legend.position = "bottom") +
                     labs(title = "October 2020"))

# to make one y -axis
p4 <- ggplot(data.frame(l = elk_plot$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90, size = 4, family = "serif") + 
  theme_void() +
  coord_cartesian(clip = "off")

multiplot_c <- p4 + (elk_plot + theme(legend.position = "none",
                       axis.text.x = element_blank()) + 
        labs(title = "Elkhorn Slough, CA", y = "")) / 
  (gtm_plot + theme(legend.position = "none",
                    axis.text.x = element_blank()) +
     labs(title = "Guana Tolomato Matanzas, FL", y = "")) / 
  (owc_plot + theme(legend.position = "bottom") +
     labs(title = "Old Woman Creek, OH", y = "")) + plot_layout(widths = c(1, 25))

ggsave(multiplot_a, filename = here('output', 'bw', 'case-studies_a.png'),
       dpi = 300, height = 7, width = 5, units = "in")
# ggsave(multiplot_b, filename = here('output', 'bw', 'case-studies_b.png'),
#        dpi = 300, height = 7, width = 5, units = "in")
# ggsave(multiplot_c, filename = here('output', 'bw', 'case-studies_c.png'),
#        dpi = 300, height = 7, width = 5, units = "in")
ggsave(multiplot_d, filename = here('output', 'bw', 'case-studies_d-gtm.png'),
       dpi = 300, height = 6, width = 6, units = "in")

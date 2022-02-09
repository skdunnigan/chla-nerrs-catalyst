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
                         aes(color = "Sonde"), 
                         shape = 1) +
              geom_point(data = filter(elk, type == "ISCO"),
                         aes(color = "Diel"),
                         size = 3) +
              geom_point(data = filter(elk, type == "SWMP Grab"),
                         aes(color = "Grab"),
                         size = 4) +
              scale_colour_manual(name = '', 
                                  values = c('Grab'='deepskyblue3', 
                                           'Diel'='orange', 
                                           'Sonde'='grey70')) +
              scale_x_datetime(date_breaks = "month", date_labels = "%b",
                               limits = lims) +
              scale_y_continuous(expand = c(0,0)) +
              theme_classic() +
              labs(x = "Date",
                   y = "Chlorophyll a (\U00B5g/L)")
  
gtm_plot <- gtm %>% 
              ggplot(aes(x = datetime_collected, y = chla_ugl)) +
              geom_point(data = filter(gtm, type == "Sonde"), 
                         aes(color = "Sonde"), 
                         shape = 1) +
              geom_point(data = filter(gtm, type == "ISCO"),
                         aes(color = "Diel"),
                         size = 3) +
              geom_point(data = filter(gtm, type == "SWMP Grab"),
                         aes(color = "Grab"),
                         size = 4) +
              scale_colour_manual(name = '', 
                                  values = c('Grab'='deepskyblue3', 
                                             'Diel'='orange', 
                                             'Sonde'='grey70')) +
              scale_x_datetime(date_breaks = "month", date_labels = "%b") +
              scale_y_continuous(expand = c(0,0)) +
              theme_classic() +
              labs(x = "Date",
                   y = "Chlorophyll a (\U00B5g/L)")

owc_plot <- owc %>% 
              ggplot(aes(x = datetime_collected, y = chla_ugl)) +
              geom_point(data = filter(owc, type == "Sonde"), 
                         aes(color = "Sonde"), 
                         shape = 1) +
              geom_point(data = filter(owc, type == "ISCO"),
                         aes(color = "Diel"),
                         size = 3) +
              geom_point(data = filter(owc, type == "SWMP Grab"),
                         aes(color = "Grab"),
                         size = 4) +
              scale_colour_manual(name = '', 
                                  values = c('Grab'='deepskyblue3', 
                                             'Diel'='orange', 
                                             'Sonde'='grey70')) +
              scale_x_datetime(date_breaks = "month", date_labels = "%b",
                               limits = lims) +
              scale_y_continuous(expand = c(0,0)) +
              theme_classic() +
  theme(text = element_text(color = "black")) +
              labs(x = "Date",
                   y = "Chlorophyll a (\U00B5g/L)")

(elk_plot + theme(legend.position = "none",
                  axis.text.x = element_blank()) + 
    labs(title = "Elkhorn Slough, WA",
         x = "")) / 
  (gtm_plot + theme(legend.position = "none",
                    axis.text.x = element_blank()) +
     labs(title = "Guana Tolomato Matanzas, FL",
          x = "")) / 
  (owc_plot + theme(legend.position = "bottom") +
     labs(title = "Old Woman Creek, OH")) 


 
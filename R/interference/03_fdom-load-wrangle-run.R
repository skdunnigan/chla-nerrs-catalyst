# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
# source('R/00_load-packages.R')


# 01 load-data ---------------------------------------------------------------

dat <- readxl::read_xlsx(here::here('analysis', 'interference-data', 'fdom_niw.xlsx'), 
                           sheet = 'averaged data') %>%
          janitor::clean_names()  
 
fdom_interf <- dat %>% 
                ggplot(aes(x = avg_fdom_qsu, y = avg_chl_rfu)) +
                  geom_point(position = "jitter") +
                  stat_smooth(method = "lm",
                              se = FALSE,
                              fullrange = T) +
                  # geom_point(aes(y = corrected_avg_chl_rfu), color = "red") +
                  # stat_smooth(aes(y = corrected_avg_chl_rfu),
                  #             method = "lm",
                  #             color = 'red',
                  #             se = FALSE,
                  #             fullrange = T) +
                  ggpubr::stat_regline_equation(label.y = 3.4) +
                  ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                                   label.y = 3.3) + # add R2 and p value
                  theme_bw() +
                  theme(axis.text = element_text(size = 12, color = 'black'),
                        axis.title = element_text(size = 12)) +
                  labs(y = chla_RFU_title,
                       x = 'fDOM (QSU)')
  

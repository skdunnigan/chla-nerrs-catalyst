# 00 load packages ----
# source('R/00_load-packages.R')
# source('R/00_vis_custom.R')

dat <- readxl::read_xlsx(here::here('analysis', 'chla_2.1_mar.xlsx'), 
                         sheet = 'Algae') %>%
       janitor::clean_names() %>% 
       dplyr::rename(type = additional_notes)

dat2 <- dat %>% 
        group_by(sample_no, algae) %>% 
        summarise(mean_rfu = mean(chlorophyll_rfu, na.rm = T),
                  mean_ugl = mean(chla_ug_l, na.rm = T))  
        

algae <- dat %>% 
          select(algae, type) %>% 
          unique()

dat2 <- left_join(dat2, algae, by = "algae")

# ggplotly(dat2 %>%  
#           ggplot(aes(x = mean_rfu, y = mean_ugl)) +
#           geom_point(aes(color = algae), position = 'jitter') +
#            scale_color_discrete(name = "Algae") +
#            theme_classic() +
#            labs(x = "Chlorophyll a (RFU) EXO",
#                 y = "Chlorophyll a (ug/L) Extracted"),
#          tooltip = c("algae", "mean_rfu", "mean_ugl")
#          )
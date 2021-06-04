all %>%
  dplyr::filter(chlorophyll_rfu > 0) %>% 
  ggpubr::ggscatter(x = "chlorophyll_rfu", y = "chla_ugl", color = "reserve_code", shape = "method",
                    add = "reg.line", conf.int = TRUE, # add regression line and confidence interval
                    add.params = list(color = "black", fill = "grey")) + # adjust line and CI colors
  scale_color_brewer(name = "Reserve", type = "qual", palette = "Set1") +
  scale_shape_discrete(name = "Method") +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), label.y = 25) + # add R2 and p value
  stat_regline_equation(label.y = 23) + # add linear equation
  theme_cowplot() +
  theme(legend.position = "bottom") +
  labs(x = chla_RFU_title,
       y = chla_extr_title,
       title = "ISCO and Tank Experiments")

all %>%
  dplyr::filter(chlorophyll_rfu > 0 & reserve_code != "GTM") %>% 
  ggpubr::ggscatter(x = "chlorophyll_rfu", y = "chla_ugl", color = "reserve_code", shape = "method",
                    add = "reg.line", conf.int = TRUE, # add regression line and confidence interval
                    add.params = list(color = "black", fill = "grey")) + # adjust line and CI colors
  scale_color_brewer(name = "Reserve", type = "qual", palette = "Set1") +
  scale_shape_discrete(name = "Method") +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), label.y = 25) + # add R2 and p value
  stat_regline_equation(label.y = 23) + # add linear equation
  theme_cowplot() +
  theme(legend.position = "bottom") +
  labs(x = chla_RFU_title,
       y = chla_extr_title,
       title = "ISCO and Tank Experiments",
       subtitle = "GTM Data Removed")

fdom <- all %>% 
  filter(chlorophyll_rfu > 0) %>% 
  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
  geom_point(aes(color = fdom_qsu), size = 3) +
  scale_color_continuous(name = "fDOM QSU") +
  ggpubr::theme_classic2() +
  labs(x = chla_RFU_title,
       y = chla_extr_title,
       title = "Both Tank and ISCO Experiments")

ggplotly(all %>% 
           filter(chlorophyll_rfu > 0) %>% 
           ggplot(aes(x = chlorophyll_rfu, y = chla_ugl, group = reserve_code)) +
           geom_point(aes(color = fdom_qsu), size = 3) +
           scale_color_continuous(name = "fDOM QSU") +
           ggpubr::theme_classic2() +
           labs(title = "Both Tank and ISCO Experiments"),
         tooltip = c("fdom_qsu", "chlorophyll_rfu", "chla_ugl", "reserve_code"))
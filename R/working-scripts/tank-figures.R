# tank --------------------------------------------------------------------


chla_extr_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L", " Extracted"))
chla_RFU_title <- expression(paste("Chlorophyll ", italic("a "), "RFU EXO"))

tank %>% 
  dplyr::filter(chlorophyll_rfu > 0) %>%
  select(reserve_code, chla_ug_l, chla_rfu, chlorophyll_rfu, chl_fluor) %>% 
  ggplot(aes(x = chlorophyll_rfu, y = chla_ug_l)) +
  geom_point(aes(color = reserve_code))

tank %>%
  dplyr::filter(chlorophyll_rfu > 0) %>%
  ggpubr::ggscatter(x = "chlorophyll_rfu", y = "chla_ug_l", color = "reserve_code",
                    add = "reg.line", conf.int = TRUE, # add regression line and confidence interval
                    add.params = list(color = "black", fill = "grey")) + # adjust line and CI colors
  scale_color_brewer(name = "Reserve", type = "qual", palette = "Set1") +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), label.y = 25) + # add R2 and p value
  stat_regline_equation(label.y = 23) + # add linear equation
  theme_cowplot() +
  theme(legend.position = "bottom") +
  labs(x = chla_RFU_title,
       y = chla_extr_title,
       title = "ISCO Experiments")
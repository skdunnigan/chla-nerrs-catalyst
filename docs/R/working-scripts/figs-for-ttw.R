library(ggpubr)
library(cowplot)

dat <- readxl::read_xlsx(here::here('analysis', '2021-03-08_ttw.xlsx')) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(reserve = toupper(reserve))

chla_extr_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L", " Extracted"))
chla_RFU_title <- expression(paste("Chlorophyll ", italic("a "), "RFU EXO"))

# tank and isco together
dat %>%
  dplyr::filter(chla_rfu_exo > 0) %>%
  ggpubr::ggscatter(x = "chla_rfu_exo", y = "chla_ugl", color = "reserve", shape = "method",
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
       title = "Tank and ISCO Experiments")
ggsave(file = here::here('output', "2021-03-08_iscotank_NERRs_plot_ttw.png"),
       dpi = 120)

dat %>%
  dplyr::filter(chla_rfu_exo > 0) %>%
  ggplot(aes(x = chla_rfu_exo, y = chla_ugl, color = method)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), label.y = 13.8) +
  stat_regline_equation(label.y = 12.3) +
  facet_wrap(~method, scales = "free_x") +
  theme_cowplot() +
  theme(legend.position = "none") +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(x = chla_RFU_title,
       y = chla_extr_title,
       title = "Method Comparison")
ggsave(file = here::here('output', "2021-03-08_iscotank_facet_plot_ttw.png"),
       dpi = 120)


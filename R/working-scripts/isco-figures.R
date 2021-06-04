# isco-wrangle-plot -----------------------------------------------------------------

chla_extr_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L", " Extracted"))
chla_RFU_title <- expression(paste("Chlorophyll ", italic("a "), "RFU EXO"))

isco %>% 
  # filter(reserve_code != "GTM") %>% 
  select(reserve_code, chla_ugl, chla_rfu, chlorophyll_rfu, chl_fluor) %>% 
  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
  geom_point(aes(color = reserve_code))

isco %>%
  ggpubr::ggscatter(x = "chlorophyll_rfu", y = "chla_ugl", color = "reserve_code",
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

isco %>%
  ggpubr::ggscatter(x = "chlorophyll_rfu", y = "chla_ugl", color = "f_domqsu",
                    add = "reg.line", conf.int = TRUE, # add regression line and confidence interval
                    add.params = list(color = "black", fill = "grey")) + # adjust line and CI colors
  scale_color_brewer(name = "FDOM", type = "seq", palette = "YlGnBu") +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), label.y = 25) + # add R2 and p value
  stat_regline_equation(label.y = 23) + # add linear equation
  theme_cowplot() +
  theme(legend.position = "bottom") +
  labs(x = chla_RFU_title,
       y = chla_extr_title,
       title = "ISCO Experiments")


fdom <- isco %>% 
  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
  geom_point(aes(color = f_domqsu, shape = reserve_code), size = 3) +
  scale_color_continuous(name = "fDOM QSU") +
  scale_shape_discrete(name = "Reserve") +
  ggpubr::theme_classic2() +
  labs(x = chla_RFU_title,
       y = chla_extr_title,
       title = "ISCO Experiments")

ggplotly(fdom)


temp <- isco %>% 
  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
  geom_point(aes(color = temp, shape = reserve_code), size = 3) +
  scale_color_continuous(name = "Temperature C") +
  scale_shape_discrete(name = "Reserve") +
  labs(x = chla_RFU_title,
       y = chla_extr_title,
       
       ggplotly(temp)
       
       sal <- isco %>% 
         ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
         geom_point(aes(color = sal, shape = reserve_code), size = 3) +
         scale_color_continuous(name = "Salinity psu") +
         scale_shape_discrete(name = "Reserve")
       
       ggplotly(sal)
       
       # remove GTM data
       isco %>%
         filter(reserve_code != "GTM") %>% 
         ggpubr::ggscatter(x = "chlorophyll_rfu", y = "chla_ugl", color = "reserve_code",
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
              title = "ISCO Experiments",
              subtitle = "GTM data removed")
       
       # just GTM
       isco %>% 
         dplyr::filter(reserve_code == "GTM") %>% 
         ggplot(aes(x = chlorophyll_rfu, y = chla_ugl, color = factor(isco_deployment_no))) +
         geom_point(size = 3)
       
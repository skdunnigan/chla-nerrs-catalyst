# all ----------------------------------------------------

all_figure <-  all %>% 
                dplyr::filter(qaqc == 0) %>%
                ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                  geom_point(position = "jitter") +
                  stat_smooth(method = "lm", color = "black", se = FALSE) +
                  ggpubr::stat_regline_equation(label.y = 37, label.x = 7.5) +
                  ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                                   label.y = 40, label.x = 7.5) + # add R2 and p value
                  scale_y_continuous(expand = c(0,0)) +
                  theme_classic() +
                  theme(legend.title = element_text(size = 14, face = "bold"),
                        text = element_text(size = 12)) +
                  labs(x = chla_RFU_title,
                       y = chla_extr_title,
                       caption = "ISCO and Tank experiments")

# all tank by reserve ---------------------------------------------------

all_reserve_figure <- all %>% 
                        dplyr::filter(qaqc == 0) %>% 
                        ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                          geom_point(aes(color = reserve_code), position = "jitter") +
                          stat_smooth(method = "lm", color = "black", se = FALSE) +
                          ggpubr::stat_regline_equation(label.y = 37, label.x = 7.5) +
                          ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                                           label.y = 40, label.x = 7.5) + # add R2 and p value
                          scale_colour_manual(name = "Reserve", values = reservecolours) +
                          scale_y_continuous(expand = c(0,0)) +
                          theme_classic() +
                          theme(legend.title = element_text(size = 14, face = "bold"),
                                text = element_text(size = 12)) +
                          labs(x = chla_RFU_title,
                               y = chla_extr_title,
                               caption = "ISCO and tank experiments")

all_reserve_method_figure <- all %>% 
                              dplyr::filter(qaqc == 0) %>% 
                              ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                                geom_point(aes(color = reserve_code, shape = method), position = "jitter") +
                                stat_smooth(method = "lm", color = "black", se = FALSE) +
                                ggpubr::stat_regline_equation(label.y = 37, label.x = 7.5) +
                                ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                                                 label.y = 40, label.x = 7.5) + # add R2 and p value
                                scale_colour_manual(name = "Reserve", values = reservecolours) +
                                scale_shape_discrete(name = "Method") +
                                scale_y_continuous(expand = c(0,0)) +
                                theme_classic() +
                                theme(legend.title = element_text(size = 14, face = "bold"),
                                      text = element_text(size = 12)) +
                                labs(x = chla_RFU_title,
                                     y = chla_extr_title,
                                     caption = "ISCO and tank experiments")

# ggplotly(
#   all %>% 
#     dplyr::filter(qaqc == 0) %>% 
#     ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
#     geom_point(aes(color = reserve_code, shape = method), position = "jitter") +
#     stat_smooth(method = "lm", color = "black", se = FALSE) +
#     scale_colour_manual(name = "Reserve", values = reservecolours) +
#     scale_shape_discrete(name = "Method") +
#     scale_y_continuous(expand = c(0,0)) +
#     theme_classic() +
#     labs(x = "Chlorophyll a (RFU) EXO",
#          y = "Chlorophyll a (ug/L) Extracted",
#          caption = "ISCO and tank experiments"),
#   tooltip = c("method", "chlorophyll_rfu", "chla_ugl", "reserve_code")
#   )


# tank and isco facet ----------------------------------------------
tank_isco <- all %>% 
              dplyr::filter(qaqc == 0) %>%
              ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
              geom_point(position = "jitter", alpha = 0.8) +
              stat_smooth(method = "lm", color = "black", se = FALSE) +
              ggpubr::stat_regline_equation(label.y = 12, label.x = 7.5) +
              ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                               label.y = 9, label.x = 7.5) + # add R2 and p value
              facet_grid(~method) +
              scale_y_continuous(expand = c(0,0)) +
              theme_classic() +
              theme(legend.title = element_text(size = 14, face = "bold"),
                    text = element_text(size = 12)) +
              labs(x = chla_RFU_title,
                   y = chla_extr_title)

# tank-reserve-only figure ----------------------------------------------

all_reserve_figure_fxn <- function(x, r2_label.y, regline_label.y, label.x) {
  all %>% 
    dplyr::filter(qaqc == 0 & reserve_code == x) %>% 
    ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
    geom_point(aes(color = reserve_code, shape = method), position = "jitter") +
    stat_smooth(method = "lm", color = "black", se = FALSE) +
    ggpubr::stat_regline_equation(label.y = regline_label.y, label.x = label.x) +
    ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                     label.y = r2_label.y, label.x = label.x) + # add R2 and p value
    scale_colour_manual(name = "Reserve", values = reservecolours) +
    scale_shape_discrete(name = "Method") +
    theme_classic() +
    theme(legend.position = "blank",
          text = element_text(size = 12)) +
    labs(x = chla_RFU_title,
         y = chla_extr_title,
         caption = paste("ISCO and tank experiments at", x))
}

# check all participating reserves
# unique(all$reserve_code)
# 
# all_reserve_figure_fxn("GTM", label.x = 0,
#                    r2_label.y = 20,
#                    regline_label.y = 18)
# all_reserve_figure_fxn("PDB", label.x = 0,
#                    r2_label.y = 20,
#                    regline_label.y = 18)



# interference ------------------------------------------------------------


fdom <- all %>% 
  filter(chlorophyll_rfu > 0 & !is.na(fdom_qsu)) %>% 
  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
  geom_point(aes(color = fdom_qsu), size = 3) +
  scale_color_continuous(name = "fDOM QSU") +
  ggpubr::theme_classic2() +
  labs(x = chla_RFU_title,
       y = chla_extr_title,
       title = "fDOM",
       caption = "Both Tank and ISCO Experiments")

# ggplotly(all %>%
#            filter(chlorophyll_rfu > 0) %>%
#            ggplot(aes(x = chlorophyll_rfu, y = chla_ugl, group = reserve_code)) +
#            geom_point(aes(color = fdom_qsu), size = 3) +
#            scale_color_continuous(name = "fDOM QSU") +
#            ggpubr::theme_classic2() +
#            labs(x = 'Chlorophyll a RFU EXO',
#                 y = 'Chlorophyll a ug/L Extracted',
#                 title = "fDOM",
#                 caption = "Both Tank and ISCO Experiments"),
#          tooltip = c("fdom_qsu", "chlorophyll_rfu", "chla_ugl", "reserve_code"))

turb <- all %>% 
          filter(chlorophyll_rfu > 0 & turb < 240) %>% 
          ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
          geom_point(aes(color = turb), size = 3) +
          scale_color_continuous(name = "Turbidity NTU") +
          ggpubr::theme_classic2() +
          labs(x = chla_RFU_title,
               y = chla_extr_title,
               title = "Turbidity",
               caption = "Both Tank and ISCO Experiments")

# ggplotly(all %>% 
#            filter(chlorophyll_rfu > 0 & turb < 240) %>% 
#            ggplot(aes(x = chlorophyll_rfu, y = chla_ugl, group = reserve_code)) +
#            geom_point(aes(color = turb), size = 3) +
#            scale_color_continuous(name = "Turbidity NTU")  +
#            ggpubr::theme_classic2() +
#            labs(x = 'Chlorophyll a RFU EXO',
#                 y = 'Chlorophyll a ug/L Extracted',
#                 title = "Turbidity", caption = "Both Tank and ISCO Experiments"),
#          tooltip = c("turb", "chlorophyll_rfu", "chla_ugl", "reserve_code"))

temp <- all %>% 
  filter(chlorophyll_rfu > 0) %>% 
  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
  geom_point(aes(color = temp), size = 3) +
  scale_color_gradient(name = expression(paste('Temperature ', "(", degree, "C)")),
                       low="blue", high="red") +
  # scale_color_continuous(name = expression(paste('Temperature ', "(", degree, "C)"))) +
  ggpubr::theme_classic2() +
  labs(x = chla_RFU_title,
       y = chla_extr_title,
       title = "Temperature",
       caption = "Both Tank and ISCO Experiments")

# ggplotly(all %>% 
#            filter(chlorophyll_rfu > 0) %>% 
#            ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
#            geom_point(aes(color = temp), size = 3) +
#            scale_color_gradient(name = 'Temperature C',
#                                 low="blue", high="red") +
#            # scale_color_continuous(name = expression(paste('Temperature ', "(", degree, "C)"))) +
#            ggpubr::theme_classic2() +
#            labs(x = 'Chlorophyll a RFU EXO',
#                 y = 'Chlorophyll a ug/L Extracted',
#                 title = "Temperature", caption = "Both Tank and ISCO Experiments"),
#          tooltip = c("temp", "chlorophyll_rfu", "chla_ugl", "reserve_code"))
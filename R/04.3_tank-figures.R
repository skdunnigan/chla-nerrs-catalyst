# all tank ----------------------------------------------------

all_tank_figure <- tank %>% 
                    dplyr::filter(qaqc == 0 & rep == 1) %>%
                    ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                      geom_point(size = 3, position = "jitter") +
                      stat_smooth(method = "lm", color = "black", se = FALSE) +
                      ggpubr::stat_regline_equation(label.y = 23) +
                      ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                                       label.y = 25) + # add R2 and p value
                      scale_y_continuous(expand = c(0,0)) +
                      theme_classic() +
                      theme(legend.title = element_text(size = 14, face = "bold"),
                            text = element_text(size = 12)) +
                      labs(x = chla_RFU_title,
                           y = chla_extr_title,
                           caption = "Only tank experiments")

# all tank by reserve ---------------------------------------------------

all_tank_reserve_figure <- tank %>% 
                            dplyr::filter(qaqc == 0 & rep == 1) %>% 
                            ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                              geom_point(aes(color = reserve_code), size = 3, position = "jitter") +
                              stat_smooth(method = "lm", color = "black", se = FALSE) +
                              ggpubr::stat_regline_equation(label.y = 23) +
                              ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                                       label.y = 25) + # add R2 and p value
                              scale_colour_manual(name = "Reserve", values = reservecolours) +
                              scale_y_continuous(expand = c(0,0)) +
                              theme_classic() +
                              theme(legend.title = element_text(size = 14, face = "bold"),
                                    text = element_text(size = 12)) +
                              labs(x = chla_RFU_title,
                                   y = chla_extr_title,
                                   caption = "Only tank experiments")

# tank-reserve-only figure ----------------------------------------------

tank_reserve_figure <- function(x, r2_label, regline_label) {
  tank %>% 
    dplyr::filter(qaqc == 0 & reserve_code == x & rep == 1) %>% 
    ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
    geom_point(aes(color = reserve_code), size = 3, position = "jitter") +
    stat_smooth(method = "lm", color = "black", se = FALSE) +
    ggpubr::stat_regline_equation(label.y = regline_label) +
    ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                     label.y = r2_label) + # add R2 and p value
    scale_colour_manual(name = "Reserve", values = reservecolours) +
    theme_classic() +
    theme(legend.position = "blank",
          text = element_text(size = 12)) +
    labs(x = chla_RFU_title,
         y = chla_extr_title,
         caption = paste("Only tank experiments at", x))
}

# check all participating reserves
# unique(tank$reserve_code)
# 
# tank_reserve_figure("GTM", regline_label = 18, r2_label = 19)
# tank_reserve_figure("HEE", regline_label = 0.88, r2_label = 0.9)
# tank_reserve_figure("MAR", regline_label = 9, r2_label = 10)
# tank_reserve_figure("PDB", regline_label = 8, r2_label = 9)
# tank_reserve_figure("SAP", regline_label = 23, r2_label = 25)

# facet-figure-all-tank --------------------------------------------------------

facet_all_tank_figure_stat <- tank %>% 
                          dplyr::filter(qaqc == 0 & rep == 1) %>%  
                          ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                            geom_point(aes(color = reserve_code), position = "jitter") +
                            stat_smooth(method = "lm", color = "black", se = FALSE) +
                            ggpubr::stat_regline_equation(label.y = 20) +
                            ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                                             label.y = 18) + # add R2 and p value
                            scale_colour_manual(name = "Reserve", values = reservecolours) +
                            scale_y_continuous(expand = c(0,0)) +
                            facet_wrap(.~ reserve_code, scales = "free_x") +
                            theme_classic() +
                            theme(legend.position = "blank",
                                  text = element_text(size = 12),
                                  strip.background = element_blank(),
                                  strip.text = element_text(size = 12, face = "bold"),) +
                            labs(x = chla_RFU_title,
                                 y = chla_extr_title,
                                 caption = "Only tank experiments")

facet_all_tank_figure <- tank %>% 
                          dplyr::filter(qaqc == 0 & rep == 1) %>%  
                          ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                          geom_point(aes(color = reserve_code), position = "jitter") +
                          stat_smooth(method = "lm", color = "black", se = FALSE) +
                          scale_colour_manual(name = "Reserve", values = reservecolours) +
                          scale_y_continuous(expand = c(0,0)) +
                          facet_wrap(.~ reserve_code, scales = "free") +
                          theme_classic() +
                          theme(legend.position = "blank",
                                text = element_text(size = 12),
                                strip.background = element_blank(),
                                strip.text = element_text(size = 12, face = "bold"),) +
                          labs(x = chla_RFU_title,
                               y = chla_extr_title,
                               caption = "Only tank experiments")

# all tank, fDOM, turb, temp ----------------------------------------------------------

all_tank_fDOM <- tank %>% 
                  dplyr::filter(qaqc == 0 & rep == 1) %>%
                  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                    geom_point(aes(color = fdom_qsu), size = 3, position = "jitter") +
                    scale_colour_continuous(name = "fDOM QSU") +
                    scale_y_continuous(expand = c(0,0)) +
                    theme_classic() +
                    theme(legend.title = element_text(size = 14, face = "bold"),
                          text = element_text(size = 12)) +
                    labs(x = chla_RFU_title,
                         y = chla_extr_title,
                         caption = "Only tank experiments with fDOM (QSU)")

all_tank_turb <- tank %>% 
                  dplyr::filter(qaqc == 0 & rep == 1 & !is.na(turb)) %>%
                  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                    geom_point(aes(color = turb), size = 3, position = "jitter") +
                    scale_colour_continuous(name = "Turbidity (NTU)") +
                    scale_y_continuous(expand = c(0,0)) +
                    theme_classic() +
                    theme(legend.title = element_text(size = 14, face = "bold"),
                          text = element_text(size = 12)) +
                    labs(x = chla_RFU_title,
                         y = chla_extr_title,
                         caption = "Only tank experiments with turbidity (NTU)")

all_tank_temp <- tank %>% 
                  dplyr::filter(qaqc == 0 & rep == 1) %>%
                  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                    geom_point(aes(color = temp), size = 3, position = "jitter") +
                    scale_colour_continuous(name = expression(paste('Temperature ', "(", degree, "C)"))) +
                    scale_y_continuous(expand = c(0,0)) +
                    theme_classic() +
                    theme(legend.title = element_text(size = 14, face = "bold"),
                          text = element_text(size = 12)) +
                    labs(x = chla_RFU_title,
                         y = chla_extr_title,
                         caption = expression(paste('Only tank experiments with temperature ', "(", degree, "C)")))


# all isco ----------------------------------------------------------------
all_isco_figure <- isco %>% 
                    dplyr::filter(qaqc == 0) %>%
                    ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                    geom_point(size = 3, position = "jitter") +
                    stat_smooth(method = "lm", color = "black", se = FALSE) +
                    ggpubr::stat_regline_equation(label.y = 37) +
                    ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                                     label.y = 40) + # add R2 and p value
                    scale_y_continuous(expand = c(0,0)) +
                    theme_classic() +
                    theme(legend.title = element_text(size = 14, face = "bold"),
                          text = element_text(size = 12)) +
                    labs(x = chla_RFU_title,
                         y = chla_extr_title,
                         caption = "Only ISCO experiments")

# all isco by reserve -----------------------------------------------------
all_isco_reserve_figure <- isco %>% 
                            dplyr::filter(qaqc == 0) %>% 
                            ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                            geom_point(aes(color = reserve_code), size = 3, position = "jitter") +
                            stat_smooth(method = "lm", color = "black", se = FALSE) +
                            ggpubr::stat_regline_equation(label.y = 40) +
                            ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                                             label.y = 43) + # add R2 and p value
                            scale_colour_manual(name = "Reserve", values = reservecolours) +
                            scale_y_continuous(expand = c(0,0)) +
                            theme_classic() +
                            theme(legend.title = element_text(size = 14, face = "bold"),
                                  text = element_text(size = 12)) +
                            labs(x = chla_RFU_title,
                                 y = chla_extr_title,
                                 caption = "Only ISCO experiments")

# ISCO-reserve-only figure ----------------------------------------------

isco_reserve_figure <- function(x, r2_label, regline_label) {
  isco %>% 
    dplyr::filter(qaqc == 0 & reserve_code == x) %>% 
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
         caption = paste("Only ISCO experiments at", x))
}

# check all participating reserves
# unique(isco$reserve_code)
# 
# isco_reserve_figure("ELK", r2_label = 12, regline_label = 11)
# isco_reserve_figure("GTM", r2_label = 11, regline_label = 10)
# isco_reserve_figure("PDB", r2_label = 15, regline_label = 13)
# isco_reserve_figure("WEL", r2_label = 4, regline_label = 3.8)
# isco_reserve_figure("OWC", r2_label = 43, regline_label = 40)



# facet-figure-all-tank --------------------------------------------------------

facet_all_isco_figure_stat <- isco %>% 
                          dplyr::filter(qaqc == 0) %>%  
                          ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                            geom_point(aes(color = reserve_code), position = "jitter") +
                            stat_smooth(method = "lm", color = "black", se = FALSE) +
                            ggpubr::stat_regline_equation(label.y = 38) +
                            ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")),
                                             label.y = 43) + # add R2 and p value
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
                                 caption = "Only ISCO experiments")

facet_all_isco_figure <- isco %>% 
                          dplyr::filter(qaqc == 0) %>%  
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
                               caption = "Only ISCO experiments")

# all isco, fDOM, turb, temp ----------------------------------------------------------

all_isco_fDOM <- isco %>% 
                  dplyr::filter(qaqc == 0 & !is.na(fdom_qsu)) %>%
                  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                    geom_point(aes(color = fdom_qsu), size = 3, position = "jitter") +
                    scale_colour_continuous(name = "fDOM QSU") +
                    theme_classic() +
                    theme(legend.title = element_text(size = 14, face = "bold"),
                          text = element_text(size = 12)) +
                    labs(x = chla_RFU_title,
                         y = chla_extr_title,
                         caption = "Only ISCO experiments with fDOM (QSU)")

all_isco_turb <- isco %>% 
                  dplyr::filter(qaqc == 0 & turb < 240) %>%
                  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                    geom_point(aes(color = turb), size = 3, position = "jitter") +
                    scale_colour_continuous(name = "Turbidity (NTU)") +
                    scale_y_continuous(expand = c(0,0)) +
                    theme_classic() +
                    theme(legend.title = element_text(size = 14, face = "bold"),
                          text = element_text(size = 12)) +
                    labs(x = chla_RFU_title,
                         y = chla_extr_title,
                         caption = "Only ISCO experiments with turbidity (NTU)")

all_isco_temp <- isco %>% 
                  dplyr::filter(qaqc == 0) %>%
                  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                    geom_point(aes(color = temp), size = 3, position = "jitter") +
                    scale_colour_continuous(name = expression(paste('Temperature ', "(", degree, "C)"))) +
                    theme_classic() +
                    theme(legend.title = element_text(size = 14, face = "bold"),
                          text = element_text(size = 12)) +
                    labs(x = chla_RFU_title,
                         y = chla_extr_title,
                         caption = expression(paste('Only ISCO experiments with temperature ', "(", degree, "C)")))

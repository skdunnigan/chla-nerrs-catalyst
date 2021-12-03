# all ----------------------------------------------------

all_figure <-  all %>% 
                ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                  geom_point(position = "jitter") +
                  stat_smooth(method = "lm", color = "black", se = FALSE) +
                  ggpubr::stat_regline_equation(label.y = 11, label.x = 15) +
                  ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                                   label.y = 15, label.x = 15) + # add R2 and p value
                  scale_y_continuous(expand = c(0,0), limits = c(0,65)) +
                  scale_x_continuous(expand = c(0,0)) +
                  theme_classic() +
                  theme(legend.title = element_text(size = 14, face = "bold"),
                        text = element_text(size = 12)) +
                  labs(x = chla_RFU_title,
                       y = chla_extr_title,
                       caption = "ISCO and Tank experiments")

# all tank by reserve ---------------------------------------------------

all_reserve_figure <- all %>% 
                        ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                          geom_point(aes(color = reserve_code), position = "jitter") +
                          stat_smooth(method = "lm", color = "black", se = FALSE) +
                          ggpubr::stat_regline_equation(label.y = 11, label.x = 15) +
                          ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                                           label.y = 15, label.x = 15) + # add R2 and p value
                          scale_colour_manual(name = "Reserve", values = reservecolours) +
                          scale_y_continuous(expand = c(0,0), limits = c(0,65)) +
                          scale_x_continuous(expand = c(0,0)) +
                          theme_classic() +
                          theme(legend.title = element_text(size = 14, face = "bold"),
                                plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
                                text = element_text(size = 12)) +
                          labs(x = chla_RFU_title,
                               y = chla_extr_title,
                               caption = "ISCO and tank experiments")

all_reserve_method_figure <- all %>% 
                              ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                                geom_point(aes(color = reserve_code, shape = method), position = "jitter") +
                                stat_smooth(method = "lm", color = "black", se = FALSE) +
                                ggpubr::stat_regline_equation(label.y = 11, label.x = 15) +
                                ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                                                 label.y = 15, label.x = 15) + # add R2 and p value
                                scale_colour_manual(name = "Reserve", values = reservecolours) +
                                scale_shape_discrete(name = "Method") +
                                scale_y_continuous(expand = c(0,0), limits = c(0,65)) +
                                scale_x_continuous(expand = c(0,0)) +
                                theme_classic() +
                                theme(legend.title = element_text(size = 14, face = "bold"),
                                      plot.margin = margin(t = 50, r = 10, b = 10, l = 10, unit = "pt"),
                                      text = element_text(size = 12)) +
                                labs(x = chla_RFU_title,
                                     y = chla_extr_title,
                                     caption = "ISCO and tank experiments")

# tank and isco facet ----------------------------------------------
tank_isco <- all %>% 
              ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
              geom_point(position = "jitter", alpha = 0.8) +
              stat_smooth(method = "lm", color = "black", se = FALSE) +
              ggpubr::stat_regline_equation(label.y = 12, label.x = 7.5) +
              ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), 
                               label.y = 9, label.x = 7.5) + # add R2 and p value
              facet_grid(~method) +
              scale_y_continuous(expand = c(0,0), limits = c(0,65)) +
              theme_classic() +
              theme(legend.title = element_text(size = 14, face = "bold"),
                    text = element_text(size = 12)) +
              labs(x = chla_RFU_title,
                   y = chla_extr_title)

# tank-reserve-only figure ----------------------------------------------

all_reserve_figure_fxn <- function(x, r2_label.y, regline_label.y, label.x) {
  all %>% 
    dplyr::filter(reserve_code == x) %>% 
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

interference_interact_all_fxn <- function(param, interact){
  
  if (interact == TRUE) {
      m = list(
        l = 80,
        r = 150,
        b = 80,
        t = 50,
        pad = 0
      )
      
      if (param > 1){
        # fDOM
        fDOM <- ggplotly(all %>%
                           ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                           geom_point(aes(color = fdom_qsu, group = reserve_code), position = "jitter") +
                           stat_smooth(method = "lm", color = "black", se = FALSE) +
                           scale_color_continuous(name = "fDOM QSU") +
                           theme_classic() +
                           labs(x = 'Chlorophyll a RFU EXO',
                                y = 'Chlorophyll a ug/L Extracted',
                                title = "fDOM",
                                caption = "Both Tank and ISCO Experiments"),
                         tooltip = c("fdom_qsu", "chlorophyll_rfu", "chla_ugl", "reserve_code"))
        fDOM %>% layout(margin = m)
      }
      else if (param == 1) {
        # temperature
        temp <- ggplotly(all %>%
                           filter(method == "isco") %>%
                           ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                           geom_point(aes(color = temp, group = reserve_code), position = "jitter") +
                           scale_color_gradient(name = 'Temperature C',
                                                low="blue", high="red") +
                           stat_smooth(method = "lm", color = "black", se = FALSE) +
                           theme_classic() +
                           labs(x = 'Chlorophyll a RFU EXO',
                                y = 'Chlorophyll a ug/L Extracted',
                                title = "Temperature"),
                         tooltip = c("temp", "chlorophyll_rfu", "chla_ugl", "reserve_code"))
        temp %>% layout(margin = m)
      } else {
        turb <- ggplotly(all %>%
                           filter(!is.na(turb)) %>%
                           ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                           geom_point(aes(color = turb, group = reserve_code), position = "jitter") +
                           scale_color_continuous(name = "Turbidity NTU")  +
                           stat_smooth(method = "lm", color = "black", se = FALSE) +
                           theme_classic() +
                           labs(x = 'Chlorophyll a RFU EXO',
                                y = 'Chlorophyll a ug/L Extracted',
                                title = "Turbidity", caption = "Both Tank and ISCO Experiments"),
                         tooltip = c("turb", "chlorophyll_rfu", "chla_ugl", "reserve_code"))
        turb %>% layout(margin = m)
      }
      
  } 
  else {
    if (param > 1){
      # fDOM
      fDOM <- all %>% 
        filter(!is.na(fdom_qsu)) %>% 
        ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
        geom_point(aes(color = fdom_qsu), position = "jitter") +
        stat_smooth(method = "lm", color = "black", se = FALSE) +
        scale_color_continuous(name = "fDOM QSU") +
        theme_classic() +
        labs(x = chla_RFU_title,
             y = chla_extr_title,
             title = "fDOM",
             caption = "Both Tank and ISCO Experiments")
      fDOM 
    }
    else if (param == 1) {
      # temperature
      temp <- all %>% 
        ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
        geom_point(aes(color = temp), position = "jitter") +
        stat_smooth(method = "lm", color = "black", se = FALSE) +
        scale_color_gradient(name = expression(paste('Temperature ', "(", degree, "C)")),
                             low="blue", high="red") +
        theme_classic() +
        labs(x = chla_RFU_title,
             y = chla_extr_title,
             title = "Temperature",
             caption = "Both Tank and ISCO Experiments")
      temp
    } else {
      turb <- all %>% 
        filter(!is.na(turb)) %>% 
        ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
        geom_point(aes(color = turb), position = "jitter") +
        stat_smooth(method = "lm", color = "black", se = FALSE) +
        scale_color_continuous(name = "Turbidity NTU") +
        theme_classic() +
        labs(x = chla_RFU_title,
             y = chla_extr_title,
             title = "Turbidity",
             caption = "Both Tank and ISCO Experiments")
      turb
    }
    
  }
}



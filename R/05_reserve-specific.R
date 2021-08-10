# reserve specifics

# black and white all points with stat line
reserve_all_fxn <- function(site) {
  all %>% 
    dplyr::filter(qaqc == 0 & reserve_code == site) %>% 
    ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
    geom_point(aes(shape = method), position = "jitter") +
    stat_smooth(method = "lm", color = "black", se = FALSE) +
    scale_shape_discrete(name = "Method") +
    theme_classic() +
    theme(text = element_text(size = 12)) +
    labs(x = chla_RFU_title,
         y = chla_extr_title)
}

reserve_all_facet_fxn <- function(site) {
  all %>% 
    dplyr::filter(qaqc == 0 & reserve_code == site) %>% 
    ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
    geom_point(aes(shape = method), position = "jitter") +
    facet_grid(~method) +
    stat_smooth(method = "lm", color = "black", se = FALSE) +
    scale_shape_discrete(name = "Method") +
    theme_classic() +
    theme(legend.position = "none",
          text = element_text(size = 12)) +
    labs(x = chla_RFU_title,
         y = chla_extr_title)
}
# reserve_all_facet_fxn("GTM")
# and you can add the equations to the figures
# you'll need to adjust the labels
# reserve_all_facet_fxn("GTM") + ggpubr::stat_regline_equation() +
#   ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")))

# interactive graph for isco deployments
interact_reserve_isco_fxn <- function(site) {
  m = list(
    l = 80,
    r = 150,
    b = 80,
    t = 50,
    pad = 0
  )
  
a <- ggplotly(all %>% 
  dplyr::filter(qaqc == 0 & reserve_code == site & method == "isco") %>% 
  ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
  geom_point(aes(color = isco_deployment_no), position = "jitter") +
  stat_smooth(method = "lm", color = "black", se = FALSE) +
  scale_colour_discrete(name = "ISCO \n Deployment") +
  theme_classic() +
  theme(text = element_text(size = 12)) +
  labs(x = "Chlorophyll (RFU) EXO",
       y = "Chlorophyll (ug/L) Extracted"),
  tooltip = c('isco_deployment_no', 'chlorophyll_rfu', 'chla_ugl')
)

a %>% layout(margin = m)
}

# interact_reserve_isco_fxn("WEL")

# interactive graph for tank samples
interact_reserve_tank_fxn <- function(site) {
  m = list(
    l = 80,
    r = 150,
    b = 80,
    t = 50,
    pad = 0
  )
  
  
 a <-  ggplotly(all %>%
              dplyr::mutate(date_collected = factor(date_collected)) %>% 
              dplyr::filter(qaqc == 0 & reserve_code == site & method == "tank") %>% 
              ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
              geom_point(aes(color = date_collected), position = "jitter") +
              stat_smooth(method = "lm", color = "black", se = FALSE) +
              scale_colour_discrete(name = "Date Collected") +
              theme_classic() +
              theme(text = element_text(size = 12)) +
              labs(x = "Chlorophyll (RFU) EXO",
                   y = "Chlorophyll (ug/L) Extracted"),
            tooltip = c('date_collected', 'chlorophyll_rfu', 'chla_ugl')
  )
 a %>% layout(margin = m)
}

# interact_reserve_tank_fxn("GTM")


# interactive interference figures ----------------------------------------

# param 0 = turb, 1 = temp, 2 = fdom
interact_reserve_interf_fxn <- function(site, param) {
  m = list(
    l = 80,
    r = 150,
    b = 80,
    t = 50,
    pad = 0
  )
  
  
  if (param > 1) {
  
  fDOM <-  ggplotly(all %>%
                   dplyr::filter(qaqc == 0 & reserve_code == site & chlorophyll_rfu > 0) %>%
                   ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                   geom_point(aes(color = fdom_qsu), position = "jitter") +
                   stat_smooth(method = "lm", color = "black", se = FALSE) +
                   scale_colour_continuous(name = "fDOM QSU") +
                   theme_classic() +
                   labs(x = "Chlorophyll (RFU) EXO",
                        y = "Chlorophyll (ug/L) Extracted",
                        title = "fDOM"),
                 tooltip = c('fdom_qsu', 'chlorophyll_rfu', 'chla_ugl')
  )
  fDOM %>% layout(margin = m)
  }
  else if (param == 1) {
    temp <-  ggplotly(all %>%
                        dplyr::filter(qaqc == 0 & reserve_code == site & chlorophyll_rfu > 0) %>%
                        ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                        geom_point(aes(color = temp), position = "jitter") +
                        stat_smooth(method = "lm", color = "black", se = FALSE) +
                        scale_color_gradient(name = "Temperature (C)",
                                             low="blue", high="red") +
                        theme_classic() +
                        labs(x = "Chlorophyll (RFU) EXO",
                             y = "Chlorophyll (ug/L) Extracted",
                             title = "Temperature"),
                      tooltip = c('temp', 'chlorophyll_rfu', 'chla_ugl')
    )
    temp %>% layout(margin = m)
  } else {
    turb <-  ggplotly(all %>%
                        dplyr::filter(qaqc == 0 & reserve_code == site & chlorophyll_rfu > 0) %>%
                        ggplot(aes(x = chlorophyll_rfu, y = chla_ugl)) +
                        geom_point(aes(color = turb), position = "jitter") +
                        stat_smooth(method = "lm", color = "black", se = FALSE) +
                        scale_colour_continuous(name = "Turbidity FNU") +
                        theme_classic() +
                        labs(x = "Chlorophyll (RFU) EXO",
                             y = "Chlorophyll (ug/L) Extracted",
                             title = "Turbidity"),
                      tooltip = c('turb', 'chlorophyll_rfu', 'chla_ugl')
    )
    turb %>% layout(margin = m)
  }
}

# interact_reserve_interf_fxn("GTM", param = 1)


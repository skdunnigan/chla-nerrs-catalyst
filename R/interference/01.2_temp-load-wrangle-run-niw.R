# great resource for interpreting lm model outputs: https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R


# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
# source('R/00_load-packages.R')


# 01 load-data ---------------------------------------------------------------

dat_niw <- readxl::read_xlsx(here::here('analysis', 'interference-data', 'temp_niw.xlsx'),
                         sheet = "raw") %>%
           janitor::clean_names() %>%
           dplyr::rename(chla_rfu = chlorophyll_rfu) %>%
           dplyr::mutate(conc = factor(conc,
                                       levels = c('100',
                                                  '50',
                                                  '25',
                                                  '0')),
                         conc_perc = paste0(conc, '%'),
                         conc_perc = factor(conc_perc,
                                            levels = c('100%',
                                                       '50%',
                                                       '25%',
                                                       '0%')))

Tr <- 20 # enter reference temperature

# 02 create initial dilution plot ----------------------------------------------------

## load titles
chla_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L"))
chla_RFU_title <- expression(paste("Chlorophyll ", italic(" a "), " RFU"))

niw.plot.a <- dat_niw %>%
                ggplot(mapping = aes(x = temp_c, y = chla_rfu)) +
                geom_point() +
                stat_smooth(method = "lm", aes(color = conc_perc),
                            se = FALSE,
                            fullrange = T) +
                ylim(0,4.1) +
                theme_bw() +
                theme(legend.position = "none",
                      axis.text = element_text(size = 12, color = 'black'),
                      axis.title = element_text(size = 12)) +
                labs(y = chla_RFU_title,
                     x = 'Temperature ('~degree*C*')') 
niw.plot.a
# ggsave(here::here('output', 'plotA.png'), plot = plot.a, dpi = 300)
# 03 assess for slope:intercept constant across all concentrations ---------------------------------------

## linear regression model on all concentrations and clean output with `broom::tidy()`
## additional diagnostics with `broom::glance()` function
lm_out_niw <- dat_niw %>%
                dplyr::group_by(conc) %>%
                do(broom::tidy(lm(chla_rfu ~ temp_c, data = .)))

  diag_niw <- dat_niw %>%
                group_by(conc) %>%
                do(broom::glance(lm(chla_rfu ~ temp_c, data = .)))

##check out model outputs
lm_out_niw
diag_niw

## examine slope:intercept
m_niw <- lm_out_niw %>%
          dplyr::filter(term == 'temp_c') %>%
          dplyr::rename(m = estimate) %>%
          dplyr::select(-term)

b_niw <- lm_out_niw %>%
          dplyr::filter(term == '(Intercept)') %>%
          dplyr::select(conc, estimate) %>%
          dplyr::rename(b = estimate)

## examine slope:intercept ratios for each dilution: are they constant?
ratios_niw <- dplyr::left_join(m_niw, b_niw, by = "conc") %>% 
              dplyr::mutate(conc_perc = paste0(conc, '%'),
                            conc_perc = factor(conc_perc,
                                               levels = c('100%',
                                                          '50%',
                                                          '25%',
                                                          '0%')))

niw.plot.b <- ratios_niw %>%
                ggplot(aes(x = b, y = m)) +
                geom_point(aes(color = conc_perc), size = 3) +
                stat_smooth(method = "lm", se = FALSE, color = "black") +
                scale_x_continuous(position = "top") +
                scale_color_discrete(name = "Concentration") +
                theme_bw() +
                theme(legend.position = "bottom",
                      axis.title = element_text(size = 12),
                      text = element_text(size = 12)) +
                labs(x = "Intercept (b)",
                     y = "Slope (m)") 
niw.plot.b

# ggsave(here::here('output', 'slope-intercept.png'), plot = plot.b, dpi = 120)




## export statistic tables by uncommenting the following lines of code:
# write.csv(lm_out, here::here('output', 'lm_output1_raw.csv'))
# write.csv(diag, here::here('output', 'lm_output2_raw.csv'))
# write.csv(ratios, here::here('output', 'lm-stats-ratios.csv'))

# remove unnecessary df
# rm(m, b, lm_out, diag)

# derive p ----------------------------------------------------------------

combo_niw <- dplyr::left_join(m_niw, b_niw, by = "conc") %>%
             dplyr::mutate(ratio = m/b,
                           rho = (m/((m*Tr) + b))
                          ) %>% 
             dplyr::filter(conc != "0")

rho_niw <- mean(combo_niw$rho, na.rm = T)


# 06 apply adjustment to chla values --------------------------------------

# Equation 1 from Watras et al. 2011:

Fr_niw <- function(x, y){
  y / (1 + rho_niw*(x - Tr))
}


dat_corr_niw <- dat_niw %>%
                  mutate(chla_r = Fr_niw(temp_c, chla_rfu))


# 07 create plot with corrected chlorophyll data --------------------------

niw.plot.c <- dat_corr_niw %>%
                ggplot(mapping = aes(x = temp_c, y = chla_r)) +
                geom_point() +
                stat_smooth(method = "lm", aes(color = conc_perc),
                            se = FALSE,
                            fullrange = T) +
                scale_color_discrete(name = "Concentration") +
                ylim(0,4.1) +
                theme_bw() +
                theme(legend.position = "bottom",
                      axis.text = element_text(size = 12, color = 'black'),
                      axis.title = element_text(size = 12)) +
                labs(y = chla_RFU_title,
                     x = 'Temperature ('~degree*C*')') 
niw.plot.c

# ggsave(here::here('output', 'plotB.png'), plot = plot.c, dpi = 300)


# (niw.plot.a + labs(x = '', title = 'A')) / (niw.plot.c + labs(title = 'B')) +
#   plot_annotation(caption = "Temperature quenching of chlorophyll (A) fluorescence in unfiltered water from Georgetown, South Carolina.\n In panels (B, C) the temperature quench was removed by adjusting the raw data to a reference temperature of 20 ('~*degree*C') using Eq. 1.")


# 08 goodness of fit ------------------------------------------------------




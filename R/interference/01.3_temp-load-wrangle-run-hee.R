# great resource for interpreting lm model outputs: https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R


# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
# source('R/00_load-packages.R')


# 01 load-data ---------------------------------------------------------------

dat_hee <- readxl::read_xlsx(here::here('analysis', 'interference-data', 'temp_hee.xlsx')) %>%
           janitor::clean_names() %>%
          dplyr::rename(chla_rfu = chlorophyll_rfu) %>%
          dplyr::mutate(conc = conc * 100,
                        conc = factor(conc,
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

hee.plot.a <- dat_hee %>%
                ggplot(mapping = aes(x = temp_c, y = chla_rfu)) +
                geom_point() +
                stat_smooth(method = "lm", aes(color = conc_perc),
                            se = FALSE,
                            fullrange = T) +
                # ylim(0,4.1) +
                theme_bw() +
                theme(legend.position = "none",
                      axis.text = element_text(size = 12, color = 'black'),
                      axis.title = element_text(size = 12)) +
                labs(y = chla_RFU_title,
                     x = 'Temperature ('~degree*C*')') 
hee.plot.a
# ggsave(here::here('output', 'plotA.png'), plot = plot.a, dpi = 300)
# 03 assess for slope:intercept constant across all concentrations ---------------------------------------

## linear regression model on all concentrations and clean output with `broom::tidy()`
## additional diagnostics with `broom::glance()` function
lm_out_hee <- dat_hee %>%
                dplyr::group_by(conc) %>%
                do(broom::tidy(lm(chla_rfu ~ temp_c, data = .)))

  diag_hee <- dat_hee %>%
                group_by(conc) %>%
                do(broom::glance(lm(chla_rfu ~ temp_c, data = .)))

##check out model outputs
lm_out_hee
diag_hee

## examine slope:intercept
m_hee <- lm_out_hee %>%
          dplyr::filter(term == 'temp_c') %>%
          dplyr::rename(m = estimate) %>%
          dplyr::select(-term)

b_hee <- lm_out_hee %>%
          dplyr::filter(term == '(Intercept)') %>%
          dplyr::select(conc, estimate) %>%
          dplyr::rename(b = estimate)

## examine slope:intercept ratios for each dilution: are they constant?
ratios_hee <- dplyr::left_join(m_hee, b_hee, by = "conc") %>% 
              dplyr::mutate(conc_perc = paste0(conc, '%'),
                            conc_perc = factor(conc_perc,
                                               levels = c('100%',
                                                          '50%',
                                                          '25%',
                                                          '0%')))

hee.plot.b <- ratios_hee %>%
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
hee.plot.b

# ggsave(here::here('output', 'slope-intercept.png'), plot = plot.b, dpi = 120)




## export statistic tables by uncommenting the following lines of code:
# write.csv(lm_out, here::here('output', 'lm_output1_raw.csv'))
# write.csv(diag, here::here('output', 'lm_output2_raw.csv'))
# write.csv(ratios, here::here('output', 'lm-stats-ratios.csv'))

# remove unnecessary df
# rm(m, b, lm_out, diag)

# derive p ----------------------------------------------------------------

combo_hee <- dplyr::left_join(m_hee, b_hee, by = "conc") %>%
             dplyr::mutate(ratio = m/b,
                           rho = (m/((m*Tr) + b))
                          ) %>% 
             dplyr::filter(conc != "0")

rho_hee <- mean(combo_hee$rho, na.rm = T)


# 06 apply adjustment to chla values --------------------------------------

# Equation 1 from Watras et al. 2011:

Fr_hee <- function(x, y){
  y / (1 + rho_hee*(x - Tr))
}


dat_corr_hee <- dat_hee %>%
                  mutate(chla_r = Fr_hee(temp_c, chla_rfu))


# 07 create plot with corrected chlorophyll data --------------------------

hee.plot.c <- dat_corr_hee %>%
                ggplot(mapping = aes(x = temp_c, y = chla_r)) +
                geom_point() +
                stat_smooth(method = "lm", aes(color = conc_perc),
                            se = FALSE,
                            fullrange = T) +
                scale_color_discrete(name = "Concentration") +
                # ylim(0,4.1) +
                theme_bw() +
                theme(legend.position = "bottom",
                      axis.text = element_text(size = 12, color = 'black'),
                      axis.title = element_text(size = 12)) +
                labs(y = chla_RFU_title,
                     x = 'Temperature ('~degree*C*')') 
hee.plot.c

# ggsave(here::here('output', 'plotB.png'), plot = plot.c, dpi = 300)


# (niw.plot.a + labs(x = '', title = 'A')) / (niw.plot.c + labs(title = 'B')) +
#   plot_annotation(caption = "Temperature quenching of chlorophyll (A) fluorescence in unfiltered water from Georgetown, South Carolina.\n In panels (B, C) the temperature quench was removed by adjusting the raw data to a reference temperature of 20 ('~*degree*C') using Eq. 1.")


# 08 goodness of fit ------------------------------------------------------




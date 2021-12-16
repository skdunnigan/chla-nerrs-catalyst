# great resource for interpreting lm model outputs: https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R


# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
# source('R/00_load-packages.R')


# 01 load-data ---------------------------------------------------------------

dat <- readxl::read_xlsx(here::here('analysis', 'interference-data', 'temp_gtm.xlsx')) %>%
       janitor::clean_names() %>%
       dplyr::rename(chla_rfu = chlorophyll_rfu) %>%
       dplyr::mutate(conc = conc * 100,
                     conc = factor(conc,
                                   levels = c('100',
                                              '50',
                                              '25',
                                              '0')))

Tr <- 20 # enter reference temperature

# 02 create initial dilution plot ----------------------------------------------------

## load titles
chla_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L"))
chla_RFU_title <- expression(paste("Chlorophyll ", italic(" a "), " RFU"))

plot.a <- dat %>%
            ggplot(mapping = aes(x = temp_c, y = chla_rfu)) +
            geom_point() +
            stat_smooth(method = "lm", aes(color = conc),
                        se = FALSE,
                        fullrange = T) +
            xlim(5,33) +
            theme_bw() +
            theme(legend.position = "none",
                  axis.text = element_text(size = 12, color = 'black'),
                  axis.title = element_text(size = 12)) +
            labs(y = chla_RFU_title,
                 x = 'Temperature ('~degree*C*')') +
            annotate("text",
                     x = 33,
                     y = 3.75,
                     size = 4,
                     label = "100 %") +
            annotate("text",
                     x = 33,
                     y = 2.35,
                     size = 4,
                     label = "50 %") +
            annotate("text",
                     x = 33,
                     y = 1.9,
                     size = 4,
                     label = "25 %") +
            annotate("text",
                     x = 33,
                     y = 1.7,
                     size = 4,
                     label = "blank")
plot.a
# ggsave(here::here('output', 'plotA.png'), plot = plot.a, dpi = 300)
# 03 assess for slope:intercept constant across all concentrations ---------------------------------------

## linear regression model on all concentrations and clean output with `broom::tidy()`
## additional diagnostics with `broom::glance()` function
lm_out <- dat %>%
          dplyr::group_by(conc) %>%
          do(broom::tidy(lm(chla_rfu ~ temp_c, data = .)))

  diag <- dat %>%
          group_by(conc) %>%
          do(broom::glance(lm(chla_rfu ~ temp_c, data = .)))

##check out model outputs
lm_out
diag

## examine slope:intercept
m <- lm_out %>%
      dplyr::filter(term == 'temp_c') %>%
      dplyr::rename(m = estimate) %>%
      dplyr::select(-term)

b <- lm_out %>%
      dplyr::filter(term == '(Intercept)') %>%
      dplyr::select(conc, estimate) %>%
      dplyr::rename(b = estimate)

## examine slope:intercept ratios for each dilution: are they constant?
ratios <- dplyr::left_join(m, b, by = "conc") %>%
          dplyr::mutate(ratio = m/b)

## export statistic tables by uncommenting the following lines of code:
# write.csv(lm_out, here::here('output', 'lm_output1_raw.csv'))
# write.csv(diag, here::here('output', 'lm_output2_raw.csv'))
# write.csv(ratios, here::here('output', 'lm-stats-ratios.csv'))

# remove unnecessary df
# rm(m, b, lm_out, diag)

# derive p ----------------------------------------------------------------

plot.b <- ratios %>%
            ggplot(aes(x = b, y = m)) +
            geom_point(aes(color = conc), size = 3) +
            stat_smooth(method = "lm", se = FALSE, color = "black") +
            theme_bw() +
            theme(legend.position = "none",
                  axis.title = element_text(size = 12),
                  text = element_text(size = 12)) +
            labs(x = "Intercept (b)",
                 y = "Slope (m)") +
            annotate("text",
                     x = 2.4,
                     y = -0.0195,
                     label = "blank") +
            annotate("text",
                     x = 3.25,
                     y = -0.0425,
                     label = "25 %") +
            annotate("text",
                     x = 3.8,
                     y = -0.0475,
                     label = "50 %") +
            annotate("text",
                     x = 4.5,
                     y = -0.026,
                     label = "100 %")
plot.b
# ggsave(here::here('output', 'slope-intercept.png'), plot = plot.b, dpi = 120)

## calculate slope for slope ( = y) vs intercept ( = x) of the relationship

p.lm <- ratios %>% lm(m ~ b, data = .)
p.lm.origin <- ratios %>% lm(m ~ 0 + b, data = .)

p.lm.tidy <- broom::tidy(p.lm)

p.lm.origin <- broom::tidy(p.lm.origin)

## get p

p <- p.lm.tidy[[2,2]]
p.origin <- p.lm.origin[[1,2]]


# 06 apply adjustment to chla values --------------------------------------

# Equation 1 from Watras et al. 2011:

Fr <- function(x, y){
  y / (1 + p*(x - Tr))
}

Fr_origin <- function(x, y){
  y / (1 + p.origin*(x - Tr))
}

dat_corr <- dat %>%
            mutate(chla_r = Fr(temp_c, chla_rfu),
                   chla_r_origin = Fr_origin(temp_c, chla_rfu))


# 07 create plot with corrected chlorophyll data --------------------------

plot.c <- dat_corr %>%
            ggplot(mapping = aes(x = temp_c, y = chla_r)) +
            geom_point() +
            stat_smooth(method = "lm", aes(color = conc),
                        se = FALSE,
                        fullrange = T) +
            xlim(5,34) +
            theme_bw() +
            theme(legend.position = "none",
                  axis.text = element_text(size = 12, color = 'black'),
                  axis.title = element_text(size = 12)) +
            labs(y = chla_RFU_title,
                 x = 'Temperature ('~degree*C*')') +
            annotate("text",
                     x = 33,
                     y = 4,
                     size = 4,
                     label = "100 %") +
            annotate("text",
                     x = 33,
                     y = 2.5,
                     size = 4,
                     label = "50 %") +
            annotate("text",
                     x = 33,
                     y = 2.1,
                     size = 4,
                     label = "25 %") +
            annotate("text",
                     x = 33,
                     y = 1.8,
                     size = 4,
                     label = "blank")
plot.c

# ggsave(here::here('output', 'plotB.png'), plot = plot.c, dpi = 300)

plot.d <- dat_corr %>%
            ggplot(mapping = aes(x = temp_c, y = chla_r_origin)) +
            geom_point() +
            stat_smooth(method = "lm", aes(color = conc),
                        se = FALSE,
                        fullrange = T) +
            xlim(5,34) +
            theme_bw() +
            theme(legend.position = "none",
                  axis.text = element_text(size = 12, color = 'black'),
                  axis.title = element_text(size = 12)) +
            labs(y = chla_RFU_title,
                 x = 'Temperature ('~degree*C*')') +
            annotate("text",
                     x = 33,
                     y = 4.25,
                     size = 4,
                     label = "100 %") +
            annotate("text",
                     x = 33,
                     y = 2.65,
                     size = 4,
                     label = "50 %") +
            annotate("text",
                     x = 33,
                     y = 2.2,
                     size = 4,
                     label = "25 %") +
            annotate("text",
                     x = 33,
                     y = 1.9,
                     size = 4,
                     label = "blank")
plot.d

# ggsave(here::here('output', 'plotC.png'), plot = plot.d, dpi = 300)

all <- ((plot.a + labs(title = "A")) + ((plot.c + labs(x = '', y = '', title = "B")) / (plot.d + labs(y = '', title = "C")))) +
  plot_annotation(caption = "Figure 1: Temperature quenching of chlorophyll (A) fluorescence in unfiltered water from Pellicer Creek, Florida.\n In panels (B, C) the temperature quench was removed by adjusting the raw data to a reference temperature of 20 ('~*degree*C') using Eq. 1.")

# ggsave(here::here('output', 'multiplot.png', plot = all, dpi = 300))

plot.f <- (plot.a + labs(x = '', title = 'A')) / (plot.d + labs(title = 'B')) +
  plot_annotation(caption = "Figure 1: Temperature quenching of chlorophyll (A) fluorescence in unfiltered water from Pellicer Creek, Florida.\n In panels (B, C) the temperature quench was removed by adjusting the raw data to a reference temperature of 20 ('~*degree*C') using Eq. 1.")

# ggsave(here::here('output', 'poster-plot.png'), plot = plot.f, dpi = 300)
# 08 goodness of fit ------------------------------------------------------




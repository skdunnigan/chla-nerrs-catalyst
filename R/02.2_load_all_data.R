# load qaqc full data (2021-12-03)

all <- readxl::read_xlsx(here::here('output', '2021_chla-catalyst_data_all.xlsx'),
                         sheet = "qaqc")

isco <- all %>% filter(method == "isco")
tank <- all %>% filter(method == "tank")

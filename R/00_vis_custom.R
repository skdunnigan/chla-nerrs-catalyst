# visual customizations for plots

# create label for chlorophyll plots
chla_extr_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L", " Extracted"))
chla_RFU_title <- expression(paste("Chlorophyll ", italic(" a "), "RFU"))
chla_extr_title2 <- expression(paste("Chlorophyll ", italic(" a "), mu*"g/L"))

# # view vis options for plot
# scales::show_col(hue_pal()(13))
# ggpubr::show_line_types()
# ggpubr::show_point_shapes()

# create color palette for sites
reservecolours <- c(`ELK` = "#F8766D",
                    `GND` = "#E18A00",
                    `GRB` = "#BE9C00",
                    `GTM` = "#8CAB00",
                    `HEE` = "#24B700",
                    `LKS` = "#00BE70",
                    `MAR` = "#00C1AB",
                    `NIW` = "#00BBDA",
                    `OWC` = "#00ACFC",
                    `PDB` = "#8B93FF",
                    `SAP` = "#D575FE",
                    `WEL` = "#F962DD") # removed WKB

# for interactive plot functions
fdom <- 2
temperature <- 1
turbidity <- 0

# for fdom interference standards
standard_colors <- c(`GTM` = "#000000",
                     `LKS` = "#56B4E9",
                     `NIW` = "#E69F00",
                     `OWC` = "#009E73")
                     # `Humic` = "#CC79A7")
standard_shapes <- c(`GTM` = 17,
                     `LKS` = 8,
                     `NIW` = 1,
                     `OWC` = 19)
                     # `Humic` = 7)
standard_linetypes <- c(`GTM` = "solid",
                        `LKS` = "dashed",
                        `NIW` = "dotdash",
                        `OWC` = "dotted")
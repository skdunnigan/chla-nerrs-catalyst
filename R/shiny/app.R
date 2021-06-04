library(shiny) 
library(shinyWidgets) # https://github.com/dreamRs/shinyWidgets
library(bslib)
library(thematic)

source(here::here('R', '2021-05.R'))

# bslib::bs_theme_preview() # use to help create theme

reserve_name <- unique(all$reserve_code)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "yeti"),
  titlePanel("Chlorophyll Catalyst Project Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("reserve", label = "Reserve", choices = reserve_name,
                  multiple = TRUE),
      pickerInput(
        inputId = "myPicker", 
        label = "Select/deselect all + format selected", 
        choices = reserve_name, 
        options = list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = "count > 3"
        ), 
        multiple = TRUE
      )
    ),
    mainPanel(
      plotOutput("plot", click = "plot_click"),
      verbatimTextOutput("info"),
      tableOutput("data")
    )
  )
)

server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  reserve <- reactive({
    get(input$reserve)
  })
  # create reactive expression
  
  output$plot <- renderPlot({
    all %>%
      dplyr::filter(chlorophyll_rfu > 0 & reserve_code == c(reserve())) %>%
      ggpubr::ggscatter(x = "chlorophyll_rfu", y = "chla_ugl", color = "reserve_code", shape = "method",
                        add = "reg.line", conf.int = TRUE, # add regression line and confidence interval
                        add.params = list(color = "black", fill = "grey")) + # adjust line and CI colors
      scale_color_brewer(name = "Reserve", type = "qual", palette = "Set1") +
      scale_shape_discrete(name = "Method") +
      stat_cor(
        aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), label.y = 25) + # add R2 and p value
      stat_regline_equation(label.y = 23) + # add linear equation
      theme_cowplot() +
      theme(legend.position = "bottom") +
      labs(x = chla_RFU_title,
           y = chla_extr_title,
           title = "ISCO and Tank Experiments")
  }, res = 96)
  
  output$info <- renderPrint({
    req(input$plot_click)
    x <- round(input$plot_click$x, 2)
    y <- round(input$plot_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })
  
  output$data <- renderTable({
    nearPoints(mtcars, input$plot_click)
  })
  
}

shinyApp(ui, server)


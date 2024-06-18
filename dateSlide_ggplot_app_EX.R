library(shiny)
library(tidyverse)

data <- faithful %>% mutate(eruptionTime=lubridate::now() + lubridate::dhours(cumsum(waiting)))

ui <- fluidPage(
  sliderInput("slider", label = "Date Range",
              min = min(data$eruptionTime),
              max = max(data$eruptionTime),
              value=c(min(data$eruptionTime),max(data$eruptionTime)),
              timeFormat="%Y"),
  plotOutput("plot")
)

server <- function(input, output) {
  output$plot <- renderPlot({
    data %>% 
      ggplot() +
      geom_point(aes(x=eruptionTime, y=eruptions)) +
      coord_cartesian(xlim = input$slider)
  })
}

shinyApp(ui = ui, server = server)

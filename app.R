library(shiny)

ui <- fluidPage(
  titlePanel("Sepsis Model Test"),
  "테스트 중입니다!"
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

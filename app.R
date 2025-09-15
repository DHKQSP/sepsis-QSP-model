# app.R - 간단한 테스트 버전
library(shiny)
library(rxode2)

# 모델 파일 안전하게 로드
tryCatch({
  source("modelfile_0822.R")
}, error = function(e) {
  print(paste("Error loading modelfile:", e$message))
})

tryCatch({
  load("model_struct.saved")
}, error = function(e) {
  print("model_struct.saved not found, continuing without it")
})

tryCatch({
  source("calcNomParams_human_0602.R")
}, error = function(e) {
  print(paste("Error loading calcNomParams:", e$message))
})

# 간단한 UI
ui <- fluidPage(
  titlePanel("Sepsis QSP Model - Debug Version"),
  
  sidebarLayout(
    sidebarPanel(
      h3("항생제 투여 설정"),
      numericInput("dose", "용량 (mg):", value = 1000, min = 0),
      numericInput("interval", "간격 (h):", value = 12, min = 6),
      actionButton("run", "실행", class = "btn-primary")
    ),
    
    mainPanel(
      h4("상태:"),
      verbatimTextOutput("status"),
      plotOutput("plot")
    )
  )
)

# 서버
server <- function(input, output) {
  
  output$status <- renderPrint({
    "앱이 실행 중입니다. 모델 파일 로드 확인 중..."
  })
  
  observeEvent(input$run, {
    output$status <- renderPrint({
      tryCatch({
        # 모델 초기화 테스트
        if(exists("ode") && exists("mod1")) {
          m1 <- rxode2(model = ode, modname = mod1)
          "모델 초기화 성공!"
        } else {
          "모델 변수가 없습니다. modelfile_0822.R 확인 필요"
        }
      }, error = function(e) {
        paste("에러:", e$message)
      })
    })
    
    output$plot <- renderPlot({
      plot(1:10, main = "테스트 플롯")
    })
  })
}

shinyApp(ui = ui, server = server)

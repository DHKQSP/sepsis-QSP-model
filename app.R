# model_struct.saved 파일 생성 (없을 경우)
if(!file.exists("model_struct.saved")) {
  model_struct <- list()
  save(model_struct, file = "model_struct.saved")
}

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
    tryCatch({
      # 심플하게 rxode2 호출
      m1 <- rxode2(ode)
      theta <- calcNomParams()
      "모델 초기화 성공!"
    }, error = function(e) {
      paste("모델 에러:", e$message)
    })
  })
  
  observeEvent(input$run, {
    # 모델 실행 코드
    m1 <- rxode2(ode)
    theta <- calcNomParams()
    # ... 시뮬레이션 코드
  })
}
  
shinyApp(ui = ui, server = server)

# app.R
library(shiny)
library(rxode2)
library(promises)
library(future)

# 비동기 처리 설정
plan(multisession)

# 모델 파일들 로드
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

# UI
ui <- fluidPage(
  titlePanel("Sepsis QSP Model"),
  
  sidebarLayout(
    sidebarPanel(
      h3("항생제 투여 설정"),
      numericInput("dose", "용량 (mg):", value = 1000, min = 0),
      numericInput("interval", "간격 (h):", value = 12, min = 6),
      br(),
      actionButton("compile", "1. 모델 컴파일", class = "btn-warning", style = "width: 100%;"),
      br(), br(),
      actionButton("run", "2. 시뮬레이션 실행", class = "btn-primary", style = "width: 100%;"),
      br(), br(),
      div(style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
          h5("사용 방법:"),
          tags$ol(
            tags$li("'모델 컴파일' 클릭 (30-60초 대기)"),
            tags$li("컴파일 완료 후 '시뮬레이션 실행' 클릭")
          )
      )
    ),
    
    mainPanel(
      h4("상태:"),
      verbatimTextOutput("status"),
      hr(),
      plotOutput("plot", height = "600px")
    )
  )
)

# 서버
server <- function(input, output, session) {
  
  # 전역 변수로 모델 저장
  values <- reactiveValues(
    m1 = NULL,
    theta = NULL,
    compiled = FALSE,
    compiling = FALSE
  )
  
  # 상태 표시
  output$status <- renderPrint({
    if(values$compiling) {
      "⏳ 컴파일 중... 30-60초 기다려주세요... (브라우저를 닫지 마세요)"
    } else if(!values$compiled) {
      "📌 먼저 '1. 모델 컴파일' 버튼을 눌러주세요"
    } else {
      "✅ 컴파일 완료! '2. 시뮬레이션 실행' 버튼을 누르세요"
    }
  })
  
  # 컴파일 버튼 - 사용자가 클릭할 때만 컴파일
  observeEvent(input$compile, {
    if(values$compiling) {
      showNotification("이미 컴파일 중입니다. 잠시만 기다려주세요.", type = "warning")
      return()
    }
    
    values$compiling <- TRUE
    output$status <- renderPrint({ 
      "⏳ 컴파일 시작... 30-60초 소요됩니다. 잠시만 기다려주세요..." 
    })
    
    # 비동기로 컴파일
    future_promise({
      # 실제 모델 컴파일
      m1 <- rxode2(ode)
      theta <- calcNomParams()
      list(m1 = m1, theta = theta)
    }) %...>% 
    function(result) {
      values$m1 <- result$m1
      values$theta <- result$theta
      values$compiled <- TRUE
      values$compiling <- FALSE
      output$status <- renderPrint({ 
        "✅ 컴파일 성공! 이제 '2. 시뮬레이션 실행'을 클릭하세요." 
      })
      showNotification("모델 컴파일 완료!", type = "success", duration = 5)
    } %...!% 
    function(error) {
      values$compiling <- FALSE
      output$status <- renderPrint({ 
        paste("❌ 컴파일 실패:", error$message) 
      })
      showNotification("컴파일 실패. 다시 시도해주세요.", type = "error")
    }
  })
  
  # 실행 버튼
  observeEvent(input$run, {
    if(!values$compiled) {
      output$status <- renderPrint({ 
        "⚠️ 먼저 '1. 모델 컴파일'을 완료해주세요!" 
      })
      showNotification("먼저 모델을 컴파일해주세요!", type = "warning")
      return()
    }
    
    output$status <- renderPrint({ "🔄 시뮬레이션 실행 중..." })
    
    # 시뮬레이션 실행
    tryCatch({
      # 초기값 설정 (원래 코드에서 복사)
      inits <- c(
        AngI=8.164, AngII=5.17, AT1_bound_AngII=16.6, AT2_bound_AngII=5.5,
        plasma_renin_concentration=17.845, blood_volume_L=5, 
        extracellular_fluid_volume=15, sodium_amount=700,
        ECF_sodium_amount=2100, tubulo_glomerular_feedback_effect=1,
        normalized_aldosterone_level_delayed=1,
        preafferent_pressure_autoreg_signal=1,
        glomerular_pressure_autoreg_signal=1,
        cardiac_output_delayed=5, CO_error=0, Na_concentration_error=0,
        normalized_vasopressin_concentration_delayed=1,
        F0_TGF=1.0183333333333333333333333333333e-14,
        P_bowmans=14, oncotic_pressure_difference=28,
        SN_macula_densa_Na_flow_delayed=5.0916666666666666666666666666666e-21,
        serum_creatinine=4.6, CFU_lung=3.5e8, CFU_blood=0,
        R_lung=1e6, R_blood=0, CINC1=30, Lag1=0, Lag2=0,
        NS1=0, NS2=0, AC=48, AT2=3e+10, AT1=2e+10,
        dAT1=250.26, dAT2=375.4, pDC=0, pDC1=0, M1=0,
        Th1=0, Th17=0, Treg=0, N=2455.6, TNFa=0.00024335,
        IL6=0.00014131, IL1b=0.028005, IL2=0.35115, IL12=0,
        IL17=1.13E-05, IL10=0, TGFb=0, GMCSF=0,
        TNFa_c=1.54E-09, IL6_c=6.42E-08, IL1b_c=1.27E-05,
        IL2_c=7.34E-06, IL12_c=8.93E-06, IL17_c=0,
        IL10_c=9.72E-07, TGFb_c=8.11E-07, GMCSF_c=4.04E-06,
        pDC_c=0.70096, M1_c=0.92091, N_c=0.48093,
        Th1_c=0.7964, Th17_c=0.00017499, Treg_c=0.21768,
        C_venous_vanco=0, C_vas_ki_vanco=0, C_vas_lu_vanco=0,
        C_vas_ad_vanco=0, C_vas_bo_vanco=0, C_vas_go_vanco=0,
        C_vas_he_vanco=0, C_vas_mu_vanco=0, C_vas_sk_vanco=0,
        C_vas_br_vanco=0, C_vas_li_vanco=0, C_vas_re_vanco=0,
        C_vas_gu_vanco=0, C_vas_sp_vanco=0, C_vas_pa_vanco=0,
        C_arterial_vanco=0
      )
      
      # 투여 설정
      ev1 <- eventTable(amount.units = "mg", time.units = "hours")
      ev1$add.dosing(dose = input$dose, start.time = 12, nbr.doses = 1, cmt = "depot")
      ev1$add.dosing(dose = input$dose, start.time = 24, 
                     nbr.doses = 10, dosing.interval = input$interval, cmt = "depot")
      ev1$add.sampling(seq(0, 200))  # 200시간까지
      
      # 시뮬레이션 실행
      x <- values$m1$run(values$theta, ev1, inits = inits)
      
      # 결과 플롯
      output$plot <- renderPlot({
        par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
        
        # MAP
        plot(x[,"time"], x[,"mean_arterial_pressure_MAP"], 
             xlab = "Time (h)", ylab = "MAP (mmHg)", 
             type = "l", col = "blue", lwd = 2,
             main = "Mean Arterial Pressure")
        grid()
        
        # GFR
        plot(x[,"time"], x[,"GFR_ml_min"], 
             xlab = "Time (h)", ylab = "GFR (ml/min)", 
             type = "l", col = "green", lwd = 2,
             main = "Glomerular Filtration Rate")
        grid()
        
        # Bacterial Load
        plot(x[,"time"], x[,"CFU_lung"], 
             xlab = "Time (h)", ylab = "CFU Lung", 
             type = "l", col = "red", lwd = 2,
             main = "Bacterial Load (Lung)", log = "y")
        grid()
        
        # Drug Concentration
        plot(x[,"time"], x[,"C_venous_vanco"], 
             xlab = "Time (h)", ylab = "Vancomycin (mg/L)", 
             type = "l", col = "purple", lwd = 2,
             main = "Drug Concentration")
        grid()
      })
      
      output$status <- renderPrint({ 
        paste("✅ 시뮬레이션 완료!", 
              "\n📊 용량:", input$dose, "mg",
              "\n⏰ 투여 간격:", input$interval, "시간") 
      })
      
      showNotification("시뮬레이션 완료!", type = "success", duration = 5)
      
    }, error = function(e) {
      output$status <- renderPrint({
        paste("❌ 시뮬레이션 에러:", e$message)
      })
      showNotification("시뮬레이션 실행 중 오류 발생", type = "error")
    })
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)

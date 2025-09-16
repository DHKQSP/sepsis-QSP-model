# app.R
library(shiny)
library(rxode2)

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
      actionButton("compile", "1. 모델 컴파일 시작", 
                   class = "btn-warning", 
                   style = "width: 100%;"),
      br(), br(),
      actionButton("run", "2. 시뮬레이션 실행", 
                   class = "btn-primary", 
                   style = "width: 100%;"),
      br(), br(),
      tags$div(style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
          tags$b("사용 방법:"),
          tags$ol(
            tags$li("'모델 컴파일 시작' 클릭"),
            tags$li("30-60초 기다리기"),
            tags$li("'시뮬레이션 실행' 클릭")
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
  
  # 반응형 값들
  values <- reactiveValues(
    m1 = NULL,
    theta = NULL,
    compiled = FALSE,
    compile_start = NULL
  )
  
  # 상태 표시
  output$status <- renderPrint({
    if(!is.null(values$compile_start) && !values$compiled) {
      elapsed <- round(difftime(Sys.time(), values$compile_start, units = "secs"))
      paste("⏳ 컴파일 중...", elapsed, "초 경과... (최대 60초 소요)")
    } else if(values$compiled) {
      "✅ 컴파일 완료! '시뮬레이션 실행' 버튼을 누르세요"
    } else {
      "📌 시작하려면 '1. 모델 컴파일 시작' 버튼을 누르세요"
    }
  })
  
  # 컴파일 버튼
  observeEvent(input$compile, {
    if(!is.null(values$compile_start) && !values$compiled) {
      showNotification("이미 컴파일 중입니다. 잠시만 기다려주세요.", 
                       type = "warning")
      return()
    }
    
    values$compile_start <- Sys.time()
    
    # isolate로 반응성 차단
    isolate({
      # 백그라운드에서 컴파일 시도
      withProgress(message = '모델 컴파일 중...', 
                   detail = '30-60초 소요됩니다', value = 0, {
        
        incProgress(0.2, detail = "ODE 시스템 준비...")
        
        tryCatch({
          # 컴파일 시간 제한
          setTimeLimit(cpu = 60, elapsed = 60, transient = TRUE)
          
          incProgress(0.3, detail = "C 코드 생성...")
          
          # 모델 컴파일
          m1 <- rxode2(ode)
          
          incProgress(0.6, detail = "파라미터 계산...")
          
          # 파라미터 계산
          theta <- calcNomParams()
          
          incProgress(0.9, detail = "완료 처리...")
          
          # 저장
          values$m1 <- m1
          values$theta <- theta
          values$compiled <- TRUE
          
          # 시간 제한 해제
          setTimeLimit(cpu = Inf, elapsed = Inf)
          
          showNotification("모델 컴파일 성공!", type = "success")
          
        }, error = function(e) {
          # 시간 제한 해제
          setTimeLimit(cpu = Inf, elapsed = Inf)
          
          values$compile_start <- NULL
          showNotification(paste("컴파일 실패:", e$message), 
                          type = "error", duration = 10)
        })
      })
    })
  })
  
  # 실행 버튼
  observeEvent(input$run, {
    if(!values$compiled) {
      showNotification("먼저 모델을 컴파일해주세요!", type = "warning")
      return()
    }
    
    withProgress(message = '시뮬레이션 실행 중...', value = 0, {
      
      tryCatch({
        incProgress(0.2, detail = "초기값 설정...")
        
        # 초기값 설정
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
        
        incProgress(0.4, detail = "투여 스케줄 설정...")
        
        # 투여 설정
        ev1 <- eventTable(amount.units = "mg", time.units = "hours")
        ev1$add.dosing(dose = input$dose, start.time = 12, 
                      nbr.doses = 1, cmt = "depot")
        ev1$add.dosing(dose = input$dose, start.time = 24, 
                      nbr.doses = 10, dosing.interval = input$interval, 
                      cmt = "depot")
        ev1$add.sampling(seq(0, 200))
        
        incProgress(0.6, detail = "시뮬레이션 계산...")
        
        # 시뮬레이션 실행
        x <- values$m1$run(values$theta, ev1, inits = inits)
        
        incProgress(0.9, detail = "그래프 생성...")
        
        # 결과 플롯
        output$plot <- renderPlot({
          par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
          
          plot(x[,"time"], x[,"mean_arterial_pressure_MAP"], 
               xlab = "Time (h)", ylab = "MAP (mmHg)", 
               type = "l", col = "blue", lwd = 2,
               main = "Mean Arterial Pressure")
          grid()
          
          plot(x[,"time"], x[,"GFR_ml_min"], 
               xlab = "Time (h)", ylab = "GFR (ml/min)", 
               type = "l", col = "green", lwd = 2,
               main = "Glomerular Filtration Rate")
          grid()
          
          plot(x[,"time"], x[,"CFU_lung"], 
               xlab = "Time (h)", ylab = "CFU Lung", 
               type = "l", col = "red", lwd = 2,
               main = "Bacterial Load (Lung)", log = "y")
          grid()
          
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
        
        showNotification("시뮬레이션 완료!", type = "success")
        
      }, error = function(e) {
        output$status <- renderPrint({
          paste("❌ 시뮬레이션 에러:", e$message)
        })
        showNotification("시뮬레이션 실행 실패", type = "error")
      })
    })
  })
  
  # 자동 상태 업데이트 (컴파일 중일 때)
  observe({
    if(!is.null(values$compile_start) && !values$compiled) {
      invalidateLater(1000)  # 1초마다 업데이트
    }
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)

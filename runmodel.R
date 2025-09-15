# app.R
library(shiny)
library(rxode2)

# 기존 파일들 로드
source("modelfile_0822.R")
load("model_struct.saved")
source("calcNomParams_human_0602.R")

# UI 정의
ui <- fluidPage(
  titlePanel("Sepsis QSP Model Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      h3("약물 투여 설정"),
      numericInput("dose1", "첫 투여 용량 (mg):", value = 1000, min = 0),
      numericInput("dose2", "유지 용량 (mg):", value = 1000, min = 0),
      numericInput("ndoses", "유지 투여 횟수:", value = 27, min = 1),
      numericInput("interval", "투여 간격 (hours):", value = 12, min = 1),
      numericInput("sim_time", "시뮬레이션 시간 (hours):", value = 1000, min = 100),
      br(),
      actionButton("run", "시뮬레이션 실행", class = "btn-primary"),
      br(), br(),
      downloadButton("downloadData", "결과 데이터 다운로드")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("생리학적 지표",
                 plotOutput("physio_plot", height = "600px")),
        tabPanel("사이토카인 (Lung)",
                 plotOutput("cytokine_lung", height = "800px")),
        tabPanel("사이토카인 (Plasma)", 
                 plotOutput("cytokine_plasma", height = "800px")),
        tabPanel("면역세포",
                 plotOutput("immune_plot", height = "800px")),
        tabPanel("약물농도",
                 plotOutput("drug_plot", height = "400px")),
        tabPanel("데이터 테이블",
                 DT::dataTableOutput("data_table"))
      )
    )
  )
)

# Server 정의
server <- function(input, output) {
  
  # 모델 초기화
  m1 <- rxode2(model = ode, modname = mod1)
  theta <- calcNomParams()
  
  # 초기값 설정
  inits <- c(AngI=8.164, AngII=5.17,AT1_bound_AngII=16.6, AT2_bound_AngII = 5.5, 
             plasma_renin_concentration=17.845,
             blood_volume_L=5,extracellular_fluid_volume=15,
             sodium_amount= 700, 
             ECF_sodium_amount= 2100, 
             tubulo_glomerular_feedback_effect=1,
             normalized_aldosterone_level_delayed=1, 
             preafferent_pressure_autoreg_signal=1, 
             glomerular_pressure_autoreg_signal=1, 
             cardiac_output_delayed=5,
             CO_error=0, Na_concentration_error = 0, 
             normalized_vasopressin_concentration_delayed = 1,
             F0_TGF=1.0183333333333333333333333333333e-14, 
             P_bowmans=14, 
             oncotic_pressure_difference=28,
             SN_macula_densa_Na_flow_delayed = 5.0916666666666666666666666666666e-21,
             serum_creatinine = 4.6,
             CFU_lung = 3.5e8, CFU_blood=0, R_lung=1e6 , R_blood=0 ,CINC1 = 30, Lag1=0, 
             Lag2=0, NS1 = 0, NS2 = 0, AC=48,
             AT2=3e+10, AT1=2e+10, dAT1=250.26, dAT2=375.4,
             pDC=0,pDC1=0, M1=0, Th1=0, Th17=0, Treg=0, N = 2455.6, TNFa = 0.00024335, 
             IL6=0.00014131,
             IL1b =0.028005 , IL2 = 0.35115, IL12 = 0, IL17=1.13E-05, IL10=0,TGFb=0,GMCSF=0,
             TNFa_c = 1.54E-09  , IL6_c=6.42E-08, IL1b_c=1.27E-05, IL2_c=7.34E-06, 
             IL12_c=8.93E-06, IL17_c=0, IL10_c=9.72E-07, TGFb_c=8.11E-07, GMCSF_c=4.04E-06,
             pDC_c = 0.70096, M1_c =0.92091, N_c = 0.48093, Th1_c = 0.7964, 
             Th17_c = 0.00017499, Treg_c = 0.21768,
             C_venous_vanco = 0, C_vas_ki_vanco = 0, C_vas_lu_vanco = 0, C_vas_ad_vanco = 0,
             C_vas_bo_vanco = 0, C_vas_go_vanco = 0,
             C_vas_he_vanco = 0, C_vas_mu_vanco = 0, C_vas_sk_vanco = 0, C_vas_br_vanco = 0, 
             C_vas_li_vanco = 0,  C_vas_re_vanco = 0,
             C_vas_gu_vanco = 0, C_vas_sp_vanco = 0, C_vas_pa_vanco = 0, C_arterial_vanco = 0)
  
  # 시뮬레이션 실행 (reactive)
  sim_results <- eventReactive(input$run, {
    ev1 <- eventTable(amount.units = "mg", time.units = "hours")
    ev1$add.dosing(dose=input$dose1, start.time = 12, nbr.doses = 1, cmt="depot")
    ev1$add.dosing(dose=input$dose2, start.time = 24, nbr.doses = input$ndoses,
                   dosing.interval = input$interval, cmt="depot")
    ev1$add.sampling(seq(0, input$sim_time))
    
    x <- m1$run(theta, ev1, inits=inits)
    return(as.data.frame(x))
  })
  
  # 생리학적 지표 플롯
  output$physio_plot <- renderPlot({
    req(sim_results())
    x <- sim_results()
    
    par(mfrow=c(2,4))
    plot(x$time, x$mean_arterial_pressure_MAP, xlab="Time (h)", ylab="MAP (mmHg)", 
         cex.lab=1.5, type="l", main="Mean Arterial Pressure")
    plot(x$time, x$GFR_ml_min, xlab="Time (h)", ylab="GFR (ml/min)", 
         type="l", cex.lab=1.5, main="GFR")
    plot(x$time, x$serum_creatinine_concentration, xlab="Time (h)", ylab="Scr (mg/dL)", 
         type="l", cex.lab=1.5, main="Serum Creatinine")
    plot(x$time, x$CFU_lung, xlab="Time (h)", ylab="CFU Lung", 
         cex.lab=1.5, type="l", main="Bacterial Load", log="y")
    plot(x$time, x$cardiac_output, xlab="Time (h)", ylab="Cardiac Output", 
         cex.lab=1.5, type="l", main="Cardiac Output")
    plot(x$time, x$Q_kidney, xlab="Time (h)", ylab="Q_kidney", 
         cex.lab=1.5, type="l", main="Kidney Blood Flow")
    plot(x$time, x$CL_renal_vanco, xlab="Time (h)", ylab="CL_renal", 
         cex.lab=1.5, type="l", main="Renal Clearance")
    plot(x$time, x$C_vas_ki_vanco, xlab="Time (h)", ylab="C_kidney", 
         cex.lab=1.5, type="l", main="Kidney Drug Conc.")
  })
  
  # 사이토카인 플롯 (Lung)
  output$cytokine_lung <- renderPlot({
    req(sim_results())
    x <- sim_results()
    
    par(mfrow=c(3,3))
    plot(x$time, x$IL1b_pg_mL, xlab="Time (h)", ylab="IL-1β (pg/mL)", 
         cex.lab=1.3, type="l", main="Lung IL-1β")
    plot(x$time, x$IL6_pg_mL, xlab="Time (h)", ylab="IL-6 (pg/mL)", 
         cex.lab=1.3, type="l", main="Lung IL-6")
    plot(x$time, x$IL10_pg_mL, xlab="Time (h)", ylab="IL-10 (pg/mL)", 
         cex.lab=1.3, type="l", main="Lung IL-10")
    plot(x$time, x$TGFb_pg_mL, xlab="Time (h)", ylab="TGF-β (pg/mL)", 
         cex.lab=1.3, type="l", main="Lung TGF-β")
    plot(x$time, x$TNFa_pg_mL, xlab="Time (h)", ylab="TNF-α (pg/mL)", 
         cex.lab=1.3, type="l", main="Lung TNF-α")
    plot(x$time, x$IL12, xlab="Time (h)", ylab="IL-12 (pg/mL)", 
         cex.lab=1.3, type="l", main="Lung IL-12")
    plot(x$time, x$GMCSF, xlab="Time (h)", ylab="GM-CSF", 
         cex.lab=1.3, type="l", main="GM-CSF")
    plot(x$time, x$dAT1, xlab="Time (h)", ylab="dAT1", 
         cex.lab=1.3, type="l", main="dAT1")
    plot(x$time, x$dAT2, xlab="Time (h)", ylab="dAT2", 
         cex.lab=1.3, type="l", main="dAT2")
  })
  
  # 사이토카인 플롯 (Plasma)
  output$cytokine_plasma <- renderPlot({
    req(sim_results())
    x <- sim_results()
    
    par(mfrow=c(3,3))
    plot(x$time, x$IL1b_c_pg_mL, xlab="Time (h)", ylab="IL-1β (pg/mL)", 
         cex.lab=1.3, type="l", main="Plasma IL-1β")
    plot(x$time, x$IL6_c_pg_mL, xlab="Time (h)", ylab="IL-6 (pg/mL)", 
         cex.lab=1.3, type="l", main="Plasma IL-6")
    plot(x$time, x$IL10_c_pg_mL, xlab="Time (h)", ylab="IL-10 (pg/mL)", 
         cex.lab=1.3, type="l", main="Plasma IL-10")
    plot(x$time, x$TGFb_c_pg_mL, xlab="Time (h)", ylab="TGF-β (pg/mL)", 
         cex.lab=1.3, type="l", main="Plasma TGF-β")
    plot(x$time, x$TNFa_c_pg_mL, xlab="Time (h)", ylab="TNF-α (pg/mL)", 
         cex.lab=1.3, type="l", main="Plasma TNF-α")
    plot(x$time, x$CRP_mg_dl, xlab="Time (h)", ylab="CRP (mg/dL)", 
         cex.lab=1.3, type="l", main="Plasma CRP")
    plot(x$time, x$nom_RVR, xlab="Time (h)", ylab="RVR", 
         cex.lab=1.3, type="l", main="RVR")
  })
  
  # 면역세포 플롯
  output$immune_plot <- renderPlot({
    req(sim_results())
    x <- sim_results()
    
    par(mfrow=c(3,4))
    plot(x$time, x$N, xlab="Time (h)", ylab="Neutrophils", 
         cex.lab=1.3, type="l", main="Lung Neutrophils")
    plot(x$time, x$N_c, xlab="Time (h)", ylab="Neutrophils", 
         cex.lab=1.3, type="l", main="Plasma Neutrophils")
    plot(x$time, x$pDC, xlab="Time (h)", ylab="Dendritic Cells", 
         cex.lab=1.3, type="l", main="Lung DC")
    plot(x$time, x$pDC_c, xlab="Time (h)", ylab="Dendritic Cells", 
         cex.lab=1.3, type="l", main="Plasma DC")
    plot(x$time, x$M1, xlab="Time (h)", ylab="M1 Macrophages", 
         cex.lab=1.3, type="l", main="Lung M1")
    plot(x$time, x$M1_c, xlab="Time (h)", ylab="M1 Macrophages", 
         cex.lab=1.3, type="l", main="Plasma M1")
    plot(x$time, x$Th1, xlab="Time (h)", ylab="Th1 Cells", 
         cex.lab=1.3, type="l", main="Lung Th1")
    plot(x$time, x$Th1_c, xlab="Time (h)", ylab="Th1 Cells", 
         cex.lab=1.3, type="l", main="Plasma Th1")
    plot(x$time, x$Th17, xlab="Time (h)", ylab="Th17 Cells", 
         cex.lab=1.3, type="l", main="Lung Th17")
    plot(x$time, x$Th17_c, xlab="Time (h)", ylab="Th17 Cells", 
         cex.lab=1.3, type="l", main="Plasma Th17")
    plot(x$time, x$Treg, xlab="Time (h)", ylab="Treg Cells", 
         cex.lab=1.3, type="l", main="Lung Treg")
    plot(x$time, x$Treg_c, xlab="Time (h)", ylab="Treg Cells", 
         cex.lab=1.3, type="l", main="Plasma Treg")
  })
  
  # 약물농도 플롯
  output$drug_plot <- renderPlot({
    req(sim_results())
    x <- sim_results()
    
    par(mfrow=c(1,2))
    plot(x$time, x$C_venous_vanco, xlab="Time (h)", ylab="Plasma Vanco (mg/L)", 
         cex.lab=1.5, type="l", main="Plasma Vancomycin")
    plot(x$time, x$C_vas_lu_vanco, xlab="Time (h)", ylab="Lung Vanco (mg/L)", 
         cex.lab=1.5, type="l", main="Lung Vancomycin")
  })
  
  # 데이터 테이블
  output$data_table <- DT::renderDataTable({
    req(sim_results())
    DT::datatable(sim_results(), options = list(pageLength = 25, scrollX = TRUE))
  })
  
  # 다운로드 핸들러
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("simulation_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sim_results(), file, row.names = FALSE)
    }
  )
}

# 앱 실행
shinyApp(ui = ui, server = server)

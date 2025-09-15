# app.R - Sepsis QSP Model with Antibiotic Regimen Optimization
library(shiny)
library(shinydashboard)
library(rxode2)
library(ggplot2)
library(plotly)
library(DT)

# Load model files
source("modelfile_0822.R")
tryCatch({
  load("model_struct.saved")
}, error = function(e) {
  print("model_struct.saved not found, continuing...")
})
source("calcNomParams_human_0602.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Sepsis QSP Model - Antibiotic Optimization"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simulation", tabName = "simulation", icon = icon("play-circle")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Data Export", tabName = "export", icon = icon("download"))
    ),
    
    hr(),
    h4("항생제 투여 설정", style="padding-left:15px;"),
    
    # Loading dose
    h5("초기 부하 용량 (Loading)", style="padding-left:15px; color:#666;"),
    numericInput("loading_dose", "용량 (mg):", value = 1000, min = 0, max = 5000, step = 250),
    numericInput("loading_time", "투여 시간 (h):", value = 12, min = 0, max = 48),
    
    # Maintenance dose
    h5("유지 용량 (Maintenance)", style="padding-left:15px; color:#666;"),
    numericInput("maint_dose", "용량 (mg):", value = 1000, min = 0, max = 2000, step = 250),
    numericInput("maint_interval", "투여 간격 (h):", value = 12, min = 6, max = 48, step = 6),
    numericInput("maint_doses", "투여 횟수:", value = 27, min = 1, max = 100),
    numericInput("maint_start", "시작 시간 (h):", value = 24, min = 0, max = 72),
    
    hr(),
    h5("시뮬레이션 설정", style="padding-left:15px; color:#666;"),
    numericInput("sim_time", "시뮬레이션 시간 (h):", value = 336, min = 48, max = 1000, step = 24),
    
    br(),
    actionButton("run", "시뮬레이션 실행", class = "btn-success btn-block", 
                 style = "margin: 15px;"),
    
    # Add preset buttons
    hr(),
    h5("사전 설정 레지멘", style="padding-left:15px; color:#666;"),
    actionButton("preset_standard", "표준 요법", class = "btn-info btn-sm", style = "margin: 5px;"),
    actionButton("preset_aggressive", "적극적 요법", class = "btn-warning btn-sm", style = "margin: 5px;"),
    actionButton("preset_conservative", "보존적 요법", class = "btn-default btn-sm", style = "margin: 5px;")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box-title {
          font-weight: bold;
        }
      "))
    ),
    
    tabItems(
      # Simulation Tab
      tabItem(tabName = "simulation",
        fluidRow(
          # Key Metrics
          valueBoxOutput("map_box"),
          valueBoxOutput("gfr_box"),
          valueBoxOutput("cfu_box")
        ),
        
        fluidRow(
          # Primary outcomes
          box(title = "혈역학 및 신기능", width = 6, status = "primary",
              plotlyOutput("hemodynamics_plot", height = "400px")),
          box(title = "세균 부하 및 약물 농도", width = 6, status = "primary",
              plotlyOutput("infection_plot", height = "400px"))
        ),
        
        fluidRow(
          # Inflammatory markers
          box(title = "염증 지표 (Cytokines)", width = 12, status = "warning",
              plotlyOutput("cytokine_plot", height = "500px"))
        )
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
        fluidRow(
          box(title = "임상 지표 요약", width = 6,
              h4("주요 Endpoint 분석"),
              tableOutput("summary_table")),
          box(title = "PK/PD 파라미터", width = 6,
              h4("약동학/약력학 지표"),
              tableOutput("pkpd_table"))
        ),
        
        fluidRow(
          box(title = "면역 반응 패턴", width = 12,
              plotlyOutput("immune_heatmap", height = "600px"))
        )
      ),
      
      # Export Tab
      tabItem(tabName = "export",
        fluidRow(
          box(title = "데이터 내보내기", width = 12,
              h4("시뮬레이션 결과 다운로드"),
              br(),
              downloadButton("downloadCSV", "CSV 다운로드", class = "btn-primary"),
              downloadButton("downloadReport", "보고서 생성 (HTML)", class = "btn-info"),
              br(), br(),
              h4("데이터 미리보기"),
              DT::dataTableOutput("data_preview")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Initialize model
  m1 <- rxode2(model = ode, modname = mod1)
  theta <- calcNomParams()
  
  # Initial conditions
  inits <- c(
    AngI=8.164, AngII=5.17, AT1_bound_AngII=16.6, AT2_bound_AngII=5.5,
    plasma_renin_concentration=17.845, blood_volume_L=5, extracellular_fluid_volume=15,
    sodium_amount=700, ECF_sodium_amount=2100, tubulo_glomerular_feedback_effect=1,
    normalized_aldosterone_level_delayed=1, preafferent_pressure_autoreg_signal=1,
    glomerular_pressure_autoreg_signal=1, cardiac_output_delayed=5,
    CO_error=0, Na_concentration_error=0, normalized_vasopressin_concentration_delayed=1,
    F0_TGF=1.0183333333333333333333333333333e-14, P_bowmans=14, oncotic_pressure_difference=28,
    SN_macula_densa_Na_flow_delayed=5.0916666666666666666666666666666e-21,
    serum_creatinine=4.6, CFU_lung=3.5e8, CFU_blood=0, R_lung=1e6, R_blood=0,
    CINC1=30, Lag1=0, Lag2=0, NS1=0, NS2=0, AC=48,
    AT2=3e+10, AT1=2e+10, dAT1=250.26, dAT2=375.4,
    pDC=0, pDC1=0, M1=0, Th1=0, Th17=0, Treg=0, N=2455.6,
    TNFa=0.00024335, IL6=0.00014131, IL1b=0.028005, IL2=0.35115,
    IL12=0, IL17=1.13E-05, IL10=0, TGFb=0, GMCSF=0,
    TNFa_c=1.54E-09, IL6_c=6.42E-08, IL1b_c=1.27E-05, IL2_c=7.34E-06,
    IL12_c=8.93E-06, IL17_c=0, IL10_c=9.72E-07, TGFb_c=8.11E-07, GMCSF_c=4.04E-06,
    pDC_c=0.70096, M1_c=0.92091, N_c=0.48093, Th1_c=0.7964,
    Th17_c=0.00017499, Treg_c=0.21768,
    C_venous_vanco=0, C_vas_ki_vanco=0, C_vas_lu_vanco=0, C_vas_ad_vanco=0,
    C_vas_bo_vanco=0, C_vas_go_vanco=0, C_vas_he_vanco=0, C_vas_mu_vanco=0,
    C_vas_sk_vanco=0, C_vas_br_vanco=0, C_vas_li_vanco=0, C_vas_re_vanco=0,
    C_vas_gu_vanco=0, C_vas_sp_vanco=0, C_vas_pa_vanco=0, C_arterial_vanco=0
  )
  
  # Preset buttons
  observeEvent(input$preset_standard, {
    updateNumericInput(session, "loading_dose", value = 1000)
    updateNumericInput(session, "maint_dose", value = 1000)
    updateNumericInput(session, "maint_interval", value = 12)
  })
  
  observeEvent(input$preset_aggressive, {
    updateNumericInput(session, "loading_dose", value = 1500)
    updateNumericInput(session, "maint_dose", value = 1250)
    updateNumericInput(session, "maint_interval", value = 8)
  })
  
  observeEvent(input$preset_conservative, {
    updateNumericInput(session, "loading_dose", value = 750)
    updateNumericInput(session, "maint_dose", value = 500)
    updateNumericInput(session, "maint_interval", value = 24)
  })
  
  # Run simulation
  sim_results <- eventReactive(input$run, {
    withProgress(message = '시뮬레이션 실행 중...', value = 0, {
      # Create dosing regimen
      ev1 <- eventTable(amount.units = "mg", time.units = "hours")
      
      if(input$loading_dose > 0) {
        ev1$add.dosing(dose = input$loading_dose, 
                      start.time = input$loading_time, 
                      nbr.doses = 1, 
                      cmt = "depot")
      }
      
      if(input$maint_dose > 0 && input$maint_doses > 0) {
        ev1$add.dosing(dose = input$maint_dose, 
                      start.time = input$maint_start, 
                      nbr.doses = input$maint_doses,
                      dosing.interval = input$maint_interval, 
                      cmt = "depot")
      }
      
      ev1$add.sampling(seq(0, input$sim_time, by = 1))
      
      incProgress(0.3, detail = "모델 계산 중...")
      
      # Run model
      x <- m1$run(theta, ev1, inits = inits)
      
      incProgress(0.7, detail = "결과 처리 중...")
      
      df <- as.data.frame(x)
      
      # Calculate additional metrics
      df$MAP_change <- (df$mean_arterial_pressure_MAP - df$mean_arterial_pressure_MAP[1]) / 
                       df$mean_arterial_pressure_MAP[1] * 100
      df$GFR_change <- (df$GFR_ml_min - df$GFR_ml_min[1]) / df$GFR_ml_min[1] * 100
      df$CFU_log <- log10(pmax(df$CFU_lung, 1))
      
      incProgress(1, detail = "완료!")
      
      return(df)
    })
  })
  
  # Value boxes
  output$map_box <- renderValueBox({
    if(is.null(sim_results())) {
      valueBox(
        value = "---",
        subtitle = "MAP (mmHg)",
        icon = icon("heartbeat"),
        color = "blue"
      )
    } else {
      df <- sim_results()
      final_map <- round(tail(df$mean_arterial_pressure_MAP, 1), 1)
      color <- if(final_map < 65) "red" else if(final_map < 70) "yellow" else "green"
      
      valueBox(
        value = final_map,
        subtitle = "최종 MAP (mmHg)",
        icon = icon("heartbeat"),
        color = color
      )
    }
  })
  
  output$gfr_box <- renderValueBox({
    if(is.null(sim_results())) {
      valueBox(
        value = "---",
        subtitle = "GFR (ml/min)",
        icon = icon("filter"),
        color = "blue"
      )
    } else {
      df <- sim_results()
      final_gfr <- round(tail(df$GFR_ml_min, 1), 1)
      color <- if(final_gfr < 30) "red" else if(final_gfr < 60) "yellow" else "green"
      
      valueBox(
        value = final_gfr,
        subtitle = "최종 GFR (ml/min)",
        icon = icon("filter"),
        color = color
      )
    }
  })
  
  output$cfu_box <- renderValueBox({
    if(is.null(sim_results())) {
      valueBox(
        value = "---",
        subtitle = "Bacterial Load",
        icon = icon("bacterium"),
        color = "blue"
      )
    } else {
      df <- sim_results()
      final_cfu <- tail(df$CFU_log, 1)
      reduction <- round(df$CFU_log[1] - final_cfu, 1)
      
      valueBox(
        value = paste0(reduction, " log"),
        subtitle = "CFU 감소 (log10)",
        icon = icon("bacterium"),
        color = if(reduction > 3) "green" else if(reduction > 1) "yellow" else "red"
      )
    }
  })
  
  # Hemodynamics plot
  output$hemodynamics_plot <- renderPlotly({
    req(sim_results())
    df <- sim_results()
    
    p1 <- plot_ly(df, x = ~time) %>%
      add_trace(y = ~mean_arterial_pressure_MAP, name = "MAP", 
                type = 'scatter', mode = 'lines',
                line = list(color = 'rgb(22, 96, 167)'),
                yaxis = "y") %>%
      add_trace(y = ~GFR_ml_min, name = "GFR", 
                type = 'scatter', mode = 'lines',
                line = list(color = 'rgb(205, 12, 24)'),
                yaxis = "y2") %>%
      add_trace(y = ~serum_creatinine_concentration, name = "Scr", 
                type = 'scatter', mode = 'lines',
                line = list(color = 'rgb(22, 167, 96)'),
                yaxis = "y3") %>%
      layout(
        title = "혈역학 및 신기능 지표",
        xaxis = list(title = "시간 (hours)"),
        yaxis = list(title = "MAP (mmHg)", side = "left",
                    titlefont = list(color = 'rgb(22, 96, 167)'),
                    tickfont = list(color = 'rgb(22, 96, 167)')),
        yaxis2 = list(title = "GFR (ml/min)", overlaying = "y", side = "right",
                     titlefont = list(color = 'rgb(205, 12, 24)'),
                     tickfont = list(color = 'rgb(205, 12, 24)')),
        yaxis3 = list(title = "Scr (mg/dL)", overlaying = "y", side = "right",
                     position = 0.95,
                     titlefont = list(color = 'rgb(22, 167, 96)'),
                     tickfont = list(color = 'rgb(22, 167, 96)')),
        hovermode = 'x unified'
      )
    
    p1
  })
  
  # Infection plot
  output$infection_plot <- renderPlotly({
    req(sim_results())
    df <- sim_results()
    
    p2 <- plot_ly(df, x = ~time) %>%
      add_trace(y = ~CFU_log, name = "Bacterial Load (log10)", 
                type = 'scatter', mode = 'lines',
                line = list(color = 'red', width = 2),
                yaxis = "y") %>%
      add_trace(y = ~C_venous_vanco, name = "Plasma Vanco", 
                type = 'scatter', mode = 'lines',
                line = list(color = 'blue', width = 2),
                yaxis = "y2") %>%
      layout(
        title = "세균 부하 및 항생제 농도",
        xaxis = list(title = "시간 (hours)"),
        yaxis = list(title = "CFU (log10)", side = "left",
                    titlefont = list(color = 'red'),
                    tickfont = list(color = 'red')),
        yaxis2 = list(title = "Vancomycin (mg/L)", overlaying = "y", side = "right",
                     titlefont = list(color = 'blue'),
                     tickfont = list(color = 'blue')),
        hovermode = 'x unified',
        shapes = list(
          list(type = "line", x0 = 0, x1 = max(df$time), 
               y0 = 3, y1 = 3, yref = "y",
               line = list(color = "gray", dash = "dash"))
        )
      )
    
    p2
  })
  
  # Cytokine plot
  output$cytokine_plot <- renderPlotly({
    req(sim_results())
    df <- sim_results()
    
    # Normalize cytokine values for better visualization
    cytokines <- c("IL1b_pg_mL", "IL6_pg_mL", "TNFa_pg_mL", "IL10_pg_mL", "TGFb_pg_mL")
    
    p3 <- plot_ly(df, x = ~time)
    
    colors <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
    
    for(i in 1:length(cytokines)) {
      p3 <- p3 %>% add_trace(y = ~get(cytokines[i]), 
                            name = gsub("_pg_mL", "", cytokines[i]),
                            type = 'scatter', mode = 'lines',
                            line = list(color = colors[i], width = 2))
    }
    
    p3 <- p3 %>% layout(
      title = "염증성 사이토카인 변화",
      xaxis = list(title = "시간 (hours)"),
      yaxis = list(title = "농도 (pg/mL)", type = "log"),
      hovermode = 'x unified',
      legend = list(x = 0.7, y = 1)
    )
    
    p3
  })
  
  # Summary table
  output$summary_table <- renderTable({
    req(sim_results())
    df <- sim_results()
    
    summary_data <- data.frame(
      지표 = c("MAP 최저치", "GFR 최저치", "Scr 최고치", 
              "CFU 감소", "치료 효과 시간"),
      값 = c(
        paste0(round(min(df$mean_arterial_pressure_MAP), 1), " mmHg"),
        paste0(round(min(df$GFR_ml_min), 1), " ml/min"),
        paste0(round(max(df$serum_creatinine_concentration), 2), " mg/dL"),
        paste0(round(df$CFU_log[1] - tail(df$CFU_log, 1), 2), " log10"),
        paste0(which(df$CFU_log < (df$CFU_log[1] - 1))[1], " hours")
      ),
      상태 = c(
        ifelse(min(df$mean_arterial_pressure_MAP) < 65, "위험", "정상"),
        ifelse(min(df$GFR_ml_min) < 30, "위험", "정상"),
        ifelse(max(df$serum_creatinine_concentration) > 2, "상승", "정상"),
        ifelse((df$CFU_log[1] - tail(df$CFU_log, 1)) > 3, "효과적", "불충분"),
        ""
      )
    )
    
    summary_data
  })
  
  # PK/PD table
  output$pkpd_table <- renderTable({
    req(sim_results())
    df <- sim_results()
    
    # Calculate PK/PD parameters
    cmax <- max(df$C_venous_vanco)
    tmax <- df$time[which.max(df$C_venous_vanco)]
    auc <- sum(df$C_venous_vanco) # Simplified AUC
    
    pkpd_data <- data.frame(
      파라미터 = c("Cmax", "Tmax", "AUC", "T>MIC", "AUC/MIC"),
      값 = c(
        paste0(round(cmax, 2), " mg/L"),
        paste0(tmax, " hours"),
        paste0(round(auc, 1), " mg*h/L"),
        paste0(sum(df$C_venous_vanco > 15), " hours"),
        paste0(round(auc/15, 1))
      )
    )
    
    pkpd_data
  })
  
  # Immune heatmap
  output$immune_heatmap <- renderPlotly({
    req(sim_results())
    df <- sim_results()
    
    # Select time points for heatmap
    time_points <- seq(1, nrow(df), length.out = min(50, nrow(df)))
    
    # Select immune markers
    immune_markers <- c("N", "M1", "pDC", "Th1", "Th17", "Treg")
    
    # Create matrix for heatmap
    mat <- as.matrix(df[time_points, immune_markers])
    
    # Normalize by column (each marker)
    mat_norm <- scale(mat)
    
    plot_ly(
      x = immune_markers,
      y = df$time[time_points],
      z = mat_norm,
      type = "heatmap",
      colorscale = "RdBu",
      reversescale = TRUE
    ) %>%
      layout(
        title = "면역세포 활성화 패턴",
        xaxis = list(title = "면역세포 유형"),
        yaxis = list(title = "시간 (hours)")
      )
  })
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    req(sim_results())
    df <- sim_results()
    
    # Select key columns for preview
    preview_cols <- c("time", "mean_arterial_pressure_MAP", "GFR_ml_min", 
                     "serum_creatinine_concentration", "CFU_lung", 
                     "C_venous_vanco", "IL6_pg_mL", "TNFa_pg_mL")
    
    DT::datatable(
      df[, preview_cols],
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      )
    ) %>%
      formatRound(columns = 2:8, digits = 2)
  })
  
  # Download handlers
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste0("sepsis_simulation_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(sim_results(), file, row.names = FALSE)
    }
  )
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("sepsis_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      df <- sim_results()
      
      html_content <- paste0(
        "<html><head><title>Sepsis QSP Model Report</title></head><body>",
        "<h1>Sepsis QSP Model Simulation Report</h1>",
        "<h2>투여 설정</h2>",
        "<p>Loading: ", input$loading_dose, " mg at ", input$loading_time, " h</p>",
        "<p>Maintenance: ", input$maint_dose, " mg every ", input$maint_interval, " h</p>",
        "<h2>주요 결과</h2>",
        "<p>최종 MAP: ", round(tail(df$mean_arterial_pressure_MAP, 1), 1), " mmHg</p>",
        "<p>최종 GFR: ", round(tail(df$GFR_ml_min, 1), 1), " ml/min</p>",
        "<p>CFU 감소: ", round(df$CFU_log[1] - tail(df$CFU_log, 1), 2), " log10</p>",
        "<p>생성일: ", Sys.Date(), "</p>",
        "</body></html>"
      )
      
      writeLines(html_content, file)
    }
  )
}

# Run app
shinyApp(ui = ui, server = server)

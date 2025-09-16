# app.R
library(shiny)
library(rxode2)

# Load model files
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
  titlePanel("Sepsis QSP Model - Vancomycin PK/PD Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Model Setup"),
      actionButton("compile", "1. Compile Model", 
                   class = "btn-warning", 
                   style = "width: 100%;"),
      br(), br(),
      
      h3("Vancomycin Dosing Regimen"),
      numericInput("dose", "Dose per administration (mg):", 
                   value = 1000, min = 0, max = 5000, step = 100),
      
      numericInput("interval", "Dosing interval (hours):", 
                   value = 12, min = 6, max = 48, step = 6),
      
      numericInput("n_doses", "Number of doses:", 
                   value = 10, min = 1, max = 30, step = 1),
      
      numericInput("start_time", "First dose time (hours):", 
                   value = 12, min = 0, max = 48, step = 1),
      
      br(),
      h3("Simulation Settings"),
      numericInput("sim_duration", "Simulation duration (hours):", 
                   value = 200, min = 24, max = 720, step = 24),
      
      br(),
      actionButton("run", "2. Run Simulation", 
                   class = "btn-primary", 
                   style = "width: 100%;"),
      br(), br(),
      
      # Download button (조건부 렌더링으로 변경)
      uiOutput("downloadUI"),
      br(),
      
      tags$div(style = "background-color: #e8f4f8; padding: 10px; border-radius: 5px;",
          tags$b("Instructions:"),
          tags$ol(
            tags$li("Click 'Compile Model' (first time only)"),
            tags$li("Adjust dosing parameters"),
            tags$li("Click 'Run Simulation'"),
            tags$li("Download results if needed")
          )
      ),
      br(),
      tags$div(style = "background-color: #fff3cd; padding: 10px; border-radius: 5px;",
          tags$b("Vancomycin Target Levels:"),
          tags$ul(
            tags$li("Trough: 10-20 mg/L"),
            tags$li("Peak: < 40 mg/L"),
            tags$li("AUC/MIC target: 400-600")
          )
      )
    ),
    
    mainPanel(
      h4("System Status:"),
      verbatimTextOutput("status"),
      hr(),
      h4("Vancomycin Dosing Summary:"),
      verbatimTextOutput("dosing_summary"),
      hr(),
      plotOutput("plot", height = "700px"),
      hr(),
      h4("Key Metrics:"),
      tableOutput("metrics_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    m1 = NULL,
    theta = NULL,
    compiled = FALSE,
    compile_start = NULL,
    last_results = NULL,
    simulation_params = NULL,
    simulation_complete = FALSE
  )
  
  # Conditional download button rendering
  output$downloadUI <- renderUI({
    if(values$simulation_complete) {
      downloadButton("downloadData", "Download Results (CSV)", 
                     class = "btn-success",
                     style = "width: 100%;")
    } else {
      tags$button("Download Results (CSV)", 
                  class = "btn btn-success disabled",
                  style = "width: 100%; opacity: 0.5; cursor: not-allowed;",
                  disabled = "disabled")
    }
  })
  
  # Status display
  output$status <- renderPrint({
    if(!is.null(values$compile_start) && !values$compiled) {
      elapsed <- round(difftime(Sys.time(), values$compile_start, units = "secs"))
      paste("⏳ Compiling model...", elapsed, "seconds elapsed... (max 60 seconds)")
    } else if(values$compiled) {
      "✅ Model compiled! Set parameters and click 'Run Simulation'"
    } else {
      "📌 To start, click '1. Compile Model' button"
    }
  })
  
  # Dosing summary
  output$dosing_summary <- renderPrint({
    total_dose <- input$dose * input$n_doses
    total_time <- input$start_time + (input$n_doses - 1) * input$interval
    daily_dose <- ifelse(input$interval > 0, (input$dose * 24) / input$interval, 0)
    
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("VANCOMYCIN DOSING PROTOCOL\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("• Drug: Vancomycin (Glycopeptide antibiotic)\n")
    cat("• Single dose:", input$dose, "mg\n")
    cat("• Interval: Every", input$interval, "hours\n")
    cat("• Total doses:", input$n_doses, "\n")
    cat("• Total amount:", total_dose, "mg\n")
    cat("• Daily dose (approx.):", round(daily_dose), "mg/day\n")
    cat("• First dose at:", input$start_time, "hours\n")
    cat("• Last dose at:", total_time, "hours\n")
    cat("• Simulation period:", input$sim_duration, "hours\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  })
  
  # Compile button
  observeEvent(input$compile, {
    if(!is.null(values$compile_start) && !values$compiled) {
      output$status <- renderPrint({
        "⚠️ Already compiling. Please wait..."
      })
      return()
    }
    
    values$compile_start <- Sys.time()
    
    isolate({
      withProgress(message = 'Compiling model...', 
                   detail = 'This takes 30-60 seconds', value = 0, {
        
        incProgress(0.2, detail = "Preparing ODE system...")
        
        tryCatch({
          setTimeLimit(cpu = 60, elapsed = 60, transient = TRUE)
          
          incProgress(0.3, detail = "Generating C code...")
          m1 <- rxode2(ode)
          
          incProgress(0.6, detail = "Calculating parameters...")
          theta <- calcNomParams()
          
          incProgress(0.9, detail = "Finalizing...")
          
          values$m1 <- m1
          values$theta <- theta
          values$compiled <- TRUE
          
          setTimeLimit(cpu = Inf, elapsed = Inf)
          
          output$status <- renderPrint({
            "✅ Model compilation successful! Ready for simulation."
          })
          
        }, error = function(e) {
          setTimeLimit(cpu = Inf, elapsed = Inf)
          values$compile_start <- NULL
          output$status <- renderPrint({
            paste("❌ Compilation failed:", e$message)
          })
        })
      })
    })
  })
  
  # Run simulation
  observeEvent(input$run, {
    if(!values$compiled) {
      output$status <- renderPrint({
        "⚠️ Please compile the model first!"
      })
      return()
    }
    
    withProgress(message = 'Running simulation...', value = 0, {
      
      tryCatch({
        incProgress(0.2, detail = "Setting initial conditions...")
        
        # Initial conditions
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
        
        incProgress(0.4, detail = "Setting dosing schedule...")
        
        # Dosing schedule
        ev1 <- eventTable(amount.units = "mg", time.units = "hours")
        
        # First dose
        if(input$n_doses >= 1) {
          ev1$add.dosing(dose = input$dose, 
                        start.time = input$start_time, 
                        nbr.doses = 1, 
                        cmt = "depot")
        }
        
        # Additional doses
        if(input$n_doses > 1) {
          ev1$add.dosing(dose = input$dose, 
                        start.time = input$start_time + input$interval, 
                        nbr.doses = input$n_doses - 1, 
                        dosing.interval = input$interval, 
                        cmt = "depot")
        }
        
        # Sampling times
        ev1$add.sampling(seq(0, input$sim_duration, by = 1))
        
        incProgress(0.6, detail = "Running simulation...")
        
        # Run simulation
        x <- values$m1$run(values$theta, ev1, inits = inits)
        values$last_results <- x  # Save results
        
        # Save simulation parameters
        values$simulation_params <- list(
          timestamp = Sys.time(),
          drug = "Vancomycin",
          dose = input$dose,
          interval = input$interval,
          n_doses = input$n_doses,
          start_time = input$start_time,
          sim_duration = input$sim_duration,
          total_dose = input$dose * input$n_doses
        )
        
        # Enable download
        values$simulation_complete <- TRUE
        
        incProgress(0.9, detail = "Generating plots...")
        
        # Generate plots
        output$plot <- renderPlot({
          par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
          
          # 1. Vancomycin concentration
          plot(x[,"time"], x[,"C_venous_vanco"], 
               xlab = "Time (hours)", ylab = "Concentration (mg/L)", 
               type = "l", col = "purple", lwd = 2,
               main = "Vancomycin Plasma Concentration",
               xlim = c(0, input$sim_duration))
          
          # Target range
          polygon(c(0, input$sim_duration, input$sim_duration, 0),
                  c(10, 10, 20, 20), col = rgb(0, 1, 0, 0.1), border = NA)
          abline(h = 10, col = "green", lty = 2)
          abline(h = 20, col = "green", lty = 2)
          abline(h = 40, col = "red", lty = 2)
          
          # Dose times
          dose_times <- input$start_time + (0:(input$n_doses-1)) * input$interval
          for(dt in dose_times) {
            if(dt <= input$sim_duration) {
              abline(v = dt, col = "gray", lty = 3, lwd = 0.5)
            }
          }
          
          legend("topright", 
                 legend = c("Vancomycin", "Target Range", "Toxic Level"), 
                 col = c("purple", "green", "red"), 
                 lty = c(1, 2, 2), 
                 cex = 0.7)
          grid()
          
          # 2. Bacterial Load - Semi-log scale
          cfu_data <- x[,"CFU_lung"]
          cfu_data[cfu_data <= 0] <- 1e-10
          
          plot(x[,"time"], cfu_data, 
               xlab = "Time (hours)", ylab = "CFU/mL (log scale)", 
               type = "l", col = "red", lwd = 2,
               main = "Bacterial Load in Lung",
               log = "y",
               xlim = c(0, input$sim_duration),
               ylim = c(max(1, min(cfu_data)), max(cfu_data) * 10))
          
          abline(h = 1e2, col = "orange", lty = 2)
          grid()
          
          # 3. Mean Arterial Pressure
          plot(x[,"time"], x[,"mean_arterial_pressure_MAP"], 
               xlab = "Time (hours)", ylab = "MAP (mmHg)", 
               type = "l", col = "blue", lwd = 2,
               main = "Mean Arterial Pressure",
               xlim = c(0, input$sim_duration))
          
          polygon(c(0, input$sim_duration, input$sim_duration, 0),
                  c(65, 65, 110, 110), col = rgb(0, 0, 1, 0.05), border = NA)
          abline(h = 65, col = "darkblue", lty = 2)
          abline(h = 110, col = "darkblue", lty = 2)
          grid()
          
          # 4. GFR
          plot(x[,"time"], x[,"GFR_ml_min"], 
               xlab = "Time (hours)", ylab = "GFR (mL/min)", 
               type = "l", col = "green", lwd = 2,
               main = "Glomerular Filtration Rate",
               xlim = c(0, input$sim_duration))
          
          abline(h = 90, col = "darkgreen", lty = 2)
          abline(h = 60, col = "orange", lty = 2)
          abline(h = 30, col = "red", lty = 2)
          
          legend("topright", 
                 legend = c("GFR", "Normal", "Stage 2", "Stage 4"), 
                 col = c("green", "darkgreen", "orange", "red"), 
                 lty = c(1, 2, 2, 2), 
                 cex = 0.6)
          grid()
        })
        
        # Calculate key metrics
        vanco_data <- x[,"C_venous_vanco"]
        dose_times <- input$start_time + (0:(input$n_doses-1)) * input$interval
        peaks <- c()
        troughs <- c()
        
        for(i in 1:length(dose_times)) {
          dose_time <- dose_times[i]
          if(i < length(dose_times)) {
            next_dose <- dose_times[i+1]
            peak_window <- which(x[,"time"] >= dose_time & x[,"time"] <= min(dose_time + 2, next_dose))
            if(length(peak_window) > 0) {
              peaks <- c(peaks, max(vanco_data[peak_window]))
            }
            trough_time <- which.min(abs(x[,"time"] - (next_dose - 0.5)))
            troughs <- c(troughs, vanco_data[trough_time])
          }
        }
        
        # Metrics table
        output$metrics_table <- renderTable({
          data.frame(
            Metric = c("Average Peak (mg/L)", 
                      "Average Trough (mg/L)",
                      "Max Concentration (mg/L)",
                      "Min CFU (log10)",
                      "Final MAP (mmHg)",
                      "Final GFR (mL/min)"),
            Value = c(ifelse(length(peaks) > 0, round(mean(peaks), 2), "N/A"),
                     ifelse(length(troughs) > 0, round(mean(troughs), 2), "N/A"),
                     round(max(vanco_data), 2),
                     round(log10(max(1, min(x[,"CFU_lung"]))), 2),
                     round(tail(x[,"mean_arterial_pressure_MAP"], 1), 1),
                     round(tail(x[,"GFR_ml_min"], 1), 1)),
            Status = c(ifelse(length(peaks) > 0 && mean(peaks) < 40, "✓", "⚠"),
                      ifelse(length(troughs) > 0 && mean(troughs) >= 10 && mean(troughs) <= 20, "✓", "⚠"),
                      ifelse(max(vanco_data) < 40, "✓", "⚠"),
                      ifelse(min(x[,"CFU_lung"]) < 1e3, "✓", "⚠"),
                      ifelse(tail(x[,"mean_arterial_pressure_MAP"], 1) >= 65 && 
                            tail(x[,"mean_arterial_pressure_MAP"], 1) <= 110, "✓", "⚠"),
                      ifelse(tail(x[,"GFR_ml_min"], 1) >= 60, "✓", "⚠"))
          )
        }, align = "lcl")
        
        output$status <- renderPrint({ 
          paste("✅ Simulation complete!", 
                "\n💊 Vancomycin:", input$dose, "mg x", input$n_doses, "doses",
                "\n⏰ Interval:", input$interval, "hours",
                "\n📅 Duration:", input$sim_duration, "hours",
                "\n📊 Results ready. You can download the data.") 
        })
        
      }, error = function(e) {
        output$status <- renderPrint({
          paste("❌ Simulation error:", e$message)
        })
      })
    })
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("vancomycin_simulation_", 
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      if(!is.null(values$last_results) && !is.null(values$simulation_params)) {
        results_df <- as.data.frame(values$last_results)
        
        header_info <- paste0(
          "# Vancomycin Simulation Results\n",
          "# Generated: ", values$simulation_params$timestamp, "\n",
          "# Drug: ", values$simulation_params$drug, "\n",
          "# Dose: ", values$simulation_params$dose, " mg\n",
          "# Interval: ", values$simulation_params$interval, " hours\n",
          "# Number of doses: ", values$simulation_params$n_doses, "\n",
          "# Total dose: ", values$simulation_params$total_dose, " mg\n",
          "# First dose at: ", values$simulation_params$start_time, " hours\n",
          "# Simulation duration: ", values$simulation_params$sim_duration, " hours\n",
          "#\n"
        )
        
        writeLines(header_info, file)
        write.table(results_df, file, append = TRUE, sep = ",", 
                   row.names = FALSE, quote = FALSE)
      }
    }
  )
  
  # Auto update
  observe({
    if(!is.null(values$compile_start) && !values$compiled) {
      invalidateLater(1000)
    }
  })
}

# Run app
shinyApp(ui = ui, server = server)

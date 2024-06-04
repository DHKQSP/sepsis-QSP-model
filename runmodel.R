library(rxode2)

source("modelfile_0822.R")
load("model_struct.saved")

m1 <- rxode2(model = ode, modname = mod1 )

source("calcNomParams_human_0602.R") 
theta=calcNomParams()




###Initial conditions - do NOT change order!!!
#Order must match order in model file
#labels are not used by RxODE to match init to compartment
inits <- c(AngI=8.164, AngII=5.17,AT1_bound_AngII=16.6, AT2_bound_AngII = 5.5, plasma_renin_concentration=17.845,
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
           CFU_lung = 3.5e8,	CFU_blood=0, R_lung=1e6 , R_blood=0 ,CINC1 = 30, Lag1=0, Lag2=0, NS1 = 0, NS2 = 0, AC=48,
           AT2=3e+10, AT1=2e+10, dAT1=250.26, dAT2=375.4,
           pDC=0,pDC1=0, M1=0, Th1=0, Th17=0, Treg=0, N = 2455.6, TNFa = 0.00024335, IL6=0.00014131,
           IL1b =0.028005 , IL2 = 0.35115, IL12 = 0, IL17=1.13E-05, IL10=0,TGFb=0,GMCSF=0,
           TNFa_c = 1.54E-09  , IL6_c=6.42E-08, IL1b_c=1.27E-05, IL2_c=7.34E-06, IL12_c=8.93E-06, IL17_c=0, IL10_c=9.72E-07, TGFb_c=8.11E-07, GMCSF_c=4.04E-06,
           pDC_c = 0.70096, M1_c =0.92091, N_c = 0.48093, Th1_c = 0.7964, Th17_c = 0.00017499, Treg_c = 0.21768,
           C_venous_vanco = 0, C_vas_ki_vanco = 0,   C_vas_lu_vanco = 0, C_vas_ad_vanco = 0,  C_vas_bo_vanco = 0, C_vas_go_vanco = 0,
           C_vas_he_vanco = 0, C_vas_mu_vanco = 0, C_vas_sk_vanco = 0, C_vas_br_vanco = 0, C_vas_li_vanco = 0,  C_vas_re_vanco = 0,
           C_vas_gu_vanco = 0, C_vas_sp_vanco = 0, C_vas_pa_vanco = 0, C_arterial_vanco = 0  
           )

#calculate steady state condition

ev1<- eventTable(amount.units = "mg", time.units = "hours")
ev1$add.dosing(dose=1000, start.time = 12, nbr.doses = 1, cmt="depot")
ev1$add.dosing(dose=1000, start.time = 24, nbr.doses = 27,dosing.interval = 12, cmt="depot")

ev1$add.sampling(seq(0,1000))
x <- m1$run(theta, ev1, inits=inits)

par(mfrow=c(2,4))
plot(x[,"mean_arterial_pressure_MAP"],xlab="time(h)",ylab="MAP (mmHg)",cex.lab = 1.5,type="l")
plot(x[,"GFR_ml_min"],xlab="time(h)",ylab="GFR (ml/min)",type="l",cex.lab = 1.5)
plot(x[,"serum_creatinine_concentration"],xlab="time (h)",ylab="Scr(mg/dL)",type="l",cex.lab = 1.5)
plot(x[,"time"], x[,"CFU_lung"],xlab="time(h)",ylab="CFU Lung",cex.lab = 1.5,type="l")
plot(x[,"cardiac_output"],xlab="time(h)",ylab="cardiac_output",cex.lab = 1.5,type="l")
plot(x[,"Q_kidney"], xlab="time(h)",ylab="Q_kidney",cex.lab = 1.5,type="l")
plot(x[,"CL_renal_vanco"], xlab="time(h)",ylab="CL_renal_vanco",cex.lab = 1.5,type="l")
plot(x[,"C_vas_ki_vanco"], xlab="time(h)",ylab="C_vas_ki_vanco",cex.lab = 1.5,type="l")
par(mfrow=c(4,4))
plot(x[,"IL1b_pg_mL"],xlab="time(h)",ylab="Lung IL1b (pg/mL)",cex.lab = 1.3,type="l",)
plot(x[,"IL1b_c_pg_mL"],xlab="time(h)",ylab="plasma IL1b (pg/mL)",cex.lab = 1.3,type="l")
plot(x[,"IL6_pg_mL"],xlab="time(h)",ylab="Lung IL6 (pg/mL)",cex.lab = 1.3,type="l",)
plot(x[,"IL6_c_pg_mL"],xlab="time(h)",ylab="plasma IL6 (pg/mL)",cex.lab = 1.3,type="l")
plot(x[,"IL10_pg_mL"],xlab="time(h)",ylab="Lung IL10 (pg/mL)",cex.lab = 1.3,type="l",)
plot(x[,"IL10_c_pg_mL"],xlab="time(h)",ylab="plasma IL10 (pg/mL)",cex.lab = 1.3,type="l")
plot(x[,"TGFb_pg_mL"],xlab="time(h)",ylab="Lung TGFb (pg/mL)",cex.lab = 1.3,type="l",)
plot(x[,"TGFb_c_pg_mL"],xlab="time(h)",ylab="plasma TGFb(pg/mL)",cex.lab = 1.3,type="l")
plot(x[,"CRP_mg_dl"],xlab="time(h)",ylab="plasma CRP(mg/dL)",cex.lab = 1.3,type="l")
plot(x[,"TNFa_pg_mL"],xlab="time(h)",ylab="Lung TNFa (pg/mL)",cex.lab = 1.3,type="l",)
plot(x[,"TNFa_c_pg_mL"],xlab="time(h)",ylab="plasma TNfa (pg/mL)",cex.lab = 1.3,type="l")
plot(x[,"IL12"],xlab="time(h)",ylab="Lung IL12 (pg/mL)",cex.lab = 1.3,type="l",)
plot(x[,"nom_RVR"],xlab="time(h)",ylab="RVR",cex.lab = 1.3,type="l")
plot(x[,"GMCSF"],xlab="time(h)",ylab="GMCSF",cex.lab = 1.3,type="l")
plot(x[,"dAT1"],xlab="time(h)",ylab="dAT1",cex.lab = 1.3,type="l")
plot(x[,"dAT2"],xlab="time(h)",ylab="dAT2",cex.lab = 1.3,type="l")

par(mfrow=c(2,2))

plot(x[,"time"], x[,"C_venous_vanco"],xlab="time(h)",ylab="plasma vanco(mg/L)",cex.lab = 1.5,type="l")
plot(x[,"C_vas_lu_vanco"],xlab="time(h)",ylab="Lung vanco(mg/L)",cex.lab = 1.5,type="l")

par(mfrow=c(4,3))
plot(x[,"N_c"],xlab="time(h)",ylab="plasma N",cex.lab = 1.5,type="l")
plot(x[,"N"],xlab="time(h)",ylab="Lung N",cex.lab = 1.5,type="l")
plot(x[,"pDC_c"],xlab="time(h)",ylab="plasma DC",cex.lab = 1.5,type="l")
plot(x[,"pDC"],xlab="time(h)",ylab="Lung DC",cex.lab = 1.5,type="l")
plot(x[,"M1_c"],xlab="time(h)",ylab="plasma M1",cex.lab = 1.5,type="l")
plot(x[,"M1"],xlab="time(h)",ylab="Lung M1",cex.lab = 1.5,type="l")
plot(x[,"Th1_c"],xlab="time(h)",ylab="plasma Th1",cex.lab = 1.5,type="l")
plot(x[,"Th1"],xlab="time(h)",ylab="Lung Th1",cex.lab = 1.5,type="l")
plot(x[,"Th17_c"],xlab="time(h)",ylab="plasma Th17",cex.lab = 1.5,type="l")
plot(x[,"Th17"],xlab="time(h)",ylab="Lung Th17",cex.lab = 1.5,type="l")
plot(x[,"Treg_c"],xlab="time(h)",ylab="plasma Treg",cex.lab = 1.5,type="l")
plot(x[,"Treg"],xlab="time(h)",ylab="Lung Treg",cex.lab = 1.5,type="l")


calcNomParams <- function(){
  
  
  
  ########################################################################################
  #Parameters of normal human physiology based on literature and commmon medical knowledge
  ########################################################################################
  
  ####Systemic parameters
  nominal_map_setpoint=93  		#mmHg
  CO_nom= 5					#L/min
  ECF_nom = 15				#L
  blood_volume_nom = 5			#L
  Na_intake_rate=100/24/60		#mEq/min  - 100mmol/day or 2300 mg/day
  nom_water_intake = 2.1			#L/day
  ref_Na_concentration=140 	   	#mEq/L
  plasma_protein_concentration = 7   	#g/dl
  equilibrium_serum_creatinine=0.92	#mg/dl
  P_venous=4					#mmHg
  R_venous=3.4				#mmHg
  nom_right_atrial_pressure=0.87 	#mmHg
  nom_mean_filling_pressure=7 		#mmHg
  venous_compliance = 0.13
  
  
  
  ####Renal parameters
  nom_renal_blood_flow_L_min = 1	#L/min
  baseline_nephrons=2e6
  nom_Kf=3.9					#nl/min*mmHg
  nom_oncotic_pressure_difference= 28 #mmHg
  P_renal_vein=4  				#mmHg
  
  #Renal Vasculature
  nom_preafferent_arteriole_resistance= 19 	#mmHg
  
  
  #Renal Tubules
  Dc_pt_nom  = 27e-6			#m
  Dc_lh = 17e-6				#m
  Dc_dt = 17e-6				#m
  Dc_cd = 22e-6				#m
  
  L_pt_s1_nom = 0.005			#m
  L_pt_s2_nom = 0.005			#m
  L_pt_s3_nom =0.004			#m
  L_lh_des = 0.01 				#m
  L_lh_asc = 0.01 				#m
  L_dct = 0.005				#m	
  L_cd = L_lh_des	
  
  tubular_compliance = 0.2	
  Pc_pt_mmHg = 14				#mmHg
  Pc_lh_des_mmHg = 10.5			#mmHg
  Pc_lh_asc_mmHg = 7			#mmHg
  Pc_dt_mmHg = 3				#mmHg
  Pc_cd_mmHg = 2				#mmHg
  P_interstitial_mmHg = 5
  nominal_pt_na_reabsorption=0.7	#fraction
  nominal_loh_na_reabsorption = 0.8	#fraction
  nominal_dt_na_reabsorption=0.5	#fraction
  LoH_flow_dependence = 1
  
  
  ####RAAS Pathway parameters
  concentration_to_renin_activity_conversion_plasma = 61 
  nominal_equilibrium_PRA = 1000 	 	#fmol/ml/hr
  nominal_equilibrium_AngI = 7.5 		#fmol/ml
  nominal_equilibrium_AngII = 4.75 		#fmol/ml
  nominal_renin_half_life = 0.1733		# (hr)
  nominal_AngI_half_life = 0.5/60 		#(hr)
  nominal_AngII_half_life = 0.66/60 		#(hr)
  nominal_AT1_bound_AngII_half_life = 12/60 #hr
  nominal_AT2_bound_AngII_half_life = 12/60 #hr
  ACE_chymase_fraction = 0.95     		#% of AngI converted by ACE. The rest is converted by chymase
  fraction_AT1_bound_AngII = 0.75    		#assume AngII preferentially binds to AT1 vs AT2
  
  
  ########################################################################################
  #The following parameters were determined indirectly from many different literature studies on the response
  #various changes in the system (e.g. drug treatments, infusions of peptide, fluid, sodium, etc.....)
  ########################################################################################
  
  #Effects of AT1-bound AngII on preafferent, afferent, and efferent resistance, and aldosterone secretion
  AT1_svr_slope = 0
  AT1_preaff_scale = 0.5
  AT1_preaff_slope = 7 
  AT1_aff_scale=0.5
  AT1_aff_slope=7
  AT1_eff_scale=0.3
  AT1_eff_slope=7
  AT1_PT_scale = 0.1
  AT1_PT_slope = 7
  AT1_aldo_slope = 0.05
  
  
  #Effects of Aldosterone on distal and collecting duct sodium reabsorption
  nominal_aldosterone_concentration=85
  aldo_DCT_scale=0
  aldo_DCT_slope = 0.5
  aldo_CD_scale=0.3
  aldo_CD_slope = 0.5
  aldo_renin_slope =-0.05
  
  #Na and water transfer between blood, ECF
  Q_water = 1
  Q_Na = 1
  
  #Osmolarity control of vasopressin secretion
  Na_controller_gain=0.1  
  Kp_VP = 0.05
  Ki_VP = 0.00002
  
  nom_ADH_urea_permeability = .98
  nom_ADH_water_permeability = .98
  
  #Effects of Vasopressin on water intake and reabsorption
  nominal_vasopressin_conc=4
  water_intake_vasopressin_scale = 0  #1.5
  water_intake_vasopressin_slope = -0.5
  
  
  #Magnitude and Steepness of tubuloglomerular feedback
  S_tubulo_glomerular_feedback=0.7
  F_md_scale_tubulo_glomerular_feedback=6
  MD_Na_concentration_setpoint = 62.4
  
  #Effect of macula densa sodium flow on renin secretion 
  md_renin_A = 1
  md_renin_tau = 2
  
  #Responsiveness of renal vasculature to regulatory signals
  preaff_diameter_range=0.25
  afferent_diameter_range=1.2e-05 
  efferent_diameter_range=3e-06 
  preaff_signal_nonlin_scale=3
  afferent_signal_nonlin_scale=3
  efferent_signal_nonlin_scale=3
  
  #RAAS pathway (these parameters can be set to different values than used to calculate the equilibrium state above)
  AngI_half_life=0.008333 
  AngII_half_life=0.011 
  AT1_bound_AngII_half_life=0.2 
  AT1_PRC_slope=-1.2 
  AT1_PRC_yint=0
  AT2_bound_AngII_half_life=0.2
  concentration_to_renin_activity_conversion_plasma=61
  fraction_AT1_bound_AngII=0.75
  nominal_ACE_activity=48.9
  nominal_AT1_receptor_binding_rate=12.1
  nominal_AT2_receptor_binding_rate=4.0 
  nominal_chymase_activity=1.25  
  nominal_equilibrium_AT1_bound_AngII=16.63
  nominal_equilibrium_PRC=16.4 
  renin_half_life=0.1733 
  
  #Transfer constants for ODEs - determine speed of processes
  C_aldo_secretion=1000
  C_P_bowmans = 1000
  C_P_oncotic = 1000
  
  C_tgf_reset=0
  C_cardiac_output_delayed=.001
  C_co_error=0.00001
  C_vasopressin_delay = 1
  
  C_md_flow = 0.001 #Time delay between MD sodium flow and renin secretion
  C_tgf=1
  C_na_excretion_na_amount=-1
  C_na_intake_na_amount=1
  C_urine_flow_ecf_volume=-1
  C_water_intake_ecf_volume=1
  C_Na_error=1/6
  C_serum_creatinine = 1
  
  
  
  #Therapy effects
  HCTZ_effect_on_DT_Na_reabs = 1 
  HCTZ_effect_on_renin_secretion = 1
  DRI_effect_on_PRA = 1
  CCB_effect_on_preafferent_resistance = 1
  CCB_effect_on_afferent_resistance = 1
  CCB_effect_on_efferent_resistance = 1
  MR_antagonist_effect_on_aldo_MR = 1
  pct_target_inhibition_ARB = 0 
  pct_target_inhibition_ACEi = 0 
  

  
  
  #Metabololic tissue autoregulation of cardiac output
  tissue_autoreg_scale=1
  Kp_CO=1.5
  Ki_CO=30
  
  
  #Normalized_aldo_secretion
  K_Na_ratio_effect_on_aldo = 1; 
  
  #Renal autoregulation of glomerular pressure
  gp_autoreg_scale=0
  preaff_autoreg_scale = 0.5
  myogenic_steepness=2
  
  
  #Pressure natiuresis effect 
  
  max_pt_reabs_rate = 0.995
  pressure_natriuresis_PT_scale = 3
  pressure_natriuresis_PT_slope = 1
  
  pressure_natriuresis_LoH_scale = 3
  pressure_natriuresis_LoH_slope = 1
  
  pressure_natriuresis_DCT_scale = 3
  pressure_natriuresis_DCT_slope = 1
  
  max_cd_reabs_rate = 0.995
  pressure_natriuresis_CD_scale = 3
  pressure_natriuresis_CD_slope=1
  
  #############################################infection model########################################################
  
  kg=1.46    #h^-1, First order rate constant for net bacterial growth
  k_dN=10^-7.71	#cells^-1*h^-1, Second order rate constant for bacterial killing by N
  k_dNS1=10^-7.48	#cells^-1*h^-1, Second order rate constant for bacterial killing by NS1
  k_dNS2=10^-7.00	#cells^-1*h^-1, Second order rate constant for bacterial killing by NS2
  

  k_death = 0.187  #Nielsen EI et al.(2011)
  Bmax = 5.00e8   #Nielsen EI et al.(2011)
  
  Emax_vanco = 1.52*4.6 #h^-1
  EC50_vanco = 0.304 #mg/L
  gamma_vanco = 4.99
  

  I_AC_IL1b = 6.48e-4		#mL/pg, Scaling factor for IL-1b inhibition by AC
  I_AC_TNFa = 0.02		#mL/pg, Scaling factor for TNFa inhibition by AC
  
  Smax_CFU_IL1b= 94.7*5*20		#Capacity constant for CFU stimulating IL-1b
  SC50_CFU_IL1b= 10^8.89	#cfu/mL, Sensitivity constant for CFU stimulating IL-1b (LOW/HIGH INOCULATION)
  Smax_CFU_TNFa= 64.2*20*1000		#Capacity constant for CFU stimulating TNFa
  SC50_CFU_TNFa= 10^8.20 #cfu/mL, Sensitivity constant for CFU stimulating TNFa (LOW/HIGH INOCULATION)
  Smax_CFU_AC = 57.9		#Capacity constant for CFU stimulating TNF-a
  SC50_CFU_AC= 10^8.92 	#cfu/mL LOW/HIGH INOCULATION
  
  ######First order loss rate constant
  k_out_IL1b=0.122			#h^-1
  k_out_TNFa = 0.205		#h^-1
  k_out_AC = 0.101			#h^-1
  k_out_CINC1 = 1.75		#h^-1
  k_out_N = 1.12			  #h^-1
  
  kt_lag = 0.044			#h^-1, First order transit rate for N recruitment delay by CINC-1
  k_t_NS = 6.36E-3			#h^-1, First order transit rate for NS
  
  k_in_IL1b = 48*k_out_IL1b	#see model article
  k_in_TNFa = 20*k_out_TNFa	#see model article
  k_in_AC = 48*k_out_AC		#see model article
  k_in_N = 6.61E+5		#cells/h, Production rate constant for N
  
  
  #####scaling factor
  S_TNFa = 4.12			#mL/pg, Scaling factor for CINC-1 stimulation by TNF-a
  S_IL1b_N = 6.54E-3	#mL/pg, Scaling factor for N stimulation by IL-1b(low/high inoc)
  S_CINC1 = 4.93E-4		#mL/pg, Scaling factor for N stimulation by CINC-1
  
  
  ################################################################################################################
  
  
  
  
  #Constants and Unit conversions
  nL_mL=1e+06
  dl_ml=0.01
  L_dL=10
  L_mL=1000
  L_m3=0.001
  g_mg=0.001
  ng_mg=1e-06
  secs_mins=60
  min_hr=60
  hr_day=24
  min_day=1440
  MW_creatinine=113.12
  Pi=3.1416
  pi=3.14
  viscosity_length_constant=1.5e-09 
  gamma =  1.16667e-5;  #  viscosity of tubular fluid
  mmHg_Nperm2_conv = 133.32
  
  
  #Scaling parameters - can be used to parameterize model for other species
  ECF_scale_species = 1
  BV_scale_species=1
  water_intake_species_scale = 1
  CO_scale_species = 1
  
  ################FROM SARS-COV2 MODEL###############################
  k_DC_TNFa = 10*100		#rate constant for maturation of dendritic cells by TNF-???
  km_DC_TNFa = 3193.344 	#EC50 for maturation of dendritic cells by TNF-???
  k_DC_IFNg = 10*100		#rate constant for maturation of dendritic cells by IFN????
  km_DC_IFNg = 1064.448	#EC50 for maturation of dendritic cells by IFN????
  k_DC_IL6= 10*100		#rate constant for maturation of dendritic cells by IL-6
  km_DC_IL6= 139.104	#EC50 for maturation of dendritic cells by IL-6
  km_DC_IL10= 46.2672	#IC50 for inhibition of DC activation by IL-10
  a_DC= 690000*100	#rate constant for production of mature dendritic cells
  kbasal_DC= 0 + 0.00000001		#basal activation of dendritic cells
  b_DC= 0.005208333*100	#death rate for mature dendiritic cells
  
  k_M1_GMCSF= 10		#rate constant for macrophage activation by GM-CSF
  km_M1_GMCSF= 567.3024	#EC50 for macrophage activation by GM-CSF
 
  k_I= 0.0003		#rate constant for innate immune activation by infected cells
  k_dAT= 0.00003		#rate constant for innate immune activation by damaged cells 
  km_dAT= 5000000000	#EC50 for innate immune activation by damaged cells
  
  a_M1=2500000		#rate constant for activation of macrophages 
  b_M1=0.002791667*5	#death rate of activated macrophages
  kbasal_M1=0+ 0.001		#rate constant for basal activation of macrophages 
  
  k_M1_TNFa=1000		#rate constant for macrophage activation by TNF
  km_M1_TNFa=3200.6016	#EC50 for macrophage activation by TNF
  k_M1_GMCSF=10		#rate constant for macrophage activation by GM-CSF
  km_M1_GMCSF=567.3024	#EC50 for macrophage activation by GM-CSF
  k_M1_IFNg=10		#rate constant for macrophage activation by IFNg
  km_M1_IFNg=800.1504	#EC50 for macrophage activation by IFNg
  km_M1_IL10=167.40864	#IC50 for inhibition of macrophage activation by IL-10
  
  k_N_IFNg=10		#rate constant for neutrophil activation by IFN
  km_N_IFNg=53222.4	#EC50 for neutrophil activation by IFN
  k_N_TNFa=0.1		#rate constant for neutrophil activation by TNF
  km_N_TNFa=5261.76	#EC50 for neutrophil activation by TNF
  k_N_GMCSF=1		#rate constant for neutrophil activation by GM-CSF
  km_N_GMCSF=725.76	#EC50 for neutrophil activation by GM-CSF
  k_N_IL17c=10		#rate constant for neutrophil recruitment by IL-17
  km_N_IL17c=2237.76	#EC50 for neutrophil recruitment by IL-17
  b_N=1.12 #death rate of activated neutrophils
  a_N= 	79354300000#rate constant for activation of neutrophils
  kbasal_N=	.0004		#basal recruitment of activated neutrophils
  
  a_Th1=0.004*200000		#rate constant for activation of Th1 cells
  b_Th1=0.019629167	#death rate for activated Th1 cells
  k_Th1_IL2	=1.5		#rate constant for activation of by Th1 cells by IL-2
  K_Th1_IL12=0.501984	#EC50 for activation of Th1 cells by IL-2
  k_Th1_IL12IL2=2		#rate constant for induction of IL-2 activity for activation of Th1 cells by IL-12
  K_Th1_IL12IL2=7.52976	#EC50 for activation of Th1 cells by IL-12
  K_Th1_IL10=32516.12903	#IC50 for inhibition of activated Th1 cells by IL-10
  K_Th1_TGFb=1832727.273	#IC50 for inhibition for activated Th1 cells by TGF
  k_Th1_IFNg=1		#rate constant for activation of Th1 cells by IFN
  K_IFNg_Th1=931.392	#EC50 for activation of Th1 cells by IFN
  K_Th1_IL6	=267610.6195	#EC50 for inhibition of Th1 activation by IL-6
  k_Th1_Th17=0.01	#rate constant for differentiation of activated Th17 cells to Th1 cells
  K_Th1_Th17=0.0328608	#EC50 for differentiation of activated Th17 cells to Th1 cells
  k_Th1_Treg=0.005		#rate constant for differentiation of activated Treg cells to Th1 cells
  K_Th1_Treg=20.92608	#EC50 for differentiation of activated Treg cells to Th1 cells
  
  a_Th17=0.00692*200000		#rate constant for activation of Th17 cells 
  b_Th17=0.0216125		#death rate of activated Th17 cells
  k_Th17_TGFb=4		#rate constant for activation of Th17 cells by TGF
  K_Th17_TGFb=638.372448	#EC50 for activation of Th17 cells by TGF
  K_Th17_IL2=7032558.14	#IC50 for inhibition of Th17 activation by IL-2
  K_Th17_IFNg=6833898.305	#IC50 for inhibition of Th17 activation by IFN
  K_Th17_IL10=1359101.124	#IC50 for inhibition of Th17 activation by IL-10
  k_Th17_IL6=1*100		#rate constant for Th17 activation by IL-6
  km_Th17_IL6=42.336	#EC50 for Th17 activation by IL-6
  k_Th17_IL1b=1	#rate constant for Th17 activation by IL-1
  km_Th17_IL1b=20728.85472	#EC50 for Th17 activation by IL-1
 
  a_Treg=0.004*1000000		#rate constant for Treg activation
  b_Treg=0.014791667*5	#death of activated Treg cells
  k_Treg_IL2=1*100		#rate constant for Treg activation by IL-2
  K_Treg_IL2=1.796256	#EC50 for Treg activation by IL-2
  K_Treg_IL17=491707.3171	#IC50 constant for Treg inhibition by IL-17
  K_Treg_IL6=535221.2389	#IC50 constant for Treg inibition by IL-6
  k_Treg_TGFb=1		#rate constant for Treg activation by TGF
  K_Treg_TGFb=32.256	#EC50 for Treg activation by TGF
 
  
  a_tnf=0.06487992	#basal induction rate of TNF
  a_tnf_m1=2.10E-04		#production rate of TNF by activated macrophages
  a_tnf_th1=1.13E-09	#production of TNF by activated Th1 cells
  a_tnf_th17=1.13E-09 #production of TNF by activated Th17 cells
  a_tnf_at1=	0.00014	#production rate of TNF by damaged AT1 cells	#1/cell
  a_tnf_i	=1.40E-05	#production rate of TNF by infected AT2 cells	#1/cell
  a_tnf_at2	=0.00014	#production rate of TNF by damaged AT2 cells	#1/cell
  b_tnf=3.16*10		#clearance rate of TNF
  
  a_il6=0.0169344*500	#basal induction rate of IL-6
  a_il6_m1=2.90E-04		#production rate of IL-6 by activated macrophages
  a_il6_th17=2.50E-04	#production of IL-6 by activated Th17 cells
  a_il6_neu=1.08E-08		#production  of IL-6 by activated neutrophils
  a_il6_at1=	3.12E-06*100 #production rate of IL-6 by damaged AT1 cells	#1/cell
  a_il6_i=	6.24E-07	#production rate of IL-6 by infected AT2 cells	#1/cell
  a_il6_at2=	3.12E-06*1000	#production rate of IL-6 by damaged AT2 cells	#1/cell
  b_il6=1.32*1000		#clearance rate of IL-6
  
  
  a_il1b = 0.84672	#basal induction rate of IL-1
  a_il1b_m1 = 2.07E-08	#production rate of IL-1 by activated macrophages
  a_il1b_dc = 1.035E-07		#production rate of IL-1 by mature dendritic cells
  a_il1b_at1 =	5.17E-10	#production rate of IL-1 by damaged AT1 cells	#1/cell
  a_il1b_i	= 2.07E-09	#production rate of IL-1 by infected AT1 cells	#1/cell
  a_il1b_at2	= 2.59E-09	#production rate of IL-1 by damaged AT2 cells	#1/cell
  b_il1b = 0.088*60	#clearance rate of IL-1
  
  
  a_ifng_dc=2.17E-09		#production of IFN by mature dendritic cells
  a_ifng_th1=6.15E-07	#production of IFN by activated Th1 cells
  a_ifng=0.126979181	#basal induction rate of IFN
  b_ifng=3.59		#clearance rate of IFN
  
  a_il2_dc=5.60E-08		#production of IL-2 by mature dendritic cells
  a_il2_th1=2.00E-08		#production of IL-2 by activated Th1 cells
  b_il2=1.913		#clearance rate of IL-2
  a_il2=25.96608		#basal induction rate of IL-2
  a_il12=48.26304		#basal induction of IL-12
  a_il12_m1=6.18E-08*10000		#production rate of IL-2 by activated macrophages
  a_il12_dc=4.98E-08*10000		#production rate of IL-2 by mature dendritic cells
  b_il12=2.73		#clearance rate of IL-12
  a_il17_th17=1.05E-05	#production rate of IL-17 by activated Th17 cells
  
  b_il17=0.0825		#clearance rate of IL-17
  a_il17=0.12997152		#basal induction rate of IL-17
  
  a_il10_treg=1.15E-05*200	#production rate of IL-10 by Treg cells
  b_il10=1.22		#clearance rate of IL-10 by Treg cells
  a_il10=0.272538		#basal induction rate of IL-10 
  
  a_tgfb_th17=7.42E-06*100	#production rate of TGF by activated Th17 cells
  a_tgfb_treg=7.15E-06*100	#production rate of TGF by activated Treg cells
  b_tgfb=1.9		#clearance rate of TGF
  a_tgfb=2.1506688		#basal induction rate of TGF
  
  a_gmcsf_m1=1.27E-06*1000	#production rate of GM-CSF by activated macrophages
  a_gmcsf_th1=4.29E-08*1000	#production rate of GM-CSF by activated Th1 cells
  a_gmcsf_th17= 1.15E-07*1000	#production ratio of GM-CSF by activated Th17 cells
  b_gmcsf= 1		#clearance rate of GM-CSF
  a_gmcsf= 15.5232		#basal induction of GM-CSF
  
  tau_Ab= 3960		#time scale for Ab production
  a_Ab= 1000000		#rate constant for Ab production
  b_Ab= 0.00297619		#degradation of Ab production

  
  
  ktr_TNFa= 0.1	#transport rate of TNF from alveolar to plasma compartment
  ktr_IL6= 0.1	#transport rate of IL-6 from alveolar to plasma compartment
  ktr_IL1b=0.1	#transport rate of IL-1 from alveolar to plasma compartment
  ktr_IFNb=0.1	#transport rate of Type I IFN from alveolar to plasma compartment
  ktr_IFNg=0.1	#transport rate of IFN from alveolar to plasma compartment
  ktr_IL2= 0.2	#transport rate of IL-2 from alveolar to plasma compartment
  ktr_IL12=0.1	#transport rate of IL-12 from alveolar to plasma compartment
  ktr_IL17=0.1	#transport rate of IL-17 from alveolar to plasma compartment
  ktr_IL10=0.1	#transport rate of IL-10 from alveolar to plasma compartment
  ktr_TGFb=0.1	#transport rate of TGF from alveolar to plasma compartment
  ktr_GMCSF=0.1	#transport rate of GM-CSF from alveolar to plasma compartment
  ktr_pDC=1	#transport rate of activated dendritic cells from alveolar to plasma compartment
  ktr_M1=1		#transport rate of activated macrophages from alveolar to plasma compartment
  ktr_N=1	#transport rate of activated neutrophils from alveolar to plasma compartment
  ktr_Th1=1	#transport rate of activated Th1 cells from alveolar to plasma compartment
  ktr_Th17=1	#transport rate of activated Th17 cells from alveolar to plasma compartment
  ktr_Treg=1	#transport rate of activated Treg cells from alveolar to plasma compartment
  ktr_CFU=1
  
  basal_tnfa=0.1		#basal production rate of TNF
  basalil6=40		#basal production rate of IL-6
  basalil1=0.1	#basal production rate of IL-1
  basalifng=4		#basal production rate of IL-1
  basalifnb	=0		#basal production of Type I IFN
  basalil2=0		#basal production rate of IL-2
  basalil12=0		#basal production of IL-12
  basalil10=0		#basal production rate of IL-10 
  basaltgfb=0		#basal production rate of TGF
  basalgmcsf=0		#basal production of GM-CSF
  
  mu_AT2 = 0.000267417	#rate constant for basal regeneration of AT2	#1/hr
  k_ROS_AT2 = 3.20E-07	#rate constant for ROS-induced damage of healthy cells	#1/hr
  km_ROS_AT2 = 50000000000	#IC50 for ROS-induced damage of healthy cells	#cells
  k_AT1_AT2	= 0.000106967	#rate constant for basal differentiation of AT2 to AT1	#1/hr
  km_AT1_AT2	= 0.9	#km for differentiation of AT2 to AT1	#dimensionless
  k_IFNb_kill =	1.1	#rate constant for induction of CD8+ infected cell clearance by Type I IFN	dimensionless
  k_kill = 1.84E-09	#rate constant for infected cell clearance by CD8+ cell clearance	cells #/hr
  km_kill =	500000	#IC50 for induction of CD8+ infected cell clearance by Type I IFN	#cells
  b_AT2	= 0.00016045	#death rate for AT2 cells	#1/hr
  
  mu_AT1=0	#rate constant for regerenation for AT1 cells (options)	#1/hr
  k_ROS_AT1	=1	#rate constant for ROS-induced damage of healthy AT1 cells	#1/hr
  km_ROS_AT1=	1	#EC50 for ROS-induced damage of healthy AT1 cells	#cells
  b_AT1	=0.00016045	#death rate for AT1 cells	#1/hr
  b_dAT1=	0.05	#death rate for damaged AT1 cells	#1/hr
  k_damage_TNFa	=0.01	#rate constant for TNFa induced damage	#dimensionless
  km_damage_TNFa=	319334.4	#IC50 for TNFa induced damage	#pmol
  k_damage_IL6=	0.01	#rate constant for IL-6 induced damage	#dimensionless
  km_damage_IL6=	11592	#IC50 for IL-6 induced damage	#pmol
  k_damage_IL1b=	0.01	#rate constant for IL-1 induced damage	#dimensionless
  km_damage_IL1b=	1727404.56	#IC50 for IL-1b induced damage	#pmol
  k_damage_IFNg=	0.01	#rate constant for IFNr induced damage	#dimensionless
  km_damage_IFNg=	53222.4	#IC50 for IFNr induced damage #pmol
  k_damage_cyt=	0.05	#rate constant overall cytokine damage#	#dimensionless
  k_mu_AT2=	30 #rate constant for threshold regeneration of AT2	#dimensionless
  k_diff_AT1=30	#rate constant for threshold differentiation of AT2 to AT1	#dimensionless
  
  
  ############ important lung variables
  n_alveolar = 480e6; #480 million alveolars per lung on average 274-790 million; coefficient of variation: 37% (https://www.researchgate.net/publication/9078564_The_Number_of_Alveoli_in_the_Human_Lung)
  size_alveolar = 4.2e6; #um^3, note 1 um^3 = 1e-15 L
  conversion_um3toL = 1e-15;
  vol_alv = n_alveolar*size_alveolar*conversion_um3toL;#2.016
  pulmonary_epithelial_cells = 5e10;# assumption, range used in other models
  AT1_total = 0.40 * pulmonary_epithelial_cells;
  AT2_total = 0.60 * pulmonary_epithelial_cells;
  
  #baseline concentrations of cells
  AT1_0 = AT1_total/vol_alv;
  AT2_0 = AT2_total/vol_alv;
  
  vol_alv_ml = vol_alv * 1000;
  vol_plasma = 5000; # mL
  
  #molecular weights g/mol. Da
  mw.il6 = 21000;
  mw.il2 = 15500;
  mw.il1b = 17500;
  mw.il10 = 18000;
  mw.il12 = 70000;
  mw.il17 = 35000;
  mw.tnfa = 25900;
  mw.ifnb = 20000;
  mw.ifng = 23000;
  mw.tgfb = 4760;
  mw.gmcsf = 25000; #14 - 35kDa
  #molecular weights kDa
  mw.fer = 484*1e3; #Da
  mw.spd = 43*1e3; #Da
  mw.crp = 20*1e3; #Da
  
  ###
  VmProtSynth =	6797.166667	#maximal production rate of liver CRP
  KmProtSyn	= 0.55	#EC50 of CRP production in liver
  kbasal_CRP	= 7.816666667	#basal production rate of CRP in blood
  kCRP_LivertoBlood	= 0.03685025	#transit rate of CRP from liver to blood
  kCRP_BloodtoLiver	= 0.037875	#transit rate of CRP from blood to liver
  kCRPSecretion	= 0.019791667	#clearance rate of CRP from blood
  kdeg_CRP	= 0.12	#fraction of liver CRP secreted in blood
  k_livercrp	= 520	#basal production of CRP in liver
  
  
  
  #variables from IBD model needed for parameters
  iDC = 1e7;
  M0 = 1e7;
  Th0 = 1e7;
  iCD4 = 300; #/uL plasma naive T cells
  iCD8 = 200; #/uL plasma naive T cells
  iMono = 500;  #/uL plasma monocytes
  iN = 2000; # immature N
  

  EC50_IL17_eff=1e5
  
  #############
  K = 1.26e-10
  P_pl_a = 25
  P_pl_v = 15
  P_isf = -3
  pi_pl_0 = 25
  v_pl_0 = 3.02e-5
  pi_isf_0 = 3
  V_isf_0 = 6.0e-5 #uL
  V_isf = 5.62e-5 #uL
  
  FVad = 0.213		# adipose
  FVbo = 0.085629	# bone
  FVbr = 0.02		# brain
  FVgu = 0.0171		# gut
  FVhe = 0.0047		# heart
  FVki = 0.0044		# kidney
  FVli = 0.021		# liver 
  FVlu = 0.0076		# lung
  FVmu = 0.4                 	# muscle
  FVsk = 0.0371              	# skin
  FVsp = 0.0026              	# spleen 
  FQte = 0.01076         
  FVve = 0.0514		# venous
  FVar = 0.0257		# arterial 
  FVpl = 0.0424             	 #plasma  
  FVrb = 0.0347              # erythrocytes 
  FVre =0.099771          	#rest of body    
  BW = 70		# BW 
  
  V_adipose = BW*FVad		 
  V_bone = BW*FVbo    
  V_brain = BW*FVbr	
  V_gut = BW*FVgu          
  V_heart = BW*FVhe        	
  V_kidney = BW*FVki          	
  V_liver = BW*FVli		
  V_lung = BW*FVlu		
  V_muscle = BW*FVmu      	
  V_skin = BW*FVsk          	
  V_spleen = BW*FVsp 
  V_gonads = BW*FQte
  V_venous = BW*FVve		
  V_arterial = BW*FVar		
  V_remain = BW*FVre        	
  

  FQad = 0.05              	# adipose 
  FQbo = 0.05              	# bone
  FQbr = 0.12              	# brain 
  FQgu = 0.146462      	# gut
  FQhe = 0.04              	# heart 
  FQki = 0.19              	# kidney 
  FQh = 0.215385       # hepatic (venous side) 
  FQlu = 1	         	#lung
  FQmu = 0.17             # muscle 
  FQsk = 0.05              # skin 
  FQsp = 0.017231       #spleen
  FQte = 0.01076         # testes
  FQre = 0.103855       	# rest of body
  
  KA=1000
 
  Inh_ki_vanco = 45 
  
  
  
  t=sort(ls())
  param=numeric(length(t))
  for (i in 1:length(t)){
    param[i]=get(t[i])
    names(param)[i]=t[i]
  }
  
  return(param)
}
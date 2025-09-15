##############
#references
#cardio-renal model: K. M. Hallow, Y. Gebremichael, A Quantitative Systems Physiology Model of Renal Function and Blood Pressure Regulation: Application in Salt-Sensitive Hypertension. CPT Pharmacometrics Syst Pharmacol 6, 393-400 (2017).
#host immune response modelL: R. Rao, C. J. Musante, R. Allen, A quantitative systems pharmacology model of the pathophysiology and treatment of COVID-19 predicts optimal timing of pharmacological interventions. NPJ Syst Biol Appl 9, 13 (2023).
#bacterial infection model: J. K. Diep, T. A. Russo, G. G. Rao, Mechanism-Based Disease Progression Model Describing Host-Pathogen Interactions During the Pathogenesis of Acinetobacter baumannii Pneumonia. CPT Pharmacometrics Syst Pharmacol 7, 507-516 (2018).


ode <- " 
IL1b_c_pg_mL= IL1b_c*mw.il1b
TNFa_c_pg_mL= TNFa_c*mw.tnfa
IL6_c_pg_mL = IL6_c*mw.il6
IL10_c_pg_mL= IL10_c*mw.il10
IL17_c_pg_mL= IL17_c*mw.il17
TGFb_c_pg_mL = TGFb_c*mw.tgfb

IL1b_pg_mL= IL1b*mw.il1b/vol_alv_ml
TNFa_pg_mL= TNFa*mw.tnfa/vol_alv_ml
IL6_pg_mL = IL6*mw.il6/vol_alv_ml
IL10_pg_mL= IL10*mw.il10/vol_alv_ml
IL17_pg_mL= IL17*mw.il17/vol_alv_ml
TGFb_pg_mL = TGFb*mw.tgfb/vol_alv_ml

EC50_IL1b = 119.0921
EC50_IL6 = 103.5472

IL1b_effect_on_aff= 1+(1*IL1b_c_pg_mL/(IL1b_c_pg_mL+EC50_IL1b))
IL1b_effect_on_eff= 1+(1*IL1b_c_pg_mL/(IL1b_c_pg_mL+EC50_IL1b))

IL6_effect_on_CD = 1
IL6_effect_on_aff= 1+(1*IL6_c_pg_mL/(IL6_c_pg_mL+EC50_IL6))
IL6_effect_on_eff= 1+(1*IL6_c_pg_mL/(IL6_c_pg_mL+EC50_IL6))

IL1b_effect_on_SVR = 1/(1+(1*IL1b_c_pg_mL/(IL1b_c_pg_mL+EC50_IL1b)))^4
IL6_effect_on_SVR = 1/(1+(1*IL6_c_pg_mL/(IL6_c_pg_mL+EC50_IL6)))^4

#################################################################################
tr_TNFa = ktr_TNFa*TNFa;
tr_IL6 = ktr_IL6*IL6;
tr_IL1b = ktr_IL1b*IL1b;

tr_IL2 = ktr_IL2*IL2;
tr_IL12 = ktr_IL12*IL12;
tr_IL17 = ktr_IL17*IL17;
tr_IL10 = ktr_IL10*IL10;
tr_TGFb = ktr_TGFb*TGFb;
tr_GMCSF = ktr_GMCSF*GMCSF;
tr_pDC = ktr_pDC*(pDC -pDC_c);
tr_M1 = ktr_M1*(M1 -M1_c);
tr_N = ktr_N*(N -N_c);
tr_Th1 = ktr_Th1*(Th1 -Th1_c);
tr_Th17 = ktr_Th17*(Th17 -Th17_c);
tr_Treg = ktr_Treg*(Treg -Treg_c);
############################
## healthy alveolar type 2 cells (AT2) (# cells)
  damage_cyt_AT = k_damage_cyt*(k_damage_TNFa*(TNFa/(km_damage_TNFa+TNFa)) +  k_damage_IL6*(IL6/(km_damage_IL6+IL6)) + k_damage_IL1b*(IL1b/(km_damage_IL1b + IL1b)));
	damage_ROS_AT2 = k_ROS_AT2*N/(km_ROS_AT2+N);	
	growth_AT2 = mu_AT2*(1 + k_mu_AT2*(max((2e+10+3e+10)-(AT2+AT1),0)/(km_AT1_AT2*(2e+10+3e+10) + max((2e+10+3e+10)-(AT2+AT1),0))))*AT2; #### induced differentiation and growth induced by AT1 decrease from baseline
	deg_AT2 = b_AT2*AT2;
	diff_AT2 = k_AT1_AT2 * AT2 * (1 + k_diff_AT1*max(2e+10-AT1,0)/(km_AT1_AT2*2e+10 + max(2e+10-AT1,0)));
	

## health alveolar type 1 cells (AT1) (# cells)
	growth_AT1 = mu_AT1*(2e+10-AT1); #### no growth, no division, source is from AT2 
	damage_ROS_AT1 = damage_ROS_AT2*AT1;
	deg_AT1 = b_AT1*AT1;



## damaged AT1 cells (# cells)
	deg_dAT1 = b_dAT1*dAT1;


## damaged AT2 cells (# cells)
	deg_dAT2 = b_dAT1*dAT2;

################immune cells##################
########## lung pulmonary dendritic cells (# cells)
act_pDC_TNFa = k_DC_TNFa*(TNFa/(km_DC_TNFa+TNFa));
act_pDC_IL6 = k_DC_IL6*(IL6/(km_DC_IL6+IL6));
act_pDC_GMCSF = k_M1_GMCSF*(GMCSF/(km_M1_GMCSF+GMCSF));
inh_pDC_IL10 = (km_DC_IL10/(km_DC_IL10+IL10));


    
########lung M1 macrophages (# cells)
act_M1_TNFa = k_M1_TNFa*(TNFa/(km_M1_TNFa+TNFa));
act_M1_GMCSF = k_M1_GMCSF*(GMCSF/(km_M1_GMCSF+GMCSF));

inh_M1_IL10 = (km_M1_IL10/(km_M1_IL10+IL10));

###########lung Neutrophils (# cells)

act_N_TNFa= k_N_TNFa*TNFa/(TNFa+km_N_TNFa); 
act_N_GMCSF = k_N_GMCSF*GMCSF/(GMCSF+km_N_GMCSF);
rec_N_IL17c = k_N_IL17c*IL17_c/(IL17_c + km_N_IL17c);

IC50_mcg = 9.3507e-7*392.464*1e6/10 #van Overveld et al (2003)


 ###########lung Th1 Cells (# cells)
act_Th1_IL12 = k_Th1_IL2*(IL12/(K_Th1_IL12+IL12))*(1+k_Th1_IL12IL2*(IL2/(K_Th1_IL12IL2+IL2)));

inh_Th1_IL10_TGFb = (K_Th1_IL10/(K_Th1_IL10+IL10)) * (K_Th1_TGFb/(K_Th1_TGFb+TGFb));
diff_Th1_Th17 = k_Th1_Th17*Th17*(IL12/(K_Th1_Th17+IL12))*(K_Th1_TGFb/(K_Th1_TGFb+TGFb));
diff_Th1_Treg = k_Th1_Treg*Treg*(IL12/(K_Th1_Treg+IL12));

###########lung TH17 cells (# cells)
inh_Th17 = (K_Th17_IL2/(K_Th17_IL2 + IL2))  * (K_Th17_IL10/(K_Th17_IL10 + IL10));
act_TH17_TGFb = k_Th17_TGFb*(TGFb/(K_Th17_TGFb + TGFb)) * inh_Th17;
act_Th17_IL6 = k_Th17_IL6*(IL6/(km_Th17_IL6 + IL6)) * inh_Th17;
act_Th17_IL1b = k_Th17_IL1b*(IL1b/(km_Th17_IL1b + IL1b)) * inh_Th17;


##########lung Treg (# cells)
act_Treg_IL2 = k_Treg_IL2*(IL2/(K_Treg_IL2 + IL2)) * (K_Treg_IL17/(K_Treg_IL17 + IL17)) * (K_Treg_IL6/(K_Treg_IL6 + IL6));
act_Treg_TGFb = k_Treg_TGFb*(TGFb/(K_Treg_TGFb + TGFb)) * (K_Treg_IL17/(K_Treg_IL17 + IL17)) * (K_Treg_IL6/(K_Treg_IL6 + IL6));

####Liver CRP Production (pmol)
    Liver_CRP = k_livercrp*V_liver*1000*((IL6_c*V_liver*1000));    
    prod_CRP_liver = kCRPSecretion*Liver_CRP;
    tr_CRP = kCRP_BloodtoLiver*Blood_CRP-kCRP_LivertoBlood*Liver_CRP/(V_liver*1000);
    prod_CRP_blood = kbasal_CRP;
    CRP_mg_dl = Blood_CRP/10

########Cytokine Dynamics

##TNF-A (pmol)
  prod_tnf_dat1 = a_tnf_at1*dAT1;
	prod_tnf_dat2 = a_tnf_at2*dAT2;
	prod_tnf_m1 = a_tnf_m1*M1;
	prod_tnf_th1 = a_tnf_th1*Th1;
	prod_tnf_th17 = a_tnf_th17*Th17;
	deg_tnf = b_tnf*TNFa;
	    
##IL-6 (pmol)
  prod_il6_dat1 = a_il6_at1*dAT1;
	prod_il6_dat2 = a_il6_at2*dAT2;
	prod_il6_m1 = a_il6_m1*M1;
	prod_il6_th17 = a_il6_th17*Th17;
	prod_il6_neu =a_il6_neu*N;
	deg_il6 = b_il6*IL6;

##IL1-B (pmol)
prod_il1b_dat1 = a_il1b_at2*dAT1;
prod_il1b_dat2 = a_il1b_at2*dAT2;
prod_il1b_m1 = a_il1b_m1*M1;
a_il1b_dc = a_il1b_m1;
prod_il1b_dc = a_il1b_dc*pDC;
deg_il1b = b_il1b*IL1b;



####IL-2 (pmol)
	prod_il2_dc = a_il2_dc*pDC;
	prod_il2_th1 = a_il2_th1*Th1;
	deg_il2 = b_il2*IL2;

####IL-12 (pmol)
	prod_il12_m1 = a_il12_m1*M1;
	prod_il12_dc = a_il12_dc*pDC;
	deg_il12 = b_il12*IL12;


#####IL-17 (pmol)
	prod_il17_th17 = a_il17_th17*Th17;
	deg_il17 = b_il17*IL17;

##### IL-10 (pmol)
	prod_il10_treg = a_il10_treg*Treg;
	deg_il10 = b_il10*IL10;

##### TGF-B (pmol)
	prod_tgfb_th17 = a_tgfb_th17*Th17;
	prod_tgfb_treg = a_tgfb_treg*Treg;
	deg_tgfb = b_tgfb*TGFb;

#####GM-CSF (pmol)
	prod_gmcsf_m1 = a_gmcsf_m1*M1;
	prod_gmcsf_th1 = a_gmcsf_th1*Th1;
	prod_gmcsf_th17 = a_gmcsf_th17*Th17;
	deg_gmcsf = b_gmcsf*GMCSF;

nom_afferent_diameter=1.5e-5*IL1b_effect_on_aff*IL6_effect_on_aff			 	#mmHg
nom_efferent_diameter=1.1e-05*IL1b_effect_on_eff*IL6_effect_on_eff 		#mmHg

 ########################################################################################
  #The following parameters are calculated at equilibrium using the parameters above
  ########################################################################################
  
  #This pressure is the setpoint that determines the myogenic response of the preafferent vasculature
  nom_preafferent_pressure = nominal_map_setpoint - nom_renal_blood_flow_L_min*nom_preafferent_arteriole_resistance;
  
  #This pressure is the setpoint that determines the myogenic response of the afferent vasculature
  nom_glomerular_pressure = nom_preafferent_pressure - nom_renal_blood_flow_L_min*(L_m3*viscosity_length_constant/(nom_afferent_diameter^4)/baseline_nephrons);
  
  #This pressure is the setpoint that determines the tubular pressure-natriuresis response 
  nom_postglomerular_pressure = nom_preafferent_pressure - nom_renal_blood_flow_L_min*(L_m3*viscosity_length_constant*(1/(nom_afferent_diameter^4)+1/(nom_efferent_diameter^4))/baseline_nephrons);
  
  RIHP0 = nom_postglomerular_pressure 	
  
  # The rate of sodium excretion must equal the rate of sodium intake. Sodium reabsorption rates vary along the tubule, but based on literature
  # measurements we have a good, and literature data provides estimates for these rates. However, there is a precise
  # rate of sodium reabsorption required to achieve the equilibrium defined by the parameters above.
  # Assuming that reabsorption rates are known in all but one segment of the tubule, the exact rate
  # of reabsorption of the remaining segment can be calculated. We chose to calculate the CD rate of reabsorpion based on estimates for
  # PT, LoH, and DT reabsorption.  
  nom_GFR = nom_Kf*(nom_glomerular_pressure - nom_oncotic_pressure_difference - (Pc_pt_mmHg+P_interstitial_mmHg))/nL_mL*baseline_nephrons;
  nom_filtered_sodium_load = nom_GFR/L_mL*ref_Na_concentration;
  nom_PT_Na_outflow = nom_filtered_sodium_load*(1-nominal_pt_na_reabsorption);
  
  nom_Na_in_AscLoH = nom_PT_Na_outflow/baseline_nephrons;
  AscLoH_Reab_Rate =(2*nominal_loh_na_reabsorption*nom_Na_in_AscLoH)/L_lh_des; #osmoles reabsorbed per unit length per minute. factor of 2 because osmoles = 2
  
  nom_LoH_Na_outflow = nom_PT_Na_outflow*(1-nominal_loh_na_reabsorption);
  nom_DT_Na_outflow = nom_LoH_Na_outflow*(1-nominal_dt_na_reabsorption);
  nominal_cd_na_reabsorption = 1-Na_intake_rate/nom_DT_Na_outflow;
  
  
  
  #RBF = (MAP - P_venous)/RVR. Given MAP, P_venous, RBF, and preafferent, afferent, and efferent resistances, the remaining peritubular resistance at steady state can be determined
  nom_RVR = (nominal_map_setpoint - P_venous)/nom_renal_blood_flow_L_min*(1+TNFa_c_pg_mL/(TNFa_c_pg_mL+300))
  nom_peritubular_resistance = nom_RVR - (nom_preafferent_arteriole_resistance + L_m3*viscosity_length_constant*(1/nom_afferent_diameter^4+1/nom_efferent_diameter^4)/baseline_nephrons);
  
  
  #Given the values for baseline MAP and CO above, the baseline TPR required to maintain this MAP and CO can be calculated. Since TPR includes renal vascular resistance, the baseline systemic (non-renal) resistance
  #can be calculated from this TPR and the values for baseline renal resistances defined above. 
  nom_TPR = nominal_map_setpoint/CO_nom
  nom_systemic_arterial_resistance= nom_TPR-R_venous
  
  #Creatinine synthesisrate at equilibrium
  creatinine_synthesis_rate  = equilibrium_serum_creatinine * dl_ml * nom_GFR #Units: mg/min
  
  
  ####RAAS Pathway parameters
  #Values for half lives and equilibrium concentrations of RAAS peptides available in the literature and 
  # defined above to calculate nominal values for other RAAS parameters not available in the literature:
  #ACE activity
  #Chymase activity
  #AT1 receptor binding rate
  #AT2 receptor binding rate
  #equilibrium AT1_bound_AngII
  #These values are then assumed to be fixed unless specified otherwise.
  #Calculating these nominal parameter values initially in a separate file is required so that these parameters can then be varied independently in the main model
  nominal_equilibrium_PRC = nominal_equilibrium_PRA/concentration_to_renin_activity_conversion_plasma
  nominal_AngI_degradation_rate = log(2)/nominal_AngI_half_life #/hr
  nominal_AngII_degradation_rate = log(2)/nominal_AngII_half_life #/hr
  nominal_AT1_bound_AngII_degradation_rate = log(2)/nominal_AT1_bound_AngII_half_life
  nominal_AT2_bound_AngII_degradation_rate = log(2)/nominal_AT2_bound_AngII_half_life
  #ACE converts 95% of AngI, chymase converts the rest
  nominal_ACE_activity = (ACE_chymase_fraction*(nominal_equilibrium_PRA - nominal_AngI_degradation_rate*nominal_equilibrium_AngI)/nominal_equilibrium_AngI)#Therapy_effect_on_ACE
  nominal_chymase_activity = (1-ACE_chymase_fraction)*(nominal_equilibrium_PRA - nominal_AngI_degradation_rate*nominal_equilibrium_AngI)/nominal_equilibrium_AngI
  #75% of bound AngII is AT1, the rest is AT2
  nominal_AT1_receptor_binding_rate = fraction_AT1_bound_AngII*(nominal_equilibrium_AngI*(nominal_ACE_activity+nominal_chymase_activity)-nominal_AngII_degradation_rate*nominal_equilibrium_AngII)/nominal_equilibrium_AngII
  nominal_AT2_receptor_binding_rate = (1-fraction_AT1_bound_AngII)*(nominal_equilibrium_AngI*(nominal_ACE_activity+nominal_chymase_activity)-nominal_AngII_degradation_rate*nominal_equilibrium_AngII)/nominal_equilibrium_AngII
  nominal_equilibrium_AT1_bound_AngII = nominal_equilibrium_AngII*nominal_AT1_receptor_binding_rate/nominal_AT1_bound_AngII_degradation_rate
  nominal_equilibrium_AT2_bound_AngII = nominal_equilibrium_AngII*nominal_AT2_receptor_binding_rate/nominal_AT2_bound_AngII_degradation_rate
  
#######################################Systemic Hemodynamics #################################################
#################  Systemic Vascular Resistance
#Systemic vascular resistance is a nominal value modulated by AngII and by a regulatory blood flow autoregulation signal
###Whole body autoregulation mechanism wherein TPR adjusts to maintain constant organ blood flow (and thus constant cardiac output)
#Modeled as Proportional-Integral controller of TPR, where the input signal is the cardiac output error signal
tissue_autoregulation_signal = max(0.1,1+tissue_autoreg_scale*((Kp_CO/CO_scale_species)*(cardiac_output_delayed - CO_nom)+(Ki_CO/CO_scale_species)*CO_error));
###Effect of the RAAS (AT1-bound AngII) on systemic vascular resistance. 
AT1_svr_int = 1 - AT1_svr_slope*nominal_equilibrium_AT1_bound_AngII;
AT1_bound_AngII_effect_on_SVR = AT1_svr_int + AT1_svr_slope * AT1_bound_AngII;
systemic_arterial_resistance = nom_systemic_arterial_resistance*tissue_autoregulation_signal*AT1_bound_AngII_effect_on_SVR;  
################# Cardiac Output and Mean Arterial Pressure
#Cardiac output is a function of blood volume and resistance to venous return
resistance_to_venous_return = ((8 * R_venous + systemic_arterial_resistance)*IL1b_effect_on_SVR*IL6_effect_on_SVR / 31); 
mean_filling_pressure = nom_mean_filling_pressure + (blood_volume_L/BV_scale_species-blood_volume_nom)/venous_compliance;
cardiac_output =  mean_filling_pressure / resistance_to_venous_return; 



  Q_adipose = cardiac_output*60*FQad      #L/h
  Q_bone = cardiac_output*60*FQbo  
  Q_brain = cardiac_output*60*FQbr  
  Q_gut = cardiac_output*60*FQgu            
  Q_heart=cardiac_output*60*FQhe            
  Q_kidney=cardiac_output*60*FQki  
  Q_liver=cardiac_output*60*FQh               
  Q_muscle=cardiac_output*60*FQmu          
  Q_skin=cardiac_output*60*FQsk    
  Q_spleen = cardiac_output*60*FQsp            
  Q_gonads = cardiac_output*60*FQte
  Qha = Q_liver - Q_gut - Q_spleen
  Q_remain=cardiac_output*60*FQre
 
  
  
################# Cardiac Output and Mean Arterial Pressure
total_peripheral_resistance = systemic_arterial_resistance + R_venous;
mean_arterial_pressure_MAP = cardiac_output * total_peripheral_resistance;
####################################### Renal Vasculature #################################################
###AT1-bound AngII constricts the preafferent, afferent, and efferent arterioles
AT1_preaff_int = 1 - AT1_preaff_scale/2;
AT1_effect_on_preaff = AT1_preaff_int + AT1_preaff_scale/(1+exp(-(AT1_bound_AngII - nominal_equilibrium_AT1_bound_AngII)/AT1_preaff_slope));
AT1_aff_int = 1 - AT1_aff_scale/2;
AT1_effect_on_aff = AT1_aff_int + AT1_aff_scale/(1+exp(-(AT1_bound_AngII - nominal_equilibrium_AT1_bound_AngII)/AT1_aff_slope));
AT1_eff_int = 1 - AT1_eff_scale/2;
AT1_effect_on_eff = AT1_eff_int + AT1_eff_scale/(1+exp(-(AT1_bound_AngII - nominal_equilibrium_AT1_bound_AngII)/AT1_eff_slope));
#################  Preafferent Resistance
#The resistance of the arcuate, interlobular arterioles, and other vasculature prior the afferent arterioles is represented by a single resistance - the preafferent arteriole resistance
#The preafferent arterioles respond myogenically to changes in pressure, and also responds to AT1-bound AngII
#The dilation/constriction of the arterioles is limited, and thus the total combined effect of all regulators must saturate
preaff_arteriole_signal_multiplier = AT1_effect_on_preaff*(preafferent_pressure_autoreg_signal)*CCB_effect_on_preafferent_resistance;
preaff_arteriole_adjusted_signal_multiplier = (1/(1+exp(preaff_signal_nonlin_scale*(1-preaff_arteriole_signal_multiplier)))+0.5);
preafferent_arteriole_resistance = nom_preafferent_arteriole_resistance*preaff_arteriole_adjusted_signal_multiplier;
#################  Afferent Arteriole Resistance
#The afferent arteriole responses the tubuloglomerular feedback (calculated later), as well as to AT1-bound AngII. 
#It may respond myogenically as well. Some studies suggest the upstream portion responds myogenically while the distal portion responds to TGF. Thus, one could consider the 
#myogenically responsive portion as part of the preafferent resistance. 
#The dilation/constriction of the arterioles is limited, and thus the total combined effect of all regulators must saturate
nom_afferent_arteriole_resistance = L_m3*viscosity_length_constant/(nom_afferent_diameter^4);
afferent_arteriole_signal_multiplier = tubulo_glomerular_feedback_effect * AT1_effect_on_aff *glomerular_pressure_autoreg_signal*CCB_effect_on_afferent_resistance;
afferent_arteriole_adjusted_signal_multiplier = (1/(1+exp(afferent_signal_nonlin_scale*(1-afferent_arteriole_signal_multiplier)))+0.5);
afferent_arteriole_resistance = nom_afferent_arteriole_resistance*afferent_arteriole_adjusted_signal_multiplier;
#################  Efferent Arteriole Resistance
#The efferent arteriole responses to AT1-bound AngII.
#The dilation/constriction of the arterioles is limited, and thus the total combined effect of all regulators must saturate
nom_efferent_arteriole_resistance = L_m3*viscosity_length_constant/(nom_efferent_diameter^4);
efferent_arteriole_signal_multiplier = AT1_effect_on_eff *CCB_effect_on_efferent_resistance;
efferent_arteriole_adjusted_signal_multiplier = 1/(1+exp(efferent_signal_nonlin_scale*(1-efferent_arteriole_signal_multiplier)))+0.5;
efferent_arteriole_resistance = nom_efferent_arteriole_resistance*efferent_arteriole_adjusted_signal_multiplier;
#################  Peritubular Resistance
peritubular_resistance = nom_peritubular_resistance*baseline_nephrons;
#################  Renal Vascular Resistance
renal_vascular_resistance = preafferent_arteriole_resistance + (afferent_arteriole_resistance + efferent_arteriole_resistance + peritubular_resistance)/baseline_nephrons; 
#################  Renal blood flow
renal_blood_flow_L_min = ((mean_arterial_pressure_MAP - P_venous) / renal_vascular_resistance); 
renal_blood_flow_ml_hr = renal_blood_flow_L_min * 1000 * 60;
#################  Renal Vasculature Pressures
preafferent_pressure = mean_arterial_pressure_MAP - renal_blood_flow_L_min*preafferent_arteriole_resistance;
glomerular_pressure = (mean_arterial_pressure_MAP  - renal_blood_flow_L_min * (preafferent_arteriole_resistance + afferent_arteriole_resistance / baseline_nephrons));
postglomerular_pressure = (mean_arterial_pressure_MAP  - renal_blood_flow_L_min * (preafferent_arteriole_resistance + (afferent_arteriole_resistance+efferent_arteriole_resistance) / baseline_nephrons));
#################  Autoregulatory signals for preafferent and afferent resistances
preaff_autoreg_int = 1 - preaff_autoreg_scale/2;
preafferent_pressure_autoreg_function = preaff_autoreg_int+preaff_autoreg_scale/(1+exp((nom_preafferent_pressure - preafferent_pressure)/myogenic_steepness));
gp_autoreg_int = 1 - gp_autoreg_scale/2;
glomerular_pressure_autoreg_function = gp_autoreg_int+gp_autoreg_scale/(1+exp((nom_glomerular_pressure - glomerular_pressure)/myogenic_steepness));
####################################### Glomerular Filtration #################################################
#################  Glomerular Ultrafiltration Coefficient Kf
vanco_ki_damage = 1 #Inh_ki_vanco/(C_vas_ki_vanco+Inh_ki_vanco)
glomerular_hydrostatic_conductance_Kf = nom_Kf*vanco_ki_damage

#################  Glomerular Filtration Rate
#GFR is calculated according to Starling's equation
number_of_functional_nephrons = baseline_nephrons;
net_filtration_pressure = glomerular_pressure - oncotic_pressure_difference - P_bowmans;
SNGFR_nL_min = glomerular_hydrostatic_conductance_Kf * (glomerular_pressure - oncotic_pressure_difference - P_bowmans);
GFR =  (SNGFR_nL_min / 1000 / 1000000 * number_of_functional_nephrons);
GFR_ml_min = GFR * 1000;
#################  Serum Creatinine
serum_creatinine_concentration = serum_creatinine/blood_volume_L;
creatinine_clearance_rate = GFR_ml_min * dl_ml * serum_creatinine_concentration; #Units: mg/min
#################  Oncotic pressure
#Landis Pappenheimer equation used to calculate oncotic pressure at entrance and exit to glomerulus
#Oncotic pressure is approximated as varying linearly along the glomerulus. Oncotic pressure in the Bowman's space is zero
#Thus the average pressure difference is the average of the entrance and exit oncotic pressure
#We do not consider filtration equilibrium
Oncotic_pressure_in = 1.629*plasma_protein_concentration+0.2935*(plasma_protein_concentration^2);
SNRBF_nl_min = 1e6*1000*renal_blood_flow_L_min/number_of_functional_nephrons;
plasma_protein_concentration_out = SNRBF_nl_min*plasma_protein_concentration/(SNRBF_nl_min-SNGFR_nL_min);
Oncotic_pressure_out = 1.629*plasma_protein_concentration_out+0.2935*(plasma_protein_concentration_out^2);
oncotic_pressure_avg = (Oncotic_pressure_in+Oncotic_pressure_out)/2;
####################################### Plasma sodium concentration and vasopressin secretion #################################################
#################  Plasma sodium concentration
Na_concentration = sodium_amount / blood_volume_L;
ECF_Na_concentration = ECF_sodium_amount/extracellular_fluid_volume;
#################  Control of vasopressin secretion
#A proportional-integral controller is used to ensure there is no steady state error in sodium concentration
#Relative gains of the P and I controller must be chosen carefully.
#In order to permit a steady-state error, the integral controller can be removed. But care should be given then in choosing the proportional gain
Na_water_controller = Na_controller_gain*(Kp_VP*(Na_concentration - ref_Na_concentration)+Ki_VP*Na_concentration_error);
#################  Vasopressin
#Vasopressin is critical in the model, because it allows water excretion to be decoupled from sodium excretion in the collecting duct
normalized_vasopressin_concentration = 1 + Na_water_controller;
vasopressin_concentration = nominal_vasopressin_conc * normalized_vasopressin_concentration;
#Effect of vasopressin on water intake
water_intake_vasopressin_int = 1-water_intake_vasopressin_scale/2;
water_intake = water_intake_species_scale*(nom_water_intake/60/24)*(water_intake_vasopressin_int + water_intake_vasopressin_scale/(1+exp((normalized_vasopressin_concentration_delayed-1)/water_intake_vasopressin_slope)));
daily_water_intake = (water_intake * 24 * 60);
####################################### Tubular Flow and Reabsorption #################################################
Dc_pt = Dc_pt_nom;
L_pt = L_pt_s1_nom+L_pt_s2_nom + L_pt_s3_nom;
#################  Filtered Na Load #################  
SN_filtered_Na_load = (SNGFR_nL_min / 1000 / 1000000)*Na_concentration;
filtered_Na_load = SN_filtered_Na_load*number_of_functional_nephrons;
#################  Regulatory effects on reabsorption #################  
### Tubular Pressure natriuresis effects through RIHP:
pressure_natriuresis_PT_int = 1 - pressure_natriuresis_PT_scale/2;
pressure_natriuresis_PT_effect = max(0.001,pressure_natriuresis_PT_int + pressure_natriuresis_PT_scale / (1 + exp((postglomerular_pressure- RIHP0) / pressure_natriuresis_PT_slope))); 
pressure_natriuresis_LoH_int = 1 - pressure_natriuresis_LoH_scale/2;
pressure_natriuresis_LoH_effect = max(0.001,pressure_natriuresis_LoH_int + pressure_natriuresis_LoH_scale / (1 + exp((postglomerular_pressure - RIHP0) / pressure_natriuresis_LoH_slope))); 
pressure_natriuresis_DCT_magnitude = max(0,pressure_natriuresis_DCT_scale );
pressure_natriuresis_DCT_int = 1 - pressure_natriuresis_DCT_magnitude/2;
pressure_natriuresis_DCT_effect = max(0.001,pressure_natriuresis_DCT_int + pressure_natriuresis_DCT_magnitude/ (1 + exp((postglomerular_pressure - RIHP0) / pressure_natriuresis_DCT_slope))); 
pressure_natriuresis_CD_magnitude = max(0,pressure_natriuresis_CD_scale);
pressure_natriuresis_CD_int = 1 - pressure_natriuresis_CD_magnitude/2;
pressure_natriuresis_CD_effect = max(0.001,pressure_natriuresis_CD_int + pressure_natriuresis_CD_magnitude/ (1 + exp((postglomerular_pressure - RIHP0) / pressure_natriuresis_CD_slope))); 
### AT1-bound AngII effect on PT reabsorption 
AT1_PT_int = 1 - AT1_PT_scale/2;
AT1_effect_on_PT = AT1_PT_int + AT1_PT_scale/(1+exp(-(AT1_bound_AngII - nominal_equilibrium_AT1_bound_AngII)/AT1_PT_slope));
### Aldosterone effect on DCT and CD reabsorption
aldosterone_concentration = normalized_aldosterone_level_delayed* nominal_aldosterone_concentration; 
Aldo_MR_normalised_effect = aldosterone_concentration*MR_antagonist_effect_on_aldo_MR;

aldo_DCT_int = 1 - aldo_DCT_scale/2;
aldo_effect_on_DCT = aldo_DCT_int + aldo_DCT_scale/(1+exp((1 - Aldo_MR_normalised_effect)/aldo_DCT_slope));
aldo_CD_int = 1 - aldo_CD_scale/2;
aldo_effect_on_CD= aldo_CD_int + aldo_CD_scale/(1+exp((1 - Aldo_MR_normalised_effect)/aldo_CD_slope));
### Tubular fractional Reabsorption rates, modulated by regulatory mechanisms (RIHP, AT1-bound AngII, and aldo)
e_pt_sodreab = min(1,nominal_pt_na_reabsorption * AT1_effect_on_PT *pressure_natriuresis_PT_effect);
e_dct_sodreab = min(1,nominal_dt_na_reabsorption * aldo_effect_on_DCT*pressure_natriuresis_DCT_effect *HCTZ_effect_on_DT_Na_reabs); 
e_cd_sodreab = min(1,nominal_cd_na_reabsorption*aldo_effect_on_CD*pressure_natriuresis_CD_effect);
#################  Proximal Tubule Na and Water reabsorption #################  
Na_reabs_per_unit_length = -log(1-e_pt_sodreab)/(L_pt); #mmol/min
Na_pt_out = SN_filtered_Na_load*exp(-Na_reabs_per_unit_length*L_pt);
water_out_pt = ((SNGFR_nL_min / 1000 / 1000000)/SN_filtered_Na_load)*Na_pt_out;
PT_Na_reabs_fraction = 1-Na_pt_out/SN_filtered_Na_load;
PT_water_reabs_fraction = 1-water_out_pt/(SNGFR_nL_min / 1000 / 1000000);
Na_concentration_out_pt = Na_pt_out/water_out_pt;
PT_Na_outflow = Na_pt_out*number_of_functional_nephrons;
#################  Loop of Henle Na and Water reabsorption #################  
##### Descending Loop of Henle #####
water_in_DescLoH = water_out_pt; # L/min
Na_in_DescLoH = Na_pt_out;
Na_concentration_in_DescLoH = Na_concentration_out_pt;
###No solute reabsorption in Descending Limb
Na_out_DescLoH = Na_in_DescLoH;
### Na Reabsorption rate in the Ascending Limb
#The rate of reabsorption per unit length may be flow-dependent, and may be modulated by tubular pressure-natriuresis
# If LoH_flow_dependence = 0, then no flow dependence. If LoH_flow_dependence = 1, perfect flow dependence
deltaLoH_NaFlow = LoH_flow_dependence*(Na_out_DescLoH-nom_Na_in_AscLoH);
# Na reabsorbed per unit length per minute. 
AscLoH_Reab_Rate =(nominal_loh_na_reabsorption*(nom_Na_in_AscLoH+deltaLoH_NaFlow))/L_lh_des; 
effective_AscLoH_Reab_Rate =AscLoH_Reab_Rate*pressure_natriuresis_LoH_effect; 
### Water reabsorption in the Descending Limb
#Descending limb is in equilibrium with interstitium, since water is reabsorbed across the osmotic gradient. The osmotic 
#gradient is generated by Na reabsorption in the ascending limb. Thus, the Na concentration along the descending limb is 
#determined by Na reabsorption in the ascending limb
#Min function necesssary to ensure that the LoH does not reabsorb more Na than is delivered to it
Na_concentration_out_DescLoH = Na_concentration_in_DescLoH*exp(min(effective_AscLoH_Reab_Rate*L_lh_des,Na_in_DescLoH)/(water_in_DescLoH*Na_concentration_in_DescLoH));
water_out_DescLoH = water_in_DescLoH*Na_concentration_in_DescLoH/Na_concentration_out_DescLoH;
##### Ascending Loop of Henle #####
Na_in_AscLoH = Na_out_DescLoH;
Na_concentration_in_AscLoH = Na_concentration_out_DescLoH;
water_in_AscLoH = water_out_DescLoH;
#Na reabsorption along the ascending limb
Na_concentration_out_AscLoH = Na_concentration_in_AscLoH - min(L_lh_des*effective_AscLoH_Reab_Rate, Na_in_DescLoH)*(exp(min(L_lh_des*effective_AscLoH_Reab_Rate, Na_in_DescLoH)/(water_in_DescLoH*Na_concentration_in_DescLoH))/water_in_DescLoH);
Na_reabsorbed_AscLoH = (Na_concentration_in_AscLoH - Na_concentration_out_AscLoH)*water_in_AscLoH;
Na_out_AscLoH = max(0, Na_in_AscLoH - Na_reabsorbed_AscLoH);
#Ascending limb is impermeable to water - no water reabsorption
water_out_AscLoH = water_in_AscLoH;
Na_concentration_out_AscLoH = Na_out_AscLoH/water_out_AscLoH;
LoH_reabs_fraction = 1-Na_out_AscLoH/Na_in_AscLoH;
### Macula Densa Na Flow and Concentration
SN_macula_densa_Na_flow = Na_out_AscLoH;
MD_Na_concentration = Na_concentration_out_AscLoH;
### Tubuloglomerular feedback 
TGF0_tubulo_glomerular_feedback = 1 - S_tubulo_glomerular_feedback/2;
tubulo_glomerular_feedback_signal = (TGF0_tubulo_glomerular_feedback + S_tubulo_glomerular_feedback / (1 + exp((MD_Na_concentration_setpoint - MD_Na_concentration)/ F_md_scale_tubulo_glomerular_feedback)));
#################  Distal Convoluted Tubule #################  
water_in_DCT = water_out_AscLoH; 
Na_in_DCT = Na_out_AscLoH;
Na_concentration_in_DCT = Na_concentration_out_AscLoH; 
#Assume DCT is impermeable to water
water_out_DCT = water_in_DCT;
#Assume sodium reabsorption at a constant fraction of delivery
R_dct = -log(1-e_dct_sodreab)/L_dct;
Na_out_DCT = Na_in_DCT*exp(-R_dct*L_dct);
Na_concentration_out_DCT = Na_out_DCT/water_out_DCT;
DCT_Na_reabs_fraction = 1-Na_out_DCT/Na_in_DCT; 
#################  Collecting Duct #################  
water_in_CD = water_out_DCT;
Na_in_CD = Na_out_DCT;
Na_concentration_in_CD = Na_concentration_out_DCT;
####Assume sodium reabsorbed, then water follows, modulated by vasopressin (ADH)
#Assume sodium reabsorbed at fractional rate eta
R_cd = -log(1-e_cd_sodreab)/L_cd;
Na_out_CD = Na_in_CD*exp(-R_cd*L_cd);
CD_Na_reabs_fraction = 1-Na_out_CD/Na_in_CD; 
#Vasopressin (ADH) effect on water reabsorption through regulation of aquaporin
ADH_water_permeability = min(1,max(0,nom_ADH_water_permeability*normalized_vasopressin_concentration));
#Water reabsorption follows gradient but is regulated by ADH
max_water_reabs_CD = water_in_CD-(Na_concentration_in_CD*water_in_CD-Na_in_CD*(1-exp(-R_cd*L_cd)))/Na_concentration_in_AscLoH; 
water_out_CD = max(0,water_in_CD - ADH_water_permeability*max_water_reabs_CD);
#################  Urine Sodium and Water Excretion #################  
#Urine flow rate
urine_flow_rate = water_out_CD*number_of_functional_nephrons;
daily_urine_flow = (urine_flow_rate * 60 * 24);
#Na Excretion
Na_excretion_via_urine = Na_out_CD*number_of_functional_nephrons;
Na_balance = Na_intake_rate - Na_excretion_via_urine;
water_balance = daily_water_intake - daily_urine_flow;
#Fractional Excretion of Sodium
FENA = Na_excretion_via_urine/filtered_Na_load;
####################################### Tubular Pressure #################################################
#####See written documentation for derivation of the equations below
#flow rates expressed in m3/min, rather than L/min
mmHg_Nperm2_conv = 133.32;
Pc_pt = Pc_pt_mmHg*mmHg_Nperm2_conv;
Pc_lh_des = Pc_lh_des_mmHg*mmHg_Nperm2_conv;
Pc_lh_asc = Pc_lh_asc_mmHg*mmHg_Nperm2_conv;
Pc_dt = Pc_dt_mmHg*mmHg_Nperm2_conv;
Pc_cd = Pc_cd_mmHg*mmHg_Nperm2_conv;
P_interstitial = P_interstitial_mmHg*mmHg_Nperm2_conv;
pi=3.14;
#################  CNT/CD
B1 = (4*tubular_compliance+1)*128*gamma/pi;
mean_cd_water_flow = (water_in_CD-water_out_CD)/2;
B2_cd = (Pc_cd^(4*tubular_compliance))/(Dc_cd^4);
P_in_cd = (0^(4*tubular_compliance+1)+B1*B2_cd*(mean_cd_water_flow/1e3)*L_cd)^(1/(4*tubular_compliance+1));
P_in_cd_mmHg = (P_in_cd+P_interstitial)/mmHg_Nperm2_conv;
#################  DCT
B2_dt = (Pc_dt^(4*tubular_compliance))/(Dc_dt^4);
P_in_dt = (P_in_cd^(4*tubular_compliance+1)+B1*B2_dt*(water_in_DCT/1e3)*L_dct)^(1/(4*tubular_compliance+1));
P_in_dt_mmHg = (P_in_dt+P_interstitial)/mmHg_Nperm2_conv;
#################  Asc LoH
B2_lh_asc = (Pc_lh_asc^(4*tubular_compliance))/(Dc_lh^4);
P_in_lh_asc = (P_in_dt^(4*tubular_compliance+1)+B1*B2_lh_asc*(water_in_AscLoH/1e3)*L_lh_asc)^(1/(4*tubular_compliance+1));
P_in_lh_asc_mmHg = (P_in_lh_asc+P_interstitial)/mmHg_Nperm2_conv;
#################  Desc LoH
A_lh_des = effective_AscLoH_Reab_Rate/(water_in_DescLoH*Na_concentration_in_DescLoH);
B2_lh_des = (Pc_lh_des^(4*tubular_compliance))*(water_in_DescLoH/1e3)/((Dc_lh^4)*A_lh_des);
P_in_lh_des = (P_in_lh_asc^(4*tubular_compliance+1)+B1*B2_lh_des*(1-exp(-A_lh_des*L_lh_des)))^(1/(4*tubular_compliance+1));
P_in_lh_des_mmHg = (P_in_lh_des+P_interstitial)/mmHg_Nperm2_conv;
#################  PT 
A_na = Na_reabs_per_unit_length; 
flow_integral_pt = (SN_filtered_Na_load/A_na)*(1-exp(-A_na*L_pt));
B2_pt = (Pc_pt^(4*tubular_compliance))/(Dc_pt^4);
B3_pt = (SNGFR_nL_min / 1e12)/SN_filtered_Na_load;
P_in_pt= (P_in_lh_des^(4*tubular_compliance+1)+B1*B2_pt*B3_pt*flow_integral_pt)^(1/(4*tubular_compliance+1));
P_in_pt_mmHg = (P_in_pt+P_interstitial)/mmHg_Nperm2_conv;
####################################### Renin Angiotensin Aldosterone System ####################################### 
###Aldosterone is secreted in response to AT1-bound AngII and changes in potassium or sodium concentration
#Potassium concentration is treated as a constant
#AT1-bound AngII effect on Aldosterone
AT1_aldo_int = 1 - AT1_aldo_slope*nominal_equilibrium_AT1_bound_AngII;
AngII_effect_on_aldo = AT1_aldo_int + AT1_aldo_slope*AT1_bound_AngII;
#Normalized aldosterone 
normalized_aldosterone_level = 1*(K_Na_ratio_effect_on_aldo * AngII_effect_on_aldo );
###Renin is secreted in response to decreases in AT1-bound AngII and decreases in MD sodium flow
#Macula Densa Sodium flow effect on renin secretion
#This relationship is known to be non-linear, and md_renin_tau can be calibrated based on data on changes in renin as a functoin of sodium intake
md_effect_on_renin_secretion = md_renin_A*exp(-md_renin_tau*(SN_macula_densa_Na_flow_delayed*baseline_nephrons - nom_LoH_Na_outflow));
#AT1-bound AngII feedback on renin secretion
AT1_bound_AngII_effect_on_PRA = (10 ^ (AT1_PRC_slope * log10(AT1_bound_AngII / nominal_equilibrium_AT1_bound_AngII) + AT1_PRC_yint));
#Aldo effect on renin secretion
aldo_renin_intercept = 1-aldo_renin_slope;
aldo_effect_on_renin_secretion =  aldo_renin_slope * normalized_aldosterone_level + aldo_renin_intercept;
#Plasma renin activity
plasma_renin_activity = concentration_to_renin_activity_conversion_plasma* plasma_renin_concentration*DRI_effect_on_PRA;
#Renin secretion
renin_secretion_rate = (log(2)/renin_half_life)*nominal_equilibrium_PRC*AT1_bound_AngII_effect_on_PRA*md_effect_on_renin_secretion*HCTZ_effect_on_renin_secretion*aldo_effect_on_renin_secretion;
#RAAS degradation rates
renin_degradation_rate = log(2)/renin_half_life; 
AngI_degradation_rate = log(2)/AngI_half_life;
AngII_degradation_rate = log(2)/AngII_half_life;
AT1_bound_AngII_degradation_rate =  log(2)/AT1_bound_AngII_half_life;
AT2_bound_AngII_degradation_rate = log(2)/AT2_bound_AngII_half_life;
#RAAS rate constants
ACE_activity = nominal_ACE_activity*(1 - pct_target_inhibition_ACEi);
chymase_activity = nominal_chymase_activity;
AT1_receptor_binding_rate = nominal_AT1_receptor_binding_rate*(1 - pct_target_inhibition_ARB);
AT2_receptor_binding_rate = nominal_AT2_receptor_binding_rate;

###################
IC50_IL1b_act_mcg = 8.9006e-8*392.464*1e6
IL1b_by_immune = a_il1b*(basalil1 + prod_il1b_dat1  + prod_il1b_dat2 + prod_il1b_m1 + prod_il1b_dc)
TNFa_by_immune = a_tnf*(basal_tnfa + prod_tnf_m1+  prod_tnf_dat2 + prod_tnf_dat1  + prod_tnf_th1 + prod_tnf_th17)

AC=IL10_pg_mL+TGFb_pg_mL

IL1b_by_CFU_lung = k_in_IL1b*((Smax_CFU_IL1b*CFU_lung/(SC50_CFU_IL1b+CFU_lung))-I_AC_IL1b*AC)/mw.il1b
TNFa_by_CFU_lung= k_in_TNFa*((Smax_CFU_TNFa*CFU_lung/(SC50_CFU_TNFa+CFU_lung))-I_AC_TNFa*AC)/mw.tnfa 
####
kSR_lung = (kg-k_death)*(CFU_lung+R_lung)/Bmax   #Nielsen EI et al.(2011)
##### 


 

#vancomycin specific factor
  fu_pl_vanco = 0.45
  CL_renal_vanco = 5.73*(GFR_ml_min/105)
  Blood_plasma_ratio_vanco = 0.55
  CL_ad_vanco = 1.05
####PD equation of antibiotics
DRUG_vanco_lung = (Emax_vanco*C_vas_lu_vanco^gamma_vanco)/(C_vas_lu_vanco^gamma_vanco + EC50_vanco^gamma_vanco)
DRUG_vanco_blood = (Emax_vanco*C_venous_vanco^gamma_vanco)/(C_venous_vanco^gamma_vanco + EC50_vanco^gamma_vanco)
   #Nielsen EI et al.(2011)
##
MPPGL = 45						# mg microsomal protein per g liver
HLM_CLint = 0.18*126.27/1000 #(Emoto et al., 2018)
CLmet = (HLM_CLint)*MPPGL*V_liver*60/1000	# CLint scaled (L/hr)


######################################################## ODEs ############################################################# 
#RAAS Pathway
d/dt(AngI) = plasma_renin_activity - (AngI) * (chymase_activity + ACE_activity) - (AngI) * AngI_degradation_rate; 
d/dt(AngII) = AngI * (chymase_activity + ACE_activity) - AngII * AngII_degradation_rate - AngII*AT1_receptor_binding_rate - AngII* (AT2_receptor_binding_rate);
d/dt(AT1_bound_AngII) = AngII * (AT1_receptor_binding_rate) - AT1_bound_AngII_degradation_rate*AT1_bound_AngII;
d/dt(AT2_bound_AngII) = AngII * (AT2_receptor_binding_rate) - AT2_bound_AngII_degradation_rate*AT2_bound_AngII;
d/dt(plasma_renin_concentration) = renin_secretion_rate - plasma_renin_concentration * renin_degradation_rate;
#Change in Extracellular fluid volume over time is determined by the different between water intake and urine outflow
d/dt(blood_volume_L) = C_water_intake_ecf_volume * (water_intake) + C_urine_flow_ecf_volume * (urine_flow_rate) + Q_water*(Na_concentration - ECF_Na_concentration);
d/dt(extracellular_fluid_volume) = Q_water*(ECF_Na_concentration - Na_concentration);
#Change in total body sodium over time is determined by the different between sodium intake and excretion
d/dt(sodium_amount) = C_na_excretion_na_amount * (Na_excretion_via_urine) + C_na_intake_na_amount * (Na_intake_rate) + Q_Na*(ECF_Na_concentration - Na_concentration);
d/dt(ECF_sodium_amount) = Q_Na*(Na_concentration - ECF_Na_concentration);
#These equations serve only to delay the input variable by one timestep. This allows the previous value of the input variable to be used in an equation that appears 
#in the code before the input variable was defined
d/dt(tubulo_glomerular_feedback_effect) = C_tgf * (tubulo_glomerular_feedback_signal-tubulo_glomerular_feedback_effect);
d/dt(normalized_aldosterone_level_delayed) =  C_aldo_secretion * (normalized_aldosterone_level-normalized_aldosterone_level_delayed);
d/dt(preafferent_pressure_autoreg_signal) = 500*(preafferent_pressure_autoreg_function - preafferent_pressure_autoreg_signal);
d/dt(glomerular_pressure_autoreg_signal) = 500*(glomerular_pressure_autoreg_function - glomerular_pressure_autoreg_signal);
d/dt(cardiac_output_delayed) = C_cardiac_output_delayed*(cardiac_output - cardiac_output_delayed);
#Error signals for PI controllers of cardiac output and sodium concentration
d/dt(CO_error) = C_co_error*(cardiac_output-CO_nom);
d/dt(Na_concentration_error) = C_Na_error*(Na_concentration - ref_Na_concentration);
#This equation allows a delay between the secretion of vasopression and its effect on water intake and tubular water reabsorption
d/dt(normalized_vasopressin_concentration_delayed)= C_vasopressin_delay*(normalized_vasopressin_concentration - normalized_vasopressin_concentration_delayed);
#TGF resetting. If C_tgf_reset = 0, no TGF resetting occurs. If it is greater than zero, the setpoint will change over time and will eventually
#come to equal the ambient MD sodium flow rate.
d/dt(F0_TGF) = C_tgf_reset*(SN_macula_densa_Na_flow*baseline_nephrons - F0_TGF);
#As above, these equations allow a variable to be used in equations that appear in the code before the variable was first defined.
d/dt(P_bowmans) = C_P_bowmans*(P_in_pt_mmHg - P_bowmans);
d/dt(oncotic_pressure_difference) = C_P_oncotic*(oncotic_pressure_avg - oncotic_pressure_difference);
d/dt(SN_macula_densa_Na_flow_delayed) = C_md_flow*(SN_macula_densa_Na_flow - SN_macula_densa_Na_flow_delayed);
#Serum Creatinine
d/dt(serum_creatinine) = creatinine_synthesis_rate - creatinine_clearance_rate;
#############################################infection model######################################################## 
d/dt(CFU_lung) = kg*CFU_lung-CFU_lung*(N*k_dN +(M1+pDC)*k_dNS1+NS2*k_dNS2+k_death+DRUG_vanco_lung)-kSR_lung*CFU_lung
d/dt(R_lung) = kSR_lung*CFU_lung - k_death*R_lung
d/dt(CINC1) = S_TNFa*TNFa*mw.tnfa-k_out_CINC1*CINC1
d/dt(Lag1) = kt_lag*CINC1-kt_lag*Lag1
d/dt(Lag2) = kt_lag*Lag1-kt_lag*Lag2
d/dt(NS1) = k_t_NS*N-k_t_NS*NS1
d/dt(NS2) = k_t_NS*NS1-k_t_NS*NS2

############################lung cell
d/dt(AT2) = growth_AT2  - damage_ROS_AT2*AT2 - deg_AT2 - damage_cyt_AT*AT2 - diff_AT2;
d/dt(AT1) = growth_AT1 - damage_ROS_AT1 - deg_AT1 - damage_cyt_AT*AT1 + diff_AT2 ;
d/dt(dAT1) = damage_ROS_AT1 - deg_dAT1 + damage_cyt_AT*AT1;
d/dt(dAT2) = damage_ROS_AT2*(AT2)  - deg_dAT2 + damage_cyt_AT*AT2;
######################################Immune systems model##############
d/dt(pDC) = (a_DC*(kbasal_DC+ k_dAT*log((dAT1+dAT2)))*(1+act_pDC_IL6 + act_pDC_TNFa+ act_pDC_GMCSF)*inh_pDC_IL10 - b_DC*pDC - tr_pDC*vol_alv_ml)/1 ;
d/dt(M1) = (a_M1*(kbasal_M1+ k_dAT*log((dAT1+dAT2)))*(1+act_M1_TNFa + act_M1_GMCSF)*inh_M1_IL10 - b_M1*M1 - tr_M1*vol_alv_ml)/1 ;
d/dt(N)= (k_in_N+ a_N*kbasal_N + (a_N*(k_dAT*log((dAT1+dAT2)))*(1+ act_N_TNFa + act_N_GMCSF) + rec_N_IL17c + k_in_N*(S_IL1b_N*IL1b*mw.il1b+S_CINC1*Lag2)) - b_N*N - tr_N*vol_alv_ml)/1;
d/dt(Th1) = (a_Th1*pDC*(act_Th1_IL12)*inh_Th1_IL10_TGFb + diff_Th1_Th17 + 1*diff_Th1_Treg - b_Th1*Th1- tr_Th1*vol_alv_ml)/1;
d/dt(Th17) = (a_Th17*pDC*(act_TH17_TGFb + act_Th17_IL6 + act_Th17_IL1b) - diff_Th1_Th17 - b_Th17*Th17 - tr_Th17*vol_alv_ml)/1;
d/dt(Treg) = (a_Treg*pDC*(act_Treg_IL2 + act_Treg_TGFb) - diff_Th1_Treg - b_Treg*Treg - tr_Treg*vol_alv_ml)/1 ;    

d/dt(IL6) = (a_il6*(basalil6 +  prod_il6_dat1 + prod_il6_dat2 + prod_il6_m1 + prod_il6_th17 + prod_il6_neu) - deg_il6 - tr_IL6*vol_alv_ml)/1 ;
d/dt(IL1b) = (IL1b_by_immune + IL1b_by_CFU_lung - deg_il1b - tr_IL1b)/1 ;
d/dt(TNFa) = (TNFa_by_immune + TNFa_by_CFU_lung - deg_tnf- tr_TNFa)/1 ;
d/dt(IL2) = (a_il2*(basalil2 +prod_il2_dc + prod_il2_th1) - deg_il2 - tr_IL2)/1;
d/dt(IL12) = (a_il12*(basalil12 +prod_il12_m1 + prod_il12_dc) - deg_il12 - tr_IL12)/1 ;
d/dt(IL17) = (a_il17*(prod_il17_th17) - deg_il17 - tr_IL17)/1;
d/dt(IL10) = (a_il10*(basalil10 + prod_il10_treg) - deg_il10- tr_IL10)/1;
d/dt(TGFb) = (a_tgfb*(basaltgfb + prod_tgfb_th17 + prod_tgfb_treg) - deg_tgfb- tr_TGFb)/1;
d/dt(GMCSF) = (a_gmcsf*(basalgmcsf + prod_gmcsf_m1 + prod_gmcsf_th1 + prod_gmcsf_th17) - deg_gmcsf- tr_GMCSF)/1;

###Central compartment cytokines (pmol/mL)
d/dt(TNFa_c) = (tr_TNFa)*vol_alv_ml/vol_plasma - b_tnf*TNFa_c;
d/dt(IL6_c) = tr_IL6*vol_alv_ml/vol_plasma - b_il6*IL6_c;
d/dt(IL1b_c) = (tr_IL1b)*vol_alv_ml/vol_plasma - b_il1b*IL1b_c;
d/dt(IL2_c) = tr_IL2*vol_alv_ml/vol_plasma - b_il2*IL2_c;
d/dt(IL12_c) = tr_IL12*vol_alv_ml/vol_plasma - b_il12*IL12_c;
d/dt(IL17_c) = tr_IL17*vol_alv_ml/vol_plasma - b_il17*IL17_c;
d/dt(IL10_c) = tr_IL10*vol_alv_ml/vol_plasma - b_il10*IL10_c;
d/dt(TGFb_c) = tr_TGFb*vol_alv_ml/vol_plasma - b_tgfb*TGFb_c;
d/dt(GMCSF_c) = tr_GMCSF*vol_alv_ml/vol_plasma - b_gmcsf*GMCSF_c;

d/dt(CRPExtracellular) = prod_CRP_liver + tr_CRP*V_liver*1000 - kdeg_CRP*CRPExtracellular;
d/dt(Blood_CRP) = (-tr_CRP + prod_CRP_blood - kdeg_CRP*Blood_CRP);

###Central compartment cells (# cells/uL)
d/dt(pDC_c) =  tr_pDC*vol_alv_ml/vol_plasma - pDC_c*b_DC;
d/dt(M1_c) =  tr_M1*vol_alv_ml/vol_plasma - M1_c*b_M1;
d/dt(N_c) = tr_N*vol_alv_ml/vol_plasma  + (a_N*(k_dAT*(dAT1+dAT2)/(km_dAT + dAT1+dAT2))*(1+ act_N_TNFa + act_N_GMCSF) + rec_N_IL17c + k_in_N*(S_IL1b_N*IL1b*mw.il1b+S_CINC1*Lag2))/vol_plasma  - N_c*b_N;
d/dt(Th1_c) =  tr_Th1*vol_alv_ml/vol_plasma - Th1_c*b_Th1;
d/dt(Th17_c) =  tr_Th17*vol_alv_ml/vol_plasma - Th17_c*b_Th17;
d/dt(Treg_c) =  tr_Treg*vol_alv_ml/vol_plasma - Treg_c*b_Treg;


d/dt(depot) = -KA*depot
rateDepot = 1000
rate(depot) = rateDepot

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

#################
d/dt(C_venous_vanco) = (1/V_venous)*(KA*depot + C_vas_ad_vanco*Q_adipose*Blood_plasma_ratio_vanco/0.0009 + C_vas_bo_vanco*Q_bone*Blood_plasma_ratio_vanco + C_vas_br_vanco*Q_brain*Blood_plasma_ratio_vanco  + C_vas_he_vanco*Q_heart*Blood_plasma_ratio_vanco + C_vas_ki_vanco*Q_kidney*Blood_plasma_ratio_vanco + C_vas_li_vanco*Q_liver*Blood_plasma_ratio_vanco + C_vas_mu_vanco*Q_muscle*Blood_plasma_ratio_vanco + C_vas_sk_vanco*Q_skin*Blood_plasma_ratio_vanco + C_vas_go_vanco*Q_gonads*Blood_plasma_ratio_vanco + C_vas_re_vanco*Q_remain*Blood_plasma_ratio_vanco - cardiac_output*60*C_venous_vanco) 
d/dt(C_vas_lu_vanco) = (1/V_lung)*(cardiac_output*60*C_venous_vanco-cardiac_output*60*C_vas_lu_vanco*Blood_plasma_ratio_vanco)
d/dt(C_vas_ad_vanco) = (1/V_adipose)*(Q_adipose*C_arterial_vanco-Q_adipose*C_vas_ad_vanco*Blood_plasma_ratio_vanco/0.0009)
d/dt(C_vas_bo_vanco) = (1/V_bone)*(Q_bone*C_arterial_vanco-Q_bone*C_vas_bo_vanco*Blood_plasma_ratio_vanco)
d/dt(C_vas_br_vanco) = (1/V_brain)*(Q_brain*C_arterial_vanco-Q_brain*C_vas_br_vanco*Blood_plasma_ratio_vanco)
d/dt(C_vas_gu_vanco) = (1/V_gut)*(Q_gut*C_arterial_vanco-Q_gut*C_vas_gu_vanco*Blood_plasma_ratio_vanco)
d/dt(C_vas_he_vanco) = (1/V_heart)*(Q_heart*C_arterial_vanco-Q_heart*C_vas_he_vanco*Blood_plasma_ratio_vanco)
d/dt(C_vas_ki_vanco) = (1/V_kidney)*(Q_kidney*C_arterial_vanco-Q_kidney*C_vas_ki_vanco*Blood_plasma_ratio_vanco-CL_renal_vanco*C_vas_ki_vanco*fu_pl_vanco) 
d/dt(C_vas_li_vanco) = (1/V_liver)*(Qha*C_arterial_vanco+Q_gut*C_vas_gu_vanco*Blood_plasma_ratio_vanco+Q_spleen*C_vas_sp_vanco*Blood_plasma_ratio_vanco-Q_liver*C_vas_li_vanco*Blood_plasma_ratio_vanco-C_vas_li_vanco*fu_pl_vanco*CLmet)
d/dt(C_vas_mu_vanco) = (1/V_muscle)*(Q_muscle*C_arterial_vanco-Q_muscle*C_vas_mu_vanco*Blood_plasma_ratio_vanco)
d/dt(C_vas_sk_vanco) = (1/V_skin)*(Q_skin*C_arterial_vanco-Q_skin*C_vas_sk_vanco*Blood_plasma_ratio_vanco)
d/dt(C_vas_sp_vanco) = (1/V_spleen)*(Q_spleen*C_arterial_vanco-Q_spleen*C_vas_sp_vanco*Blood_plasma_ratio_vanco)
d/dt(C_vas_go_vanco) = (1/V_gonads)*(Q_gonads*C_arterial_vanco-Q_gonads*C_vas_go_vanco*Blood_plasma_ratio_vanco)
d/dt(C_vas_re_vanco) = (1/V_remain)*(Q_remain*C_arterial_vanco-Q_remain*C_vas_re_vanco*Blood_plasma_ratio_vanco-CL_ad_vanco*C_vas_re_vanco*fu_pl_vanco)
d/dt(C_arterial_vanco) = (1/V_arterial)*(cardiac_output*60*C_vas_lu_vanco*Blood_plasma_ratio_vanco-cardiac_output*60*C_arterial_vanco)



"

save(ode, file = "model_struct.saved")



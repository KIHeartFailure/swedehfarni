

# Variables for tabs/mods -------------------------------------------------

tabvars <- c(
  # demo
  "shf_indexyear",
  "shf_sex",
  "shf_age",
  "shf_age_cat",

  # organizational
  "shf_location",
  "sos_timeprevhosphf",
  "sos_timeprevhosphf_cat",
  "sos_timeprevhosphf_cat2",
  "shf_followuphfunit", "shf_followuplocation_cat",

  # clinical factors and lab measurments
  "shf_ef",
  "shf_durationhf",
  "shf_nyha",
  "shf_nyha_cat",
  "shf_bmi",
  "shf_bmi_cat",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_map_cat",
  "shf_heartrate",
  "shf_heartrate_cat",
  "shf_gfrckdepi",
  "shf_gfrckdepi_cat",
  "shf_potassium",
  "shf_potassium_cat",
  "shf_hb",
  "shf_ntprobnp",
  "shf_ntprobnp_cat",

  # treatments
  "sos_lm_previousrasi",
  "shf_rasi",
  "shf_rasidosetarget_cat",
  "shf_mra",
  "shf_mradosetarget_cat",
  "shf_digoxin",
  "shf_diuretic",
  "shf_nitrate",
  "shf_asaantiplatelet",
  "shf_anticoagulantia",
  "shf_statin",
  "shf_bbl",
  "shf_bbldosetarget_cat",
  "shf_device_cat",
  "shf_sglt2",
  "shf_sglt2dosetarget_cat",

  # comorbs
  "shf_smoke_cat",
  "shf_sos_com_diabetes",
  "shf_sos_com_hypertension",
  "shf_sos_com_ihd",
  "sos_com_pad",
  "sos_com_stroke",
  "shf_sos_com_af",
  "shf_anemia",
  "sos_com_valvular",
  "sos_com_liver",
  "sos_com_cancer3y",
  "sos_com_copd",
  "sos_com_renal",
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat",

  # socec
  "scb_famtype",
  "scb_child",
  "scb_education",
  "scb_dispincome_cat",
  "shf_qol",
  "shf_qol_cat"
)

tabvarspop1 <- tabvars[!tabvars %in% c("shf_location")]
tabvarspop3 <- tabvars[!tabvars %in% c("shf_durationhf")]

# vars fox log reg and cox reg
tabvars_not_in_mod <- c(
  "shf_age",
  "sos_timeprevhosphf",
  "sos_timeprevhosphf_cat",
  "shf_nyha",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_heartrate",
  "shf_gfrckdepi",
  "shf_hb",
  "shf_ntprobnp",
  "shf_potassium",
  "shf_bmi",
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat",
  "shf_sglt2",
  "shf_rasi",
  "shf_qol",
  "shf_qol_cat", 
  
  "shf_rasidosetarget_cat",
  "shf_mradosetarget_cat",
  "shf_bbldosetarget_cat",
  "shf_sglt2dosetarget_cat"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]

modvarspop1 <- modvars[!modvars %in% c("shf_location")]
modvarspop3 <- modvars[!modvars %in% c("shf_durationhf", "sos_timeprevhosphf_cat2")]

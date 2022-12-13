

# Additional variables from mainly SHF ------------------------------------

targetdose <- data.frame(matrix(c(
  "Captopril", 50 * 3,
  "Enalapril", 10 * 2,
  "Fosinopril", "-",
  "Lisinopril", 20,
  "Ramipril", 5 * 2,
  "Candesartan", 32,
  "Eprosartan", "-",
  "Irbesartan", "-",
  "Losartan", 150,
  "Telmisartan", "-",
  "Valsartan", 160 * 2,
  "Atenolol", "-",
  "Bisoprolol", 10,
  "Carvedilol", 25 * 2,
  "Labetalol", "-",
  "Metoprolol", 200,
  "Pindolol", "-",
  "Propanolol", "-",
  "Sotalol", "-",
  "Eplerenon", 50,
  "Spironalakton", 50,
  "Dapagliflozin", 10,
  "Empagliflozin", 10,
  "Ertugliflozin", "-"
), ncol = 2, byrow = T))
colnames(targetdose) <- c("Substance", "Daily dose")


# ntprobnp

mednt <- median(rsdata$shf_ntprobnp, na.rm = T)

# income

medinc <- median(rsdata$scb_dispincome, na.rm = T)

rsdata <- rsdata %>%
  mutate(
    sos_lm_arni14num = if_else(sos_lm_arni14 == "No", 0, 1),
    scb_dispincome_cat = factor(
      case_when(
        scb_dispincome < medinc ~ 1,
        scb_dispincome >= medinc ~ 2
      ),
      levels = 1:2,
      labels = c("<median", ">=median")
    ),
    shf_ntprobnp_cat = factor(
      case_when(
        shf_ntprobnp < mednt ~ 1,
        shf_ntprobnp >= mednt ~ 2
      ),
      levels = 1:2,
      labels = c("<median", ">=median")
    ),
    shf_rasi = case_when(
      is.na(shf_arb) | is.na(shf_acei) ~ NA_character_,
      shf_arb == "Yes" | shf_acei == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    sos_com_charlsonci_cat = factor(
      case_when(
        sos_com_charlsonci <= 1 ~ 1,
        sos_com_charlsonci <= 3 ~ 2,
        sos_com_charlsonci <= 7 ~ 3,
        sos_com_charlsonci >= 8 ~ 4
      ),
      levels = 1:4,
      labels = c(
        "0-1",
        "2-3",
        "4-7",
        ">=8"
      )
    ),
    sos_timeprevhosphf_cat = factor(
      case_when(
        is.na(sos_timeprevhosphf) ~ 0,
        sos_timeprevhosphf <= 30 ~ 1,
        sos_timeprevhosphf <= 180 ~ 2,
        sos_timeprevhosphf <= 365 ~ 3,
        sos_timeprevhosphf > 365 ~ 4
      ),
      levels = 0:4,
      labels = c("No", "<=30", "31-180", "181-365", ">365")
    ),
    sos_timeprevhosphf_cat2 = factor(
      case_when(
        is.na(sos_timeprevhosphf) ~ 0,
        sos_timeprevhosphf <= 365 ~ 1,
        sos_timeprevhosphf > 365 ~ 2
      ),
      levels = 0:2,
      labels = c("No", "<=365", ">365")
    ),
    shf_qol_cat = factor(
      case_when(
        shf_qol <= 25 ~ 1,
        shf_qol <= 50 ~ 2,
        shf_qol <= 75 ~ 3,
        shf_qol <= 100 ~ 4,
      ),
      levels = 1:4,
      labels = c("0-25", "26-50", "51-75", "76-100")
    ),

    # Doses
    shf_aceidosetarget = case_when(
      shf_aceisub == "Captopril" ~ shf_aceidose / (50 * 3),
      shf_aceisub == "Enalapril" ~ shf_aceidose / (10 * 2),
      shf_aceisub == "Fosinopril" ~ NA_real_,
      shf_aceisub == "Lisinopril" ~ shf_aceidose / 20,
      shf_aceisub == "Ramipril" ~ shf_aceidose / (5 * 2)
    ),
    shf_arbdosetarget = case_when(
      shf_arbsub == "Candesartan" ~ shf_arbdose / 32,
      shf_arbsub == "Eprosartan" ~ NA_real_,
      shf_arbsub == "Irbesartan" ~ NA_real_,
      shf_arbsub == "Losartan" ~ shf_arbdose / 150,
      shf_arbsub == "Telmisartan" ~ NA_real_,
      shf_arbsub == "Valsartan" ~ shf_arbdose / (160 * 2)
    ),
    shf_rasidosetarget = pmax(shf_aceidosetarget, shf_arbdosetarget, na.rm = T),
    shf_rasidosetarget_cat = factor(
      case_when(
        shf_rasidosetarget < .5 ~ 1,
        shf_rasidosetarget <= .99 ~ 2,
        shf_rasidosetarget > .99 ~ 3
      ),
      levels = 1:3,
      labels = c("<50%", "50-99%", ">=100%")
    ),
    shf_bbldosetarget = case_when(
      shf_bblsub == "Atenolol" ~ NA_real_,
      shf_bblsub == "Bisoprolol" ~ shf_bbldose / 10,
      shf_bblsub == "Carvedilol" ~ shf_bbldose / (25 * 2),
      shf_bblsub == "Labetalol" ~ NA_real_,
      shf_bblsub == "Metoprolol" ~ shf_bbldose / 200,
      shf_bblsub == "Pindolol" ~ NA_real_,
      shf_bblsub == "Propanolol" ~ NA_real_,
      shf_bblsub == "Sotalol" ~ NA_real_
    ),
    shf_bbldosetarget_cat = factor(
      case_when(
        shf_bbldosetarget < .5 ~ 1,
        shf_bbldosetarget <= .99 ~ 2,
        shf_bbldosetarget > .99 ~ 3
      ),
      levels = 1:3,
      labels = c("<50%", "50-99%", ">=100%")
    ),
    shf_mradosetarget = case_when(
      shf_mrasub == "Eplerenon" ~ shf_mradose / 50,
      shf_mrasub == "Spironalakton" ~ shf_mradose / 50
    ),
    shf_mradosetarget_cat = factor(
      case_when(
        shf_mradosetarget < .5 ~ 1,
        shf_mradosetarget <= .99 ~ 2,
        shf_mradosetarget > .99 ~ 3
      ),
      levels = 1:3,
      labels = c("<50%", "50-99%", ">=100%")
    ),
    shf_sglt2dosetarget = case_when(
      shf_sglt2sub == "Dapagliflozin" ~ shf_sglt2dose / 10,
      shf_sglt2sub == "Empagliflozin" ~ shf_sglt2dose / 10,
      shf_sglt2sub == "Ertugliflozin" ~ NA_real_
    ),
    shf_sglt2dosetarget_cat = factor(
      case_when(
        shf_sglt2dosetarget < .5 ~ 1,
        shf_sglt2dosetarget <= .99 ~ 2,
        shf_sglt2dosetarget > .99 ~ 3
      ),
      levels = 1:3,
      labels = c("<50%", "50-99%", ">=100%")
    ),

    # Outcomes

    sos_outtime_death30 = sos_outtime_death - 30,
    sos_outtime_death = sos_outtime_death - 14,
    
    sos_out_deathcvhosphf = ifelse(sos_out_deathcv == "Yes" | sos_out_hosphf == "Yes", "Yes", "No"),
    sos_out_deathcvhosphf30 = ifelse(sos_out_deathcv == "Yes" | sos_out_hosphf30 == "Yes", "Yes", "No"),
    
    sos_out_countdeathcvhosphf = ifelse(sos_out_deathcv == "Yes",
      sos_out_counthosphf + 1,
      sos_out_counthosphf
    ),
    sos_out_countdeathcvhosphf30 = ifelse(sos_out_deathcv == "Yes",
                                        sos_out_counthosphf30 + 1,
                                        sos_out_counthosphf30
    )
  ) %>%
  mutate(across(where(is.character), as.factor))

rsdata <- rsdatafull410 %>%
  filter(casecontrol == "Case SwedeHF")

nprdata <- rsdatafull410 %>%
  filter(casecontrol == "Case NPR")

rm(rsdatafull410)

# SwedeHF -----------------------------------------------------------------

lmtmp <- left_join(
  rsdata %>%
    select(lopnr, shf_indexdtm, censdtm),
  lmarni,
  by = "lopnr"
)

lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff < 0) %>%
  select(lopnr, shf_indexdtm, EDATUM, ATC)

rsdata <- create_medvar(
  atc = global_atcarni,
  medname = "previousarni", cohortdata = rsdata, meddata = lmtmp2, id = c("lopnr", "shf_indexdtm"),
  metatime = "--1days",
  valsclass = "fac"
)

lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff >= 0, diff <= 14) %>%
  select(lopnr, shf_indexdtm, EDATUM, ATC)

rsdata <- create_medvar(
  atc = global_atcarni,
  medname = "arni14", cohortdata = rsdata, meddata = lmtmp2, id = c("lopnr", "shf_indexdtm"),
  metatime = "0-14days",
  valsclass = "fac"
)

lmtmp2 <- lmtmp %>%
  mutate(
    diff = as.numeric(EDATUM - shf_indexdtm),
    atcneed = str_detect(ATC, global_atcarni)
  ) %>%
  filter(diff >= 0, diff <= 90, atcneed) %>%
  mutate(sos_lm_arni3 = case_when(
    diff >= 0 & diff <= 14 ~ 1,
    diff >= 15 & diff <= 90 ~ 2
  )) %>%
  group_by(lopnr, shf_indexdtm) %>%
  arrange(sos_lm_arni3) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, shf_indexdtm, sos_lm_arni3)

rsdata <- left_join(rsdata,
  lmtmp2,
  by = c("lopnr", "shf_indexdtm")
) %>%
  mutate(sos_lm_arni3 = factor(if_else(is.na(sos_lm_arni3), 0, sos_lm_arni3),
    levels = 0:2, labels = c(
      ">90 days/No",
      "0-14 days",
      "15-90 days"
    )
  ))


lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff < 0, diff >= -180) %>%
  select(lopnr, shf_indexdtm, EDATUM, ATC)

rsdata <- create_medvar(
  atc = "^(C09A|C09B|C09C|C09D(?!X04))",
  medname = "previousrasi", cohortdata = rsdata, meddata = lmtmp2, id = c("lopnr", "shf_indexdtm"),
  metatime = "-180--1days",
  valsclass = "fac"
)

# discontinuation

lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff >= 0 & str_detect(ATC, global_atcarni) & EDATUM <= censdtm) %>%
  select(lopnr, shf_indexdtm, censdtm, EDATUM, ATC)

lmtmp3 <- lmtmp2 %>%
  group_by(lopnr) %>%
  arrange(EDATUM) %>%
  mutate(
    c = 1:n(),
    lagEDATUM = lag(EDATUM),
    diff = as.numeric(EDATUM - lagEDATUM),
    diffcens = as.numeric(censdtm - EDATUM),
    discdtm = case_when(
      c == n() & diffcens >= 152 ~ EDATUM + 90,
      diff > 152 & !c %in% c(1) ~ lagEDATUM + 90
    ),
  ) %>%
  ungroup() %>%
  arrange(lopnr, EDATUM) %>%
  select(lopnr, discdtm)

lmtmp4 <- lmtmp3 %>%
  filter(!is.na(discdtm)) %>%
  group_by(lopnr) %>%
  arrange(discdtm) %>%
  slice(1) %>%
  ungroup()

rsdata <- left_join(rsdata, lmtmp4, by = "lopnr") %>%
  mutate(
    disc = if_else(!is.na(discdtm), 1, 0),
    tmp_sos_outtime_death = sos_outtime_death - 14,
    disctime = if_else(!is.na(discdtm), as.numeric(discdtm - shf_indexdtm), tmp_sos_outtime_death)
  ) %>%
  select(-tmp_sos_outtime_death)

# NPR ---------------------------------------------------------------------

lmtmp <- left_join(
  nprdata %>%
    select(lopnr, shf_indexdtm, censdtm),
  lmarni,
  by = "lopnr"
)

lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff < 0) %>%
  select(lopnr, shf_indexdtm, EDATUM, ATC)

nprdata <- create_medvar(
  atc = global_atcarni,
  medname = "previousarni", cohortdata = nprdata, meddata = lmtmp2, id = c("lopnr"),
  metatime = "--1days",
  valsclass = "fac"
)

lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff >= 0, diff <= 14) %>%
  select(lopnr, shf_indexdtm, EDATUM, ATC)

nprdata <- create_medvar(
  atc = global_atcarni,
  medname = "arni14", cohortdata = nprdata, meddata = lmtmp2, id = c("lopnr"),
  metatime = "0-14days",
  valsclass = "fac"
)

lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff >= -30.5 * 5, diff <= 14) %>%
  select(lopnr, shf_indexdtm, EDATUM, ATC)

nprdata <- create_medvar(
  atc = "^(C09A|C09B|C09C|C09D(?!X04))",
  medname = "rasi", cohortdata = nprdata, meddata = lmtmp2, id = c("lopnr"),
  metatime = "-5mo-14days",
  valsclass = "fac"
)
nprdata <- create_medvar(
  atc = "^C03DA",
  medname = "mra", cohortdata = nprdata, meddata = lmtmp2, id = c("lopnr"),
  metatime = "-5mo-14days",
  valsclass = "fac"
)
nprdata <- create_medvar(
  atc = "^C07",
  medname = "bbl", cohortdata = nprdata, meddata = lmtmp2, id = c("lopnr"),
  metatime = "-5mo-14days",
  valsclass = "fac"
)
nprdata <- create_medvar(
  atc = "^(A10BK01|A10BD15|A10BD21|A10BD25|A10BK03|A10BD19|A10BD20)",
  medname = "sglt2", cohortdata = nprdata, meddata = lmtmp2, id = c("lopnr"),
  metatime = "-5mo-14days",
  valsclass = "fac"
)

# discontinuation

lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff >= 0 & str_detect(ATC, global_atcarni) & EDATUM <= censdtm) %>%
  select(lopnr, shf_indexdtm, censdtm, EDATUM, ATC)

lmtmp3 <- lmtmp2 %>%
  group_by(lopnr) %>%
  arrange(EDATUM) %>%
  mutate(
    c = 1:n(),
    lagEDATUM = lag(EDATUM),
    diff = as.numeric(EDATUM - lagEDATUM),
    diffcens = as.numeric(censdtm - EDATUM),
    discdtm = case_when(
      c == n() & diffcens >= 152 ~ EDATUM + 90,
      diff > 152 & !c %in% c(1) ~ lagEDATUM + 90
    ),
  ) %>%
  ungroup() %>%
  arrange(lopnr, EDATUM) %>%
  select(lopnr, discdtm)

lmtmp4 <- lmtmp3 %>%
  filter(!is.na(discdtm)) %>%
  group_by(lopnr) %>%
  arrange(discdtm) %>%
  slice(1) %>%
  ungroup()

nprdata <- left_join(nprdata, lmtmp4, by = "lopnr") %>%
  mutate(
    disc = if_else(!is.na(discdtm) & sos_lm_arni14 == "Yes", 1, 0),
    tmp_sos_outtime_death = sos_outtime_death - 14,
    disctime = if_else(!is.na(discdtm), as.numeric(discdtm - shf_indexdtm), tmp_sos_outtime_death)
  ) %>%
  select(-tmp_sos_outtime_death)



metalm[, "Register"] <- "Prescribed Drug Register"
metalm <- metalm[c(1, 2, 3, 6, 7, 8, 9), ]

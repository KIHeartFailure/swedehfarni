
# Repeated hf hosp --------------------------------------------------------

svpatreg <- patregrsdata %>%
  filter(sos_source == "sv")

# 14 days

tmpsos <- left_join(
  rsdata %>%
    select(lopnr, shf_indexdtm14, sos_lm_arni14, shf_location, shf_durationhf, parpop1, sos_outtime_death, sos_out_deathcv, censdtm),
  svpatreg,
  by = "lopnr"
) %>%
  mutate(sos_outtime = difftime(INDATUM, shf_indexdtm14, units = "days")) %>%
  filter(sos_outtime > 0 & INDATUM < censdtm)

tmpsos <- tmpsos %>%
  mutate(sos_out_hosphf = stringr::str_detect(HDIA, global_hficd)) %>%
  filter(sos_out_hosphf) %>%
  select(lopnr, shf_indexdtm14, sos_lm_arni14, shf_location, shf_durationhf, parpop1, sos_outtime_death, sos_out_deathcv, censdtm, sos_outtime, sos_out_hosphf)

dataout <- bind_rows(
  rsdata %>%
    select(lopnr, shf_indexdtm14, sos_lm_arni14, shf_location, shf_durationhf, parpop1, sos_outtime_death, sos_out_deathcv, censdtm),
  tmpsos
) %>%
  mutate(
    sos_out_hosphf = if_else(is.na(sos_out_hosphf), 0, 1),
    sos_outtime = as.numeric(if_else(is.na(sos_outtime), difftime(censdtm, shf_indexdtm14, units = "days"), sos_outtime))
  )

dataout <- dataout %>%
  group_by(lopnr, shf_indexdtm14, sos_outtime) %>%
  arrange(desc(sos_out_hosphf)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(lopnr, shf_indexdtm14)

dataout <- dataout %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm14) %>%
  mutate(sos_out_deathcvhosphf = if_else(n() & sos_out_deathcv == "Yes", 1, sos_out_hosphf)) %>%
  ungroup()

extradataout <- dataout %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm14) %>%
  slice(n()) %>%
  ungroup() %>%
  filter(sos_out_deathcvhosphf == 1) %>%
  mutate(sos_out_deathcvhosphf = 0)

rsdatarec <- bind_rows(dataout, extradataout) %>%
  arrange(lopnr, sos_outtime, desc(sos_out_deathcvhosphf))

# 30 days

tmpsos <- left_join(
  rsdata %>%
    select(lopnr, shf_indexdtm30, sos_lm_arni3, pop13, shf_location, sos_outtime_death, sos_out_deathcv, censdtm),
  svpatreg,
  by = "lopnr"
) %>%
  mutate(sos_outtime = difftime(INDATUM, shf_indexdtm30, units = "days")) %>%
  filter(sos_outtime > 0 & INDATUM < censdtm)

tmpsos <- tmpsos %>%
  mutate(sos_out_hosphf = stringr::str_detect(HDIA, global_hficd)) %>%
  filter(sos_out_hosphf) %>%
  select(lopnr, shf_indexdtm30, sos_lm_arni3, pop13, shf_location, sos_outtime_death, sos_out_deathcv, censdtm, sos_outtime, sos_out_hosphf, pop13)

dataout <- bind_rows(
  rsdata %>%
    select(lopnr, shf_indexdtm30, sos_lm_arni3, pop13, shf_location, shf_durationhf, sos_outtime_death, sos_out_deathcv, censdtm),
  tmpsos
) %>%
  mutate(
    sos_out_hosphf = if_else(is.na(sos_out_hosphf), 0, 1),
    sos_outtime = as.numeric(if_else(is.na(sos_outtime), difftime(censdtm, shf_indexdtm30, units = "days"), sos_outtime))
  )

dataout <- dataout %>%
  group_by(lopnr, shf_indexdtm30, sos_outtime) %>%
  arrange(desc(sos_out_hosphf)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(lopnr, shf_indexdtm30)

dataout <- dataout %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm30) %>%
  mutate(sos_out_deathcvhosphf = if_else(n() & sos_out_deathcv == "Yes", 1, sos_out_hosphf)) %>%
  ungroup()

extradataout <- dataout %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm30) %>%
  slice(n()) %>%
  ungroup() %>%
  filter(sos_out_deathcvhosphf == 1) %>%
  mutate(sos_out_deathcvhosphf = 0)

rsdatarecpop13 <- bind_rows(dataout, extradataout) %>%
  arrange(lopnr, sos_outtime, desc(sos_out_deathcvhosphf))

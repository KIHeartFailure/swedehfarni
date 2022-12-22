
# Outcomes ----------------------------------------------------------------

rsdata <- rsdata %>%
  mutate(
    shf_indexdtm14 = shf_indexdtm + 14,
    shf_indexdtm30 = shf_indexdtm + 30
  )

rsdata <- create_sosvar(
  sosdata = patregrsdata %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm14,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "hosphf",
  diakod = global_hficd,
  censdate = censdtm,
  valsclass = "fac",
  warnings = FALSE
)

rsdata <- create_sosvar(
  sosdata = patregrsdata %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm14,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "counthosphf",
  diakod = global_hficd,
  noof = TRUE,
  censdate = censdtm,
  valsclass = "num",
  warnings = FALSE
)

rsdata <- create_sosvar(
  sosdata = patregrsdata %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm30,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "hosphf30",
  diakod = global_hficd,
  censdate = censdtm,
  valsclass = "fac",
  warnings = FALSE
)

rsdata <- create_sosvar(
  sosdata = patregrsdata %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm30,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "counthosphf30",
  diakod = global_hficd,
  noof = TRUE,
  censdate = censdtm,
  valsclass = "num",
  warnings = FALSE
)

# Times since previous HFH

hfhospsos <- patregrsdata %>%
  filter(sos_source == "sv") %>%
  mutate(tmp_hfhospsos = stringr::str_detect(HDIA, global_hficd)) %>%
  filter(tmp_hfhospsos)

hfhosp <- inner_join(
  rsdata %>% select(lopnr, shf_indexdtm),
  hfhospsos,
  by = "lopnr"
) %>%
  mutate(tmp_sosdtm = coalesce(UTDATUM, INDATUM)) %>%
  filter(tmp_sosdtm < shf_indexdtm) %>%
  group_by(lopnr, shf_indexdtm) %>%
  arrange(tmp_sosdtm) %>%
  slice(n()) %>%
  ungroup() %>%
  select(lopnr, shf_indexdtm, tmp_sosdtm)

rsdata <- left_join(
  rsdata %>% select(-sos_timeprevhosphf),
  hfhosp,
  by = c("lopnr", "shf_indexdtm")
) %>%
  mutate(
    sos_timeprevhosphf = as.numeric(shf_indexdtm - tmp_sosdtm)
  ) %>%
  select(-tmp_sosdtm)

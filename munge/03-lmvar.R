

lmtmp <- left_join(
  rsdata401 %>%
    select(lopnr, shf_indexdtm),
  lmsel,
  by = "lopnr"
)

lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff >= 0, diff <= 14) %>%
  select(lopnr, shf_indexdtm, EDATUM, ATC)

rsdata <- create_medvar(
  atc = global_atcarni,
  medname = "arni14", cohortdata = rsdata401, meddata = lmtmp2, id = c("lopnr", "shf_indexdtm"),
  metatime = "0-14days",
  valsclass = "fac"
)

lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff < 0, diff >= -365) %>%
  select(lopnr, shf_indexdtm, EDATUM, ATC)

rsdata <- create_medvar(
  atc = "^(C09A|C09B|C09C|C09D(?!X04))",
  medname = "previousrasi", cohortdata = rsdata, meddata = lmtmp2, id = c("lopnr", "shf_indexdtm"),
  metatime = "-365--1days",
  valsclass = "fac"
)

metalm[, "Register"] <- "Prescribed Drug Register"
# income

medinc <- median(nprdata$scb_dispincome, na.rm = T)

nprdata <- nprdata %>%
  rename(tmp_soslocation = sos_location) %>%
  mutate(
    scb_dispincome_cat = factor(
      case_when(
        scb_dispincome < medinc ~ 1,
        scb_dispincome >= medinc ~ 2
      ),
      levels = 1:2,
      labels = c("<median", ">=median")
    ),
    sos_location = factor(
      case_when(
        tmp_soslocation %in% c("HF in-patient", "Other in-patient") ~ 2,
        tmp_soslocation %in% c("Out-patient") ~ 1
      ),
      levels = 1:2, labels = c("Out-patient", "In-patient")
    )
  ) %>%
  select(-tmp_soslocation)

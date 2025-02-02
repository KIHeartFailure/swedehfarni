
# Overtime graph ----------------------------------------------------------

lmtmp <- left_join(
  rsdata %>%
    select(lopnr, shf_indexdtm),
  lmarni,
  by = "lopnr"
) %>%
  mutate(atcneed = str_detect(ATC, global_atcarni)) %>%
  filter(atcneed)

overtimefunc <- function(year, rsdatapop = rsdata, popname) {
  yearmid <- paste0(year, "-7-01")

  popyear <- rsdatapop %>%
    filter(
      shf_indexdtm <= ymd(yearmid),
      censdtm >= ymd(yearmid)
    )

  lmyear <- inner_join(popyear,
    lmtmp %>%
      filter(AR == year),
    by = "lopnr"
  ) %>%
    group_by(lopnr) %>%
    slice(1) %>%
    ungroup()

  out <- c(pop = popname, year = year, den = popyear %>% count() %>% pull(n), num = lmyear %>% count() %>% pull(n))
}

overtime <- overtimefunc(2017, popname = "All")
overtime <- rbind(overtime, overtimefunc(2018, popname = "All"))
overtime <- rbind(overtime, overtimefunc(2019, popname = "All"))
overtime <- rbind(overtime, overtimefunc(2020, popname = "All"))
overtime <- rbind(overtime, overtimefunc(2021, popname = "All"))

overtime <- rbind(overtime, overtimefunc(year = 2017, rsdatapop = rsdata %>% filter(shf_location == "In-patient"), popname = "In-patient"))
overtime <- rbind(overtime, overtimefunc(2018, rsdata %>% filter(shf_location == "In-patient"), "In-patient"))
overtime <- rbind(overtime, overtimefunc(2019, rsdata %>% filter(shf_location == "In-patient"), "In-patient"))
overtime <- rbind(overtime, overtimefunc(2020, rsdata %>% filter(shf_location == "In-patient"), "In-patient"))
overtime <- rbind(overtime, overtimefunc(2021, rsdata %>% filter(shf_location == "In-patient"), "In-patient"))

overtime <- rbind(overtime, overtimefunc(2017, rsdata %>% filter(shf_location == "Out-patient"), "Out-patient"))
overtime <- rbind(overtime, overtimefunc(2018, rsdata %>% filter(shf_location == "Out-patient"), "Out-patient"))
overtime <- rbind(overtime, overtimefunc(2019, rsdata %>% filter(shf_location == "Out-patient"), "Out-patient"))
overtime <- rbind(overtime, overtimefunc(2020, rsdata %>% filter(shf_location == "Out-patient"), "Out-patient"))
overtime <- rbind(overtime, overtimefunc(2021, rsdata %>% filter(shf_location == "Out-patient"), "Out-patient"))

overtime <- overtime %>%
  as.data.frame() %>%
  mutate(percent = as.numeric(num) / as.numeric(den) * 100)


# Overtime graph sens complete case -------------------------------------------

lmtmp <- left_join(
  rsdata %>%
    filter(senscomp) %>%
    select(lopnr, shf_indexdtm),
  lmarni,
  by = "lopnr"
) %>%
  mutate(atcneed = str_detect(ATC, global_atcarni)) %>%
  filter(atcneed)

overtime <- overtimefunc(2017, popname = "All", rsdatapop = rsdata %>% filter(senscomp))
overtime <- rbind(overtime, overtimefunc(2018, popname = "All", rsdatapop = rsdata %>% filter(senscomp)))
overtime <- rbind(overtime, overtimefunc(2019, popname = "All", rsdatapop = rsdata %>% filter(senscomp)))
overtime <- rbind(overtime, overtimefunc(2020, popname = "All", rsdatapop = rsdata %>% filter(senscomp)))
overtime <- rbind(overtime, overtimefunc(2021, popname = "All", rsdatapop = rsdata %>% filter(senscomp)))

overtime <- rbind(overtime, overtimefunc(year = 2017, rsdatapop = rsdata %>% filter(shf_location == "In-patient" & senscomp), popname = "In-patient"))
overtime <- rbind(overtime, overtimefunc(2018, rsdata %>% filter(shf_location == "In-patient" & senscomp), "In-patient"))
overtime <- rbind(overtime, overtimefunc(2019, rsdata %>% filter(shf_location == "In-patient" & senscomp), "In-patient"))
overtime <- rbind(overtime, overtimefunc(2020, rsdata %>% filter(shf_location == "In-patient" & senscomp), "In-patient"))
overtime <- rbind(overtime, overtimefunc(2021, rsdata %>% filter(shf_location == "In-patient" & senscomp), "In-patient"))

overtime <- rbind(overtime, overtimefunc(2017, rsdata %>% filter(shf_location == "Out-patient" & senscomp), "Out-patient"))
overtime <- rbind(overtime, overtimefunc(2018, rsdata %>% filter(shf_location == "Out-patient" & senscomp), "Out-patient"))
overtime <- rbind(overtime, overtimefunc(2019, rsdata %>% filter(shf_location == "Out-patient" & senscomp), "Out-patient"))
overtime <- rbind(overtime, overtimefunc(2020, rsdata %>% filter(shf_location == "Out-patient" & senscomp), "Out-patient"))
overtime <- rbind(overtime, overtimefunc(2021, rsdata %>% filter(shf_location == "Out-patient" & senscomp), "Out-patient"))

overtime_senscomp <- overtime %>%
  as.data.frame() %>%
  mutate(percent = as.numeric(num) / as.numeric(den) * 100)


# Overtime graph sens missing as normal ---------------------------------------

lmtmp <- left_join(
  rsdata %>%
    filter(sens) %>%
    select(lopnr, shf_indexdtm),
  lmarni,
  by = "lopnr"
) %>%
  mutate(atcneed = str_detect(ATC, global_atcarni)) %>%
  filter(atcneed)

overtime <- overtimefunc(2017, popname = "All", rsdatapop = rsdata %>% filter(sens))
overtime <- rbind(overtime, overtimefunc(2018, popname = "All", rsdatapop = rsdata %>% filter(sens)))
overtime <- rbind(overtime, overtimefunc(2019, popname = "All", rsdatapop = rsdata %>% filter(sens)))
overtime <- rbind(overtime, overtimefunc(2020, popname = "All", rsdatapop = rsdata %>% filter(sens)))
overtime <- rbind(overtime, overtimefunc(2021, popname = "All", rsdatapop = rsdata %>% filter(sens)))

overtime <- rbind(overtime, overtimefunc(year = 2017, rsdatapop = rsdata %>% filter(shf_location == "In-patient" & sens), popname = "In-patient"))
overtime <- rbind(overtime, overtimefunc(2018, rsdata %>% filter(shf_location == "In-patient" & sens), "In-patient"))
overtime <- rbind(overtime, overtimefunc(2019, rsdata %>% filter(shf_location == "In-patient" & sens), "In-patient"))
overtime <- rbind(overtime, overtimefunc(2020, rsdata %>% filter(shf_location == "In-patient" & sens), "In-patient"))
overtime <- rbind(overtime, overtimefunc(2021, rsdata %>% filter(shf_location == "In-patient" & sens), "In-patient"))

overtime <- rbind(overtime, overtimefunc(2017, rsdata %>% filter(shf_location == "Out-patient" & sens), "Out-patient"))
overtime <- rbind(overtime, overtimefunc(2018, rsdata %>% filter(shf_location == "Out-patient" & sens), "Out-patient"))
overtime <- rbind(overtime, overtimefunc(2019, rsdata %>% filter(shf_location == "Out-patient" & sens), "Out-patient"))
overtime <- rbind(overtime, overtimefunc(2020, rsdata %>% filter(shf_location == "Out-patient" & sens), "Out-patient"))
overtime <- rbind(overtime, overtimefunc(2021, rsdata %>% filter(shf_location == "Out-patient" & sens), "Out-patient"))

overtime_sens <- overtime %>%
  as.data.frame() %>%
  mutate(percent = as.numeric(num) / as.numeric(den) * 100)


rm(lmarni)


# Overtime graph ----------------------------------------------------------

lmtmp <- left_join(
  rsdata %>%
    select(lopnr, shf_indexdtm),
  lmsel,
  by = "lopnr"
) %>%
  mutate(atcneed = str_detect(ATC, global_atcarni)) %>%
  filter(atcneed)

overtimefunc <- function(year){
  yearmid <- paste0(year, "-7-01")
  
  popyear <- rsdata %>%
    filter(shf_indexdtm <= ymd(yearmid),
           censdtm >= ymd(yearmid))
  
  lmyear <- inner_join(popyear, 
                       lmtmp %>%
                         filter(AR == year), 
                       by = "lopnr") %>%
    group_by(lopnr) %>%
    slice(1) %>%
    ungroup()
  
  out <- c(year = year, den = popyear %>% count() %>% pull(n), num = lmyear %>% count() %>% pull(n))
}

overtime <- overtimefunc(2017)
overtime <- rbind(overtime, overtimefunc(2018))
overtime <- rbind(overtime, overtimefunc(2019))
overtime <- rbind(overtime, overtimefunc(2020))
overtime <- rbind(overtime, overtimefunc(2021))

overtime <- overtime %>%
  as.data.frame() %>%
  mutate(percent = num / den * 100) 
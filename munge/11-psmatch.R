

# Propensity scores -------------------------------------------------------

ps <- as_tibble(
  matrix(NA,
    nrow =
      nrow(rsdata %>% filter(shf_location == "In-patient")),
    ncol = 11
  ),
  .name_repair = "universal"
)

for (i in 1:10) {
  imrsdata_ps <- mice::complete(imprsdata, i)
  imrsdata_ps <- imrsdata_ps %>% filter(shf_location == "In-patient")
  if (i == 1) ps[, 1] <- imrsdata_ps$lopnr
  pslog <- glm(
    formula(paste0(
      "sos_lm_arni14num ~ ",
      paste(modvarspop1,
        collapse = " + "
      )
    )),
    data = imrsdata_ps,
    family = binomial
  )
  ps[, i + 1] <- pslog$fitted
}

rsdata <- left_join(rsdata,
  ps %>%
    mutate(ps = rowSums(.[2:11]) / 10) %>%
    select(...1, ps),
  by = c("lopnr" = "...1")
)
rsdataps <- rsdata %>%
  filter(shf_location == "In-patient")
cal <- c(0.01 / sd(rsdataps$ps))

set.seed(2334325)
match1 <- Match(
  Tr = rsdataps$sos_lm_arni14num,
  X = rsdataps$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 1
)
set.seed(2334325)
match2 <- Match(
  Tr = rsdataps$sos_lm_arni14num,
  X = rsdataps$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 2
)
set.seed(2334325)
match3 <- Match(
  Tr = rsdataps$sos_lm_arni14num,
  X = rsdataps$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 3
)
set.seed(2334325)
match4 <- Match(
  Tr = rsdataps$sos_lm_arni14num,
  X = rsdataps$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 4
)
set.seed(2334325)
match5 <- Match(
  Tr = rsdataps$sos_lm_arni14num,
  X = rsdataps$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 5
)
matchingn <- paste0(
  "1:1: N = ", match1$wnobs, ", ",
  "1:2: N = ", match2$wnobs, ", ",
  "1:3: N = ", match3$wnobs, ", ",
  "1:4: N = ", match4$wnobs, ", ",
  "1:5: N = ", match5$wnobs
)

rsdataps$parpop1 <- rep(NA, nrow(rsdataps))
rsdataps$parpop1[c(unique(match1$index.treated), match1$index.control)] <- c(1:match1$wnobs, rep(1:match1$wnobs, each = 1))
rsdata <- left_join(rsdata, rsdataps %>% select(lopnr, parpop1), by = "lopnr")

# check ps match
koll <- left_join(rsdata %>% filter(!is.na(parpop1) & sos_lm_arni14 == "Yes"),
  rsdata %>% filter(!is.na(parpop1) & sos_lm_arni14 == "No"),
  by = "parpop1"
) %>%
  mutate(diffps = ps.x - ps.y) %>%
  summarise(
    minps = min(diffps),
    maxps = max(diffps)
  )

if (any(abs(c(koll$minps, koll$maxps)) > 0.01)) stop("PS mathinng is > 0.01")

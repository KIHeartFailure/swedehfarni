

# Inclusion/exclusion criteria --------------------------------------------------------

flownpr <- c("Posts in NPR population", nrow(nprdata))

nprdata <- nprdata %>%
  filter(shf_indexdtm >= ymd("2017-01-01"))
flownpr <- rbind(flownpr, c("Indexdate >= 1 January 2017", nrow(nprdata)))

nprdata <- nprdata %>%
  filter(sos_outtime_death > 14)
flownpr <- rbind(flownpr, c(">14 days follow-up (to avoid immortal time bias)", nrow(nprdata)))

nprdata <- nprdata %>%
  filter(sos_lm_previousarni == "No")
flownpr <- rbind(flownpr, c("No previous ARNi use", nrow(nprdata)))

nprdata <- nprdata %>%
  filter(sos_lm_arni14 == "Yes")
flownpr <- rbind(flownpr, c("ARNi users (substudy 2)", nrow(nprdata)))

colnames(flownpr) <- c("Criteria", "N")

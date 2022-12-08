

# Inclusion/exclusion criteria --------------------------------------------------------

rsdata <- rsdata %>%
  filter(casecontrol == "Case SwedeHF")
flow <- c("Posts in SHFDB4", nrow(rsdata))

rsdata <- rsdata %>%
  filter(!(shf_source == "New SHF" & shf_type == "Follow-up"))
flow <- rbind(flow, c("Exclude follow-up visits for New SwedeHF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexdtm >= ymd("2017-01-01"))
flow <- rbind(flow, c("Indexdate >= 1 January 2017", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_ef))
flow <- rbind(flow, c("No missing EF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_ef %in% c("<30", "30-39")) %>%
  mutate(shf_ef = droplevels(shf_ef))
flow <- rbind(flow, c("EF < 40%", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(sos_outtime_death > 14)
flow <- rbind(flow, c(">14 days follow-up (to avoid immortal time bias*)", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(sos_lm_arni14 != "Previous") %>%
  mutate(sos_lm_arni14 = droplevels(sos_lm_arni14))
flow <- rbind(flow, c("No previous ARNi use", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post / patient (substudy 2)", nrow(rsdata)))

flow <- rbind(flow, c(" - Hospitalized (substudy 1)", nrow(rsdata %>%
  filter(shf_location %in% c("In-patient")))))

flow <- rbind(flow, c(" - Non-missing HF duration (substudy 3)", nrow(rsdata %>% filter(!is.na(shf_durationhf)))))

colnames(flow) <- c("Criteria", "N")

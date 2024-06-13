# Inclusion/exclusion criteria --------------------------------------------------------

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
flow <- rbind(flow, c(">14 days follow-up (to avoid immortal time bias)*", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(sos_lm_previousarni == "No")
flow <- rbind(flow, c("No previous ARNi use", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post / patient", nrow(rsdata)))

flow <- rbind(flow, c("Hospitalized (substudy 1, 2 groups)", nrow(rsdata %>%
  filter(shf_location %in% c("In-patient")))))

rsdata <- rsdata %>%
  mutate(pop13 = sos_outtime_death > 30 & shf_location %in% c("In-patient"))
flow <- rbind(flow, c("Hospitalized and >30 days follow-up (substudy 1, 3 groups)*", nrow(rsdata %>%
  filter(pop13))))

flow <- rbind(flow, c("ARNi users (substudy 2)", nrow(rsdata %>% filter(sos_lm_arni14 == "Yes"))))

flow <- rbind(flow, c("Non-missing HF duration (substudy 3)", nrow(rsdata %>% filter(!is.na(shf_durationhf)))))

# sensitivity analyses

rsdata <- rsdata %>%
  mutate(
    senscomp = case_when(
      is.na(shf_gfrckdepi) | is.na(shf_bpsys) | is.na(shf_potassium) ~ FALSE,
      shf_gfrckdepi >= 30 & shf_bpsys >= 100 & shf_potassium <= 5.5 ~ TRUE,
      TRUE ~ FALSE
    ),
    sens = case_when(
      (shf_gfrckdepi >= 30 | is.na(shf_gfrckdepi)) &
        (shf_bpsys >= 100 | is.na(shf_bpsys)) &
        (shf_potassium <= 5.5 | is.na(shf_potassium)) ~ TRUE,
      TRUE ~ FALSE
    )
  )

flow <- rbind(flow, c("Sensitivity eGFR >=30 & Sys bp >=100 & K <=5.5 (missing not included)", nrow(rsdata %>% filter(senscomp))))

flow <- rbind(flow, c("Sensitivity eGFR >=30 & Sys bp >=100 & K <=5.5 (missing assumed normal)", nrow(rsdata %>% filter(sens))))

colnames(flow) <- c("Criteria", "N")

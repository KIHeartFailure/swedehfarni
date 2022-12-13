
meta_variables <- read.xlsx("C:/Users/Lina/STATISTIK/Projects/20210525_shfdb4/dm/metadata/meta_variables.xlsx", sheet = "Sheet 1")
meta_variables <- meta_variables %>% select(-`description/comment`)
meta_variablesadd <- data.frame(
  "variable" = c(
    "sos_lm_previousrasi",
    "shf_rasi",
    "shf_rasidosetarget",
    "shf_mradosetarget",
    "shf_bbldosetarget",
    "shf_sglt2dosetarget"
  ),
  "label" = c(
    "Previous ACEi/ARB",
    "ACEi/ARB",
    "Target dose ACEi/ARB",
    "Target dose MRA",
    "Target dose Beta-blocker",
    "Target dose SGLT2i"
  ),
  "unit" = rep(NA, 6),
  "valid" = rep(NA, 6)
)

meta_variables <- rbind(meta_variables, meta_variablesadd)

load("C:/Users/Lina/STATISTIK/Projects/20210525_shfdb4/dm/data/v401/rsdata401.RData")

load("C:/Users/Lina/STATISTIK/Projects/20210525_shfdb4/dm/data/20220908/patregrsdata.RData")

load("C:/Users/Lina/STATISTIK/Projects/20210525_shfdb4/dm/data/20220908/lmswedehf.RData")

lmsel <- lmswedehf %>%
  mutate(atcneed = stringr::str_detect(ATC, "^(C09A|C09B|C09C|C09D)")) %>%
  filter(
    ANTAL >= 0,
    atcneed
  )

rm(lmswedehf)

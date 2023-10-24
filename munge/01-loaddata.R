
meta_variables <- read.xlsx("C:/Users/Lina/STATISTIK/Projects/20210525_shfdb4/dm/metadata/meta_variables.xlsx", sheet = "Sheet 1")
meta_variables <- meta_variables %>% select(-`description/comment`)
meta_variablesadd <- data.frame(
  "variable" = c(
    "sos_lm_previousrasi",
    "shf_rasi",
    "shf_rasidosetarget",
    "shf_mradosetarget",
    "shf_bbldosetarget",
    "shf_sglt2dosetarget", 
    "sos_lm_rasi",
    "sos_lm_mra",
    "sos_lm_bbl",
    "sos_lm_sglt2"
  ),
  "label" = c(
    "Previous ACEi/ARB",
    "ACEi/ARB",
    "Target dose ACEi/ARB",
    "Target dose MRA",
    "Target dose Beta-blocker",
    "Target dose SGLT2i", 
    "ACEi/ARB",
    "MRA",
    "Beta-blocker",
    "SGLT2i"
  ),
  "unit" = rep(NA, 10),
  "valid" = rep(NA, 10)
)

meta_variables <- rbind(meta_variables, meta_variablesadd)

load("C:/Users/Lina/STATISTIK/Projects/20210525_shfdb4/dm/data/v410/rsdatafull410.RData")

load("D:/STATISTIK/Projects/20210525_shfdb4/dm/data/20220908/patregrsdata.RData")

# LM loaded through data
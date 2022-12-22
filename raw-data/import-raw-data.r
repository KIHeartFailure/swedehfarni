ProjectTemplate::load.project(list(munging = FALSE, data_loading = FALSE))

# LM data -----------------------------------------------------------------

lmarni <- haven::read_sas("./raw-data/lmarni.sas7bdat")
save(
  file = "./data/lmarni.RData",
  list = c(
    "lmarni"
  )
)

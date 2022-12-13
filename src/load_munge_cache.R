# 1. Set options in config/global.dcf
# 2. Load packages listed in config/global.dcf
# 3. Import functions and code in lib directory
# 4. Load data in data directory
# 5. Run data manipulations in munge directory

ProjectTemplate::reload.project(
  reset = TRUE,
  data_loading = TRUE,
  munging = TRUE
)

ProjectTemplate::cache("flow")

ProjectTemplate::cache("metalm")
ProjectTemplate::cache("matchingn")
ProjectTemplate::cache("targetdose")

ProjectTemplate::cache("rsdata")
ProjectTemplate::cache("imprsdata")
ProjectTemplate::cache("imprsdatapop1")
ProjectTemplate::cache("imprsdatapop13")
ProjectTemplate::cache("imprsdatapop2")
ProjectTemplate::cache("imprsdatapop3")
ProjectTemplate::cache("rsdatarec")
ProjectTemplate::cache("rsdatarecpop13")

ProjectTemplate::cache("overtime")

ProjectTemplate::cache("tabvars")
ProjectTemplate::cache("tabvarspop1")
ProjectTemplate::cache("tabvarspop3")
ProjectTemplate::cache("modvars")
ProjectTemplate::cache("modvarspop1")
ProjectTemplate::cache("modvarspop3")

ProjectTemplate::cache("meta_variables")
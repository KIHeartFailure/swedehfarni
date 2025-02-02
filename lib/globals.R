# default is to use tidyverse functions
select <- dplyr::select 
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

# colours 
global_cols <- rev(c(
  #"#F0FAFF",
  #"#D6F0F7",
  "#9BD4E5",
  "#70C1DA",
  "#4FB3D1",
  "#2F99BA",
  "#0F83A3",
  "#006E8A",
  "#034F69",
  "#023647"
))

# used for calculation of ci 
global_z05 <- qnorm(1 - 0.025)

global_hficd <- " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R570"
global_atcarni <- c("^C09DX04")
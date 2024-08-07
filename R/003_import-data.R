rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here("R", "001_libraries.R"))
source(here::here("R", "002_file-paths.R"))

hcap2016 <- haven::read_sav(here::here(data_path, "hc16hp_r.sav"))

saveRDS(hcap2016, here::here(RDS_path, "003_hcap2016.RDS"))
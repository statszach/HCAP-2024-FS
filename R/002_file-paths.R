data_path <- fs::path(QSPtools::network_path(),
                      "STUDIES", "HCAP", "POSTED", "DATA", "SOURCE", "HCAP_2016")
user<- Sys.getenv("USER")
if (user=="rnj") {
  data_path <- gsub("Volumes/Research/BM", "Volumes/BM", data_path)
}
data_path


dir_path <- fs::path(QSPtools::network_path(), "STUDIES", "HCAP", "PROJECTS", "HCAP-FS")
if (user=="rnj") {
  dir_path <- gsub("Volumes/Research/BM", "Volumes/BM", dir_path)
}

fs::dir_create(here::here(dir_path, "R", "Images"))
images_path <- here::here(dir_path, "R","Images")

fs::dir_create(here::here(dir_path, "R", "Mplus"))
Mplus_path <- here::here(dir_path, "R","Mplus")

fs::dir_create(here::here(dir_path, "R", "RDS"))
RDS_path <- here::here(dir_path, "R","RDS")

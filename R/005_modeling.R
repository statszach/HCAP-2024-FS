rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "001_libraries.R"))
source(here::here("R", "002_file-paths.R"))

cleaned_vars <- readRDS(here::here(RDS_path, "004_cleaned_vars.RDS"))


mplusdata <- cleaned_vars %>% 
  dplyr::select(-HRS_ID, -R1CSID_SCORE) %>% 
  dplyr::mutate(vdmde1z = minmax(vdmde1),
                vdmde2z = minmax(vdmde2),
                vdmde4z = minmax(vdmde4),
                vdmde5z = minmax(vdmde5),
                vdmre1z = minmax(vdmre1),
                vdmre2z = minmax(vdmre2),
                vdexf1z = minmax(vdexf1),
                vdexf2z = minmax(vdexf2),
                vdexf7z = minmax(vdexf7),
                vdasp1z = minmax(vdasp1),
                vdasp2z = minmax(vdasp2),
                vdasp4z = minmax(vdasp4),
                vdasp5z = minmax(vdasp5),
                vdlfl1z = minmax(vdlfl1),
                vdori1z = minmax(vdori1),
                vdvis1z = minmax(vdvis1)) %>% 
  dplyr::select(-vdmde1,
                -vdmde2,
                -vdmde4,
                -vdmde5,
                -vdmre1,
                -vdmre2,
                -vdexf1,
                -vdexf2,
                -vdexf7,
                -vdasp1,
                -vdasp2,
                -vdasp4,
                -vdasp5,
                -vdlfl1,
                -vdori1,
                -vdvis1)

fs::dir_create(Mplus_path, "005_corrfac")
setwd(here::here(Mplus_path, "005_corrfac"))

corrfacmodel <- mplusObject(
  MODEL = "
  mde by vdmde1z vdmde2z vdmde3 vdmde4z vdmde5z vdmre1z vdmre2z  ;
  exf by vdexf1z vdexf2z vdexf7z  ;
  exf by vdasp1z vdasp2z vdasp3 vdasp4z vdasp5z  ;
  lfl by vdlfl1z vdlfl2 vdlfl3 vdlfl4 vdlfl5 vdlfl6  ;
  vdvis1z with mde exf lfl  ;
  vdori1z with mde exf lfl  ;
  vdvis1z with vdori1z  ;
  ceradwl by vdmde1z@1  ;
  ceradwl by vdmre1z@1  ;
  lm by vdmde2z@1  ;
  lm by vdmre2z@1  ;
  ceradwl with lm@0  ;
  ceradwl lm with mde@0 exf@0 lfl@0 vdvis1z@0 vdori1z@0  ;
	 
  
  ",
  usevariables = colnames(mplusdata),
  VARIABLE = "idvariable = mplusid;
  CATEGORICAL = vdmde3 vdasp3 vdlfl2 vdlfl3
  vdlfl4 vdlfl5 vdlfl6;",
  OUTPUT = "TECH4 STANDARDIZED SVALUES;",
  ANALYSIS = "ESTIMATOR = WLSMV;
  PARAMETERIZATION = THETA;",
  rdata = mplusdata)

corrfac <- mplusModeler(corrfacmodel, 
                        modelout = "corrfac.inp", run = TRUE)

fs::dir_create(Mplus_path, "005_gcp")
setwd(here::here(Mplus_path, "005_gcp"))

gcpmodel <- mplusObject(
  MODEL = "
  gmem by vdmde1z vdmde2z vdmde3 vdmde4z vdmde5z  ;
  gmem by vdmre1z vdmre2z  ;
  gexf by vdexf1z@1 vdexf2z vdexf7z  ;
  gexf by vdasp1z* vdasp2z* vdasp3* vdasp4z vdasp5z  ;
  lfl by vdlfl1z vdlfl2 vdlfl3 vdlfl4 vdlfl5 vdlfl6  ;
  g by vdori1z*  ;
  g by vdvis1z*  ;
  g by gmem@1 gexf* lfl*  ;
  ceradwl by vdmde1z@1  ;
  ceradwl by vdmre1z@1  ;
  lm by vdmde2z@1  ;
  lm by vdmre2z@1  ;
  ceradwl with lm@0 ;
  ceradwl with gmem@0 ;
  ceradwl with gexf@0 ;
  ceradwl with lfl@0 ;
  ceradwl with vdvis1z@0 ;
  ceradwl with g@0 ;
  lm with ceradwl@0 ;
  lm with gmem@0 ;
  lm with gexf@0 ;
  lm with lfl@0 ;
  lm with vdvis1z@0 ;
  lm with g@0 ;
  
  ",
  usevariables = colnames(mplusdata),
  VARIABLE = "idvariable = mplusid;
  CATEGORICAL = vdmde3 vdasp3 vdlfl2 vdlfl3
  vdlfl4 vdlfl5 vdlfl6;",
  OUTPUT = "TECH4 STANDARDIZED SVALUES;",
  ANALYSIS = "ESTIMATOR = WLSMV;
              PARAMETERIZATION = THETA;",
  rdata = mplusdata)

gcp <- mplusModeler(gcpmodel, 
                    modelout = "gcp.inp", run = TRUE)
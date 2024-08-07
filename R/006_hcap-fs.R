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

fs::dir_create(Mplus_path, "006_corrfac")
setwd(here::here(Mplus_path, "006_corrfac"))

corrfacmodel <- mplusObject(
  MODEL = "
  mde BY vdmde1z@1;
     mde BY vdmde2z@0.75846;
     mde BY vdmde3@4.55370;
     mde BY vdmde4z@1.08399;
     mde BY vdmde5z@0.83016;
     mde BY vdmre1z@0.38405;
     mde BY vdmre2z@0.57089;
     exf BY vdexf1z@1;
     exf BY vdexf2z@0.80759;
     exf BY vdexf7z@0.92395;
     exf BY vdasp1z@0.99055;
     exf BY vdasp2z@0.50695;
     exf BY vdasp3@4.11511;
     exf BY vdasp4z@0.49684;
     exf BY vdasp5z@0.62893;
     lfl BY vdlfl1z@1;
     lfl BY vdlfl2@7.13470;
     lfl BY vdlfl3@10.30474;
     lfl BY vdlfl4@6.00102;
     lfl BY vdlfl5@5.22172;
     lfl BY vdlfl6@6.04673;
     ceradwl BY vdmde1z@1;
     ceradwl BY vdmre1z@1;
     lm BY vdmde2z@1;
     lm BY vdmre2z@1;

     vdvis1z WITH vdori1z@0.00576;
     vdvis1z WITH mde@0.02097;
     vdvis1z WITH exf@0.02181;
     vdvis1z WITH lfl@0.01279;
     vdvis1z WITH ceradwl@0;
     vdvis1z WITH lm@0;
     vdori1z WITH mde@0.01116;
     vdori1z WITH exf@0.00796;
     vdori1z WITH lfl@0.00454;
     vdori1z WITH ceradwl@0;
     vdori1z WITH lm@0;
     ceradwl WITH lm@0;
     ceradwl WITH mde@0;
     ceradwl WITH exf@0;
     ceradwl WITH lfl@0;
     lm WITH mde@0;
     lm WITH exf@0;
     lm WITH lfl@0;
     exf WITH mde@0.02735;
     lfl WITH mde@0.01835;
     lfl WITH exf@0.01557;

     [ vdmde1z@0.51010 ];
     [ vdmde2z@0.28891 ];
     [ vdmde4z@0.52507 ];
     [ vdmde5z@0.41436 ];
     [ vdmre1z@0.92487 ];
     [ vdmre2z@0.67885 ];
     [ vdexf1z@0.72391 ];
     [ vdexf2z@0.42529 ];
     [ vdexf7z@0.60346 ];
     [ vdasp1z@0.44242 ];
     [ vdasp2z@0.39259 ];
     [ vdasp4z@0.29399 ];
     [ vdasp5z@0.39017 ];
     [ vdlfl1z@0.37292 ];
     [ vdori1z@0.94558 ];
     [ vdvis1z@0.74065 ];

     [ vdmde3$1@-1.82217 ];
     [ vdmde3$2@-0.62290 ];
     [ vdasp3$1@-2.70029 ];
     [ vdasp3$2@-2.26355 ];
     [ vdasp3$3@-1.77592 ];
     [ vdasp3$4@-1.11911 ];
     [ vdasp3$5@-0.91830 ];
     [ vdlfl2$1@-2.22925 ];
     [ vdlfl3$1@-3.27404 ];
     [ vdlfl4$1@-1.87597 ];
     [ vdlfl5$1@-2.23136 ];
     [ vdlfl6$1@-3.00848 ];
     [ vdlfl6$2@-2.14655 ];
     [ vdlfl6$3@-0.82510 ];

     vdmde1z@0.02410;
     vdmde2z@0.01688;
     vdmde4z@0.03749;
     vdmde5z@0.04839;
     vdmre1z@0.00460;
     vdmre2z@0.01569;
     vdexf1z@0.01981;
     vdexf2z@0.02114;
     vdexf7z@0.02002;
     vdasp1z@0.00614;
     vdasp2z@0.00444;
     vdasp4z@0.00678;
     vdasp5z@0.01053;
     vdlfl1z@0.00982;
     vdori1z@0.01153;
     vdvis1z@0.04576;
     mde@0.04290;
     exf@0.02921;
     lfl@0.01183;
     ceradwl@0.00363;
     lm@0.00572; 
  
  ",
  usevariables = colnames(mplusdata),
  VARIABLE = "idvariable = mplusid;
  CATEGORICAL = vdmde3 vdasp3 vdlfl2 vdlfl3
  vdlfl4 vdlfl5 vdlfl6;",
  OUTPUT = "TECH4 STANDARDIZED SVALUES;",
  ANALYSIS = "ESTIMATOR = WLSMV;
  PARAMETERIZATION = THETA;",
  rdata = mplusdata,
  SAVEDATA = "FILE IS fs.dat;
  SAVE is fscores;
  FORMAT IS free;")

corrfac <- mplusModeler(corrfacmodel, 
                        modelout = "corrfac.inp", run = TRUE)

fs::dir_create(Mplus_path, "006_gcp")
setwd(here::here(Mplus_path, "006_gcp"))

gcpmodel <- mplusObject(
  MODEL = "
  gmem BY vdmde1z@1;
     gmem BY vdmde2z@0.76063;
     gmem BY vdmde3@4.55019;
     gmem BY vdmde4z@1.08883;
     gmem BY vdmde5z@0.82926;
     gmem BY vdmre1z@0.38317;
     gmem BY vdmre2z@0.57107;
     gexf BY vdexf1z@1;
     gexf BY vdexf2z@0.80969;
     gexf BY vdexf7z@0.92489;
     gexf BY vdasp1z@0.99151;
     gexf BY vdasp2z@0.50778;
     gexf BY vdasp3@4.12898;
     gexf BY vdasp4z@0.49798;
     gexf BY vdasp5z@0.63039;
     lfl BY vdlfl1z@1;
     lfl BY vdlfl2@7.12966;
     lfl BY vdlfl3@10.26911;
     lfl BY vdlfl4@5.99354;
     lfl BY vdlfl5@5.21220;
     lfl BY vdlfl6@6.06369;
     g BY vdori1z@0.30137;
     g BY vdvis1z@0.72436;
     ceradwl BY vdmde1z@1;
     ceradwl BY vdmre1z@1;
     lm BY vdmde2z@1;
     lm BY vdmre2z@1;

     g BY gmem@1;
     g BY gexf@0.85663;
     g BY lfl@0.56229;

     ceradwl WITH lm@0;
     ceradwl WITH gmem@0;
     ceradwl WITH gexf@0;
     ceradwl WITH lfl@0;
     ceradwl WITH vdvis1z@0;
     ceradwl WITH g@0;
     lm WITH gmem@0;
     lm WITH gexf@0;
     lm WITH lfl@0;
     lm WITH vdvis1z@0;
     lm WITH g@0;

     [ vdmde1z@0.51010 ];
     [ vdmde2z@0.28891 ];
     [ vdmde4z@0.52507 ];
     [ vdmde5z@0.41436 ];
     [ vdmre1z@0.92487 ];
     [ vdmre2z@0.67885 ];
     [ vdexf1z@0.72391 ];
     [ vdexf2z@0.42529 ];
     [ vdexf7z@0.60346 ];
     [ vdasp1z@0.44242 ];
     [ vdasp2z@0.39259 ];
     [ vdasp4z@0.29399 ];
     [ vdasp5z@0.39017 ];
     [ vdlfl1z@0.37292 ];
     [ vdori1z@0.94558 ];
     [ vdvis1z@0.74065 ];

     [ vdmde3$1@-1.82087 ];
     [ vdmde3$2@-0.62245 ];
     [ vdasp3$1@-2.70187 ];
     [ vdasp3$2@-2.26488 ];
     [ vdasp3$3@-1.77696 ];
     [ vdasp3$4@-1.11977 ];
     [ vdasp3$5@-0.91884 ];
     [ vdlfl2$1@-2.22860 ];
     [ vdlfl3$1@-3.26762 ];
     [ vdlfl4$1@-1.87524 ];
     [ vdlfl5$1@-2.23033 ];
     [ vdlfl6$1@-3.01097 ];
     [ vdlfl6$2@-2.14833 ];
     [ vdlfl6$3@-0.82578 ];

     vdmde1z@0.02410;
     vdmde2z@0.01680;
     vdmde4z@0.03712;
     vdmde5z@0.04850;
     vdmre1z@0.00458;
     vdmre2z@0.01573;
     vdexf1z@0.01991;
     vdexf2z@0.02111;
     vdexf7z@0.02005;
     vdasp1z@0.00618;
     vdasp2z@0.00444;
     vdasp4z@0.00677;
     vdasp5z@0.01051;
     vdlfl1z@0.00983;
     vdori1z@0.00861;
     vdvis1z@0.02890;
     gmem@0.01069;
     gexf@0.00553;
     lfl@0.00167;
     g@0.03214;
     ceradwl@0.00370;
     lm@0.00569;
  
  ",
  usevariables = colnames(mplusdata),
  VARIABLE = "idvariable = mplusid;
  CATEGORICAL = vdmde3 vdasp3 vdlfl2 vdlfl3
  vdlfl4 vdlfl5 vdlfl6;",
  OUTPUT = "TECH4 STANDARDIZED SVALUES;",
  ANALYSIS = "ESTIMATOR = WLSMV;
              PARAMETERIZATION = THETA;",
  rdata = mplusdata,
  SAVEDATA = "FILE IS fs.dat;
  SAVE is fscores;
  FORMAT IS free;")

gcp <- mplusModeler(gcpmodel, 
                    modelout = "gcp.inp", run = TRUE)

setwd(here::here())

# Obtain FS

corrfac_fs <- corrfac$results$savedata %>% 
  dplyr::select(MPLUSID, VDORI1Z, VDVIS1Z, MDE, EXF, LFL)

gcp_fs <- gcp$results$savedata %>% 
  dplyr::select(MPLUSID, G)

hcap_fs <- corrfac_fs %>% 
  left_join(gcp_fs, by = "MPLUSID")

saveRDS(hcap_fs, here::here("006_fs.rds"))
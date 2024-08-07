rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "001_libraries.R"))
source(here::here("R", "002_file-paths.R"))

hc16hp_r <- readRDS(here::here(RDS_path, "003_hcap2016.RDS"))

hcap_tidy1 <- hc16hp_r %>% 
  tidyr::unite("HRS_ID", HHID:PN, remove = F)

# Orientation

vdori1 <- hcap_tidy1 %>% 
  dplyr::mutate(
    ori_1 = dplyr::case_when(R1ORIENT_T1 == 1 ~ 1,
                             R1ORIENT_T1 == 5 ~ 0,
                             TRUE ~ NA_real_),
    ori_2 = dplyr::case_when(R1ORIENT_T2 == 1 ~ 1,
                             R1ORIENT_T2 == 5 ~ 0,
                             TRUE ~ NA_real_),
    ori_3 = dplyr::case_when(R1ORIENT_T3 == 1 ~ 1,
                             R1ORIENT_T3 == 5 ~ 0,
                             TRUE ~ NA_real_),
    ori_4 = dplyr::case_when(R1ORIENT_T4 == 1 ~ 1,
                             R1ORIENT_T4 == 5 ~ 0,
                             TRUE ~ NA_real_),
    ori_5 = dplyr::case_when(R1ORIENT_T5 == 1 ~ 1,
                             R1ORIENT_T5 == 5 ~ 0,
                             TRUE ~ NA_real_),
    ori_6 = dplyr::case_when(RIORIENT_P1 == 1 ~ 1,
                             RIORIENT_P1 == 5 ~ 0,
                             TRUE ~ NA_real_),
    ori_7 = dplyr::case_when(RIORIENT_P2 == 1 ~ 1,
                             RIORIENT_P2 == 5 ~ 0,
                             TRUE ~ NA_real_),
    ori_8 = dplyr::case_when(RIORIENT_P3 == 1 ~ 1,
                             RIORIENT_P3 == 5 ~ 0,
                             TRUE ~ NA_real_),
    ori_9 = dplyr::case_when(RIORIENT_P4 == 1 ~ 1,
                             RIORIENT_P4 == 5 ~ 0,
                             TRUE ~ NA_real_),
    ori_10 = dplyr::case_when(RIORIENT_P5 == 1 ~ 1,
                              RIORIENT_P5 == 5 ~ 0,
                              TRUE ~ NA_real_)) %>%
  dplyr::rowwise() %>% 
  dplyr::mutate(vdori1 = sum(across(ori_1:ori_10))) %>% 
  ungroup() %>% 
  dplyr::select(HRS_ID, vdori1)

psych::describe(vdori1$vdori1) # Not exactly right

# Memory

vdmde1 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1WORD_DSCORE) %>% 
  dplyr::rename(vdmde1 = R1WORD_DSCORE) %>% 
  dplyr::select(HRS_ID, vdmde1)

psych::describe(vdmde1$vdmde1) # all right but kurtosis

vdmde2 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1LMB_DELSCORE, R1LMB_DELTEST) %>% 
  dplyr::mutate(vdmde2 = dplyr::case_when(!is.na(R1LMB_DELSCORE) ~ R1LMB_DELSCORE,
                                          R1LMB_DELTEST == 9 ~ NA_real_,
                                          TRUE ~ NA_real_)) %>% 
  dplyr::select(HRS_ID, vdmde2)

psych::describe(vdmde2$vdmde2) # pretty close but not exact

vdmde3 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1DLRC3) %>% 
  dplyr::mutate(vdmde3 = dplyr::case_when(R1DLRC3 == 0 ~ 0,
                                          R1DLRC3 == 1 ~ 0,
                                          R1DLRC3 == 2 ~ 1,
                                          R1DLRC3 == 3 ~ 2,
                                          TRUE ~ NA_real_)) %>% 
  dplyr::select(HRS_ID, vdmde3)

psych::describe(vdmde3$vdmde3) # not quite right

vdmde4 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1CPDEL_SCORE) %>% 
  dplyr::rename(vdmde4 = R1CPDEL_SCORE) %>% 
  dplyr::select(HRS_ID, vdmde4)

psych::describe(vdmde4$vdmde4) # mostly right except kurtosis

vdmde5 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1BM_DELSCORE) %>% 
  dplyr::rename(vdmde5 = R1BM_DELSCORE) %>% 
  dplyr::select(HRS_ID, vdmde5)

psych::describe(vdmde5$vdmde5) # mostly right except kurtosis

vdmre1 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1WREC_FOIL, R1WREC_ORG) %>% 
  dplyr::mutate(vdmre1 = R1WREC_FOIL + R1WREC_ORG) %>% 
  dplyr::select(HRS_ID, vdmre1)

psych::describe(vdmre1$vdmre1) # mostly right

vdmre2 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1LMB_RECOSCORE) %>% 
  dplyr::rename(vdmre2 = R1LMB_RECOSCORE) %>% 
  dplyr::select(HRS_ID, vdmre2)

psych::describe(vdmre2$vdmre2) # mostly right

# Visuospatial

vdvis1 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1CP_SCORE) %>% 
  dplyr::rename(vdvis1 = R1CP_SCORE) %>% 
  dplyr::select(HRS_ID, vdvis1)

psych::describe(vdvis1$vdvis1) # mostly right except kurtosis

# Executive Functioning

vdexf1 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1RV_SCORE) %>% 
  dplyr::rename(vdexf1 = R1RV_SCORE) %>% 
  dplyr::select(HRS_ID, vdexf1)

psych::describe(vdexf1$vdexf1) # mostly right except kurtosis

vdexf2 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1TMB_SCORE) %>% 
  dplyr::mutate(R1TMB_SCORE = dplyr::if_else(R1TMB_SCORE > 300, NA_real_, R1TMB_SCORE),
                vdexf2 = 1 - (log(R1TMB_SCORE) / log(300))) %>% 
  dplyr::select(HRS_ID, vdexf2)

psych::describe(vdexf2$vdexf2) # mostly right

vdexf7 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1NS_SCORE) %>% 
  dplyr::rename(vdexf7 = R1NS_SCORE)

psych::describe(vdexf7$vdexf7) # close but off enough i'm concerned

vdasp1 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1DIG_SCORE) %>% 
  dplyr::rename(vdasp1 = R1DIG_SCORE)

psych::describe(vdasp1$vdasp1) # close but off enough i'm concerned

vdasp2 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1TMA_SCORE) %>% 
  dplyr::mutate(R1TMA_SCORE = dplyr::if_else(R1TMA_SCORE > 300, NA_real_, R1TMA_SCORE),
                vdasp2 = 1 - (log(R1TMA_SCORE) / log(300))) %>% 
  dplyr::select(HRS_ID, vdasp2)

psych::describe(vdasp2$vdasp2) # mostly right

vdasp3 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1BACKWARDS1:R1BACKWARDS5) %>% 
  dplyr::mutate(bckwd1 = dplyr::case_when(R1BACKWARDS1 == 1 ~ 1,
                                          R1BACKWARDS1 == 5 ~ 0,
                                          TRUE ~ NA_real_),
                bckwd2 = dplyr::case_when(R1BACKWARDS2 == 1 ~ 1,
                                          R1BACKWARDS2 == 5 ~ 0,
                                          TRUE ~ NA_real_),
                bckwd3 = dplyr::case_when(R1BACKWARDS3 == 1 ~ 1,
                                          R1BACKWARDS3 == 5 ~ 0,
                                          TRUE ~ NA_real_),
                bckwd4 = dplyr::case_when(R1BACKWARDS4 == 1 ~ 1,
                                          R1BACKWARDS4 == 5 ~ 0,
                                          TRUE ~ NA_real_),
                bckwd5 = dplyr::case_when(R1BACKWARDS5 == 1 ~ 1,
                                          R1BACKWARDS5 == 5 ~ 0,
                                          TRUE ~ NA_real_)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(vdasp3 = sum(across(bckwd1:bckwd5))) %>% 
  ungroup() %>% 
  dplyr::select(HRS_ID, vdasp3)

psych::describe(vdasp3$vdasp3) # mostly right
table(vdasp3$vdasp3)

vdasp4 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1BC_SCORE) %>% 
  dplyr::rename(vdasp4 = R1BC_SCORE)

psych::describe(vdasp4$vdasp4) # mostly right, but higher range

vdasp5 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1LC_SCORE) %>% 
  dplyr::rename(vdasp5 = R1LC_SCORE)

psych::describe(vdasp5$vdasp5) # mostly right

# Language

vdlfl1 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1VERBAL_TOTAL) %>% 
  dplyr::rename(vdlfl1 = R1VERBAL_TOTAL)

psych::describe(vdlfl1$vdlfl1) # mostly right but higher range

vdlfl2 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1SCIS, R1CACTUS) %>% 
  dplyr::mutate(tics1 = dplyr::case_when(R1SCIS == 1 ~ 1,
                                         R1SCIS == 5 ~ 0,
                                         TRUE ~ NA_real_),
                tics2 = dplyr::case_when(R1CACTUS == 1 ~ 1,
                                         R1CACTUS == 5 ~ 0,
                                         TRUE ~ NA_real_),
                vdlfl2_temp = tics1 + tics2,
                vdlfl2 = dplyr::case_when(vdlfl2_temp == 0 ~ 0,
                                          vdlfl2_temp == 1 ~ 0,
                                          vdlfl2_temp == 2 ~ 1)) %>% 
  dplyr::select(HRS_ID, vdlfl2)

psych::describe(vdlfl2$vdlfl2)
table(vdlfl2$vdlfl2)

vdlfl3 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1OBJECT) %>% 
  dplyr::mutate(vdlfl3 = dplyr::case_when(R1OBJECT == 0 ~ 0,
                                          R1OBJECT == 1 ~ 0,
                                          R1OBJECT == 2 ~ 1)) %>% 
  dplyr::select(HRS_ID, vdlfl3)

psych::describe(vdlfl3$vdlfl3)
table(vdlfl3$vdlfl3) # Intentionally changed this because of the cell sizes

vdlfl4 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1SENTEN) %>% 
  dplyr::mutate(vdlfl4 = dplyr::case_when(R1SENTEN == 1 ~ 1,
                                          R1SENTEN == 2 ~ 1,
                                          R1SENTEN == 5 ~ 0,
                                          TRUE ~ NA_real_)) %>% 
  dplyr::select(HRS_ID, vdlfl4)

psych::describe(vdlfl4$vdlfl4)
table(vdlfl4$vdlfl4) # pretty close

vdlfl5 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1COMBFOL) %>% 
  dplyr::mutate(vdlfl5 = dplyr::case_when(R1COMBFOL == 1 ~ 1,
                                          R1COMBFOL == 2 ~ 1,
                                          R1COMBFOL == 5 ~ 0,
                                          TRUE ~ NA_real_)) %>% 
  dplyr::select(HRS_ID, vdlfl5)

psych::describe(vdlfl5$vdlfl5)
table(vdlfl5$vdlfl5) # pretty close

vdlfl6 <- hcap_tidy1 %>% 
  dplyr::select(HRS_ID, R1CSID_SCORE) %>% 
  dplyr::mutate(vdlfl6 = dplyr::case_when(R1CSID_SCORE == 0 ~ 0,
                                          R1CSID_SCORE == 1 ~ 0,
                                          R1CSID_SCORE == 2 ~ 1,
                                          R1CSID_SCORE == 3 ~ 2,
                                          R1CSID_SCORE == 4 ~ 3,
                                          TRUE ~ NA_real_))
psych::describe(vdlfl6$vdlfl6)
table(vdlfl6$vdlfl6) # pretty close

cleaned_vars <- vdori1 %>% 
  left_join(vdmde1, by = "HRS_ID") %>% 
  left_join(vdmde2, by = "HRS_ID") %>% 
  left_join(vdmde3, by = "HRS_ID") %>% 
  left_join(vdmde4, by = "HRS_ID") %>% 
  left_join(vdmde5, by = "HRS_ID") %>% 
  left_join(vdmre1, by = "HRS_ID") %>% 
  left_join(vdmre2, by = "HRS_ID") %>% 
  left_join(vdexf1, by = "HRS_ID") %>% 
  left_join(vdexf2, by = "HRS_ID") %>% 
  left_join(vdexf7, by = "HRS_ID") %>%
  left_join(vdasp1, by = "HRS_ID") %>% 
  left_join(vdasp2, by = "HRS_ID") %>% 
  left_join(vdasp3, by = "HRS_ID") %>% 
  left_join(vdasp4, by = "HRS_ID") %>% 
  left_join(vdasp5, by = "HRS_ID") %>% 
  left_join(vdlfl1, by = "HRS_ID") %>% 
  left_join(vdlfl2, by = "HRS_ID") %>% 
  left_join(vdlfl3, by = "HRS_ID") %>% 
  left_join(vdlfl4, by = "HRS_ID") %>% 
  left_join(vdlfl5, by = "HRS_ID") %>% 
  left_join(vdlfl6, by = "HRS_ID") %>% 
  left_join(vdvis1, by = "HRS_ID") %>% 
  dplyr::mutate(mplusid = row_number()) %>% 
  labelled::remove_attributes("label") %>% 
  labelled::remove_attributes("format.spss")

saveRDS(cleaned_vars, here::here(RDS_path, "004_cleaned_vars.RDS"))
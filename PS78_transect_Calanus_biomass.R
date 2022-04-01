#Biomass Calanus

library(tidyverse)
#run PS78_ecotaxa_database.R first, then PS78_transect_PL_ESD_relationship.R
#Calanus individuals where the developmental stages could not be defined are excluded.
#Males are also excluded

lW <- PS78_estL

#Extract Calanus species
biomass_Calanus <- lW %>% filter(str_detect(object_annotation_hierarchy, ">Calanus"))
biomass_Cfinglac <- biomass_Calanus %>% filter(!str_detect(object_annotation_hierarchy, "Calanus hyperboreus"))
biomass_Chyp <- biomass_Calanus %>% filter(str_detect(object_annotation_hierarchy, "Calanus hyperboreus"))
biomass_Chyp$DW_CN_mg_m3 <- NA
biomass_Cfinglac$DW_CN_mg_m3 <- NA
#Conversion factors: species-specific
#Calanus: Ikeda & Skjoldal 1989
biomass_Cfinglac$DW_CF_mg_m3 <- 1*0.245*biomass_Cfinglac$biovol_maj_mm3_m3
biomass_Chyp$DW_CF_mg_m3 <- 1*0.32*biomass_Chyp$biovol_maj_mm3_m3

#Conversion factors: Arctic Copepods
#Kosobokova and Hirche 2002
biomass_Cfinglac$DW_CF16_mg_m3 <- 1*0.162*biomass_Cfinglac$biovol_maj_mm3_m3
biomass_Chyp$DW_CF16_mg_m3 <- 1*0.162*biomass_Chyp$biovol_maj_mm3_m3

#LW relationship from our study (Power curve) and Hirche & Mumm (1992)
#ESD
biomass_Cfinglac$DW_ESD_mg_ind <- 0.029014*biomass_Cfinglac$esd_maj_min_mm^2.6932426
biomass_Cfinglac$DW_ESD_mg_m3 <- biomass_Cfinglac$acq_sub_part*biomass_Cfinglac$DW_ESD_mg_ind/biomass_Cfinglac$sample_tot_vol
biomass_Chyp$DW_ESD_mg_ind <- 0.039802*biomass_Chyp$esd_maj_min_mm^2.285416
biomass_Chyp$DW_ESD_mg_m3 <- biomass_Chyp$acq_sub_part*biomass_Chyp$DW_ESD_mg_ind/biomass_Chyp$sample_tot_vol

#Calanus: length-weight relationship from our study (Power curve) and Hirche & Mumm (1992)
#Estimated Prosome Lenght (PL)
biomass_Cfinglac$DW_PL_mg_ind <- 0.029014*biomass_Cfinglac$estimated_L^2.6932426
biomass_Cfinglac$DW_PL_mg_m3 <- biomass_Cfinglac$acq_sub_part*biomass_Cfinglac$DW_PL_mg_ind/biomass_Cfinglac$sample_tot_vol
biomass_Chyp$DW_PL_mg_ind <- 0.039802*biomass_Chyp$estimated_L^2.285416
biomass_Chyp$DW_PL_mg_m3 <- biomass_Chyp$acq_sub_part*biomass_Chyp$DW_PL_mg_ind/biomass_Chyp$sample_tot_vol

#biomass CN analyser
#biomass from direct mean measurements druing PS78 (C5 + F, C1-C4 Literaturdaten)
#C. finmarchicus
cfin <- biomass_Cfinglac %>% filter(!str_detect(species, "glacialis"))
cfinF <- cfin %>% filter(str_detect(species, "finmarchicus f"))
cfinF$DW_CN_mg_m3 <- 0.416 * cfinF$abundance_m3
cfinC5 <- cfin %>% filter(str_detect(species, "finmarchicus C5"))
cfinC5$DW_CN_mg_m3 <- 0.499 * cfinC5$abundance_m3
cfinC4 <- cfin %>% filter(str_detect(species, "finmarchicus C4"))
cfinC4$DW_CN_mg_m3 <- 0.128 * cfinC4$abundance_m3
cfinC3 <- cfin %>% filter(str_detect(species, "finmarchicus C3"))
cfinC3$DW_CN_mg_m3 <- 0.046 * cfinC3$abundance_m3
cfinC2 <- cfin %>% filter(str_detect(species, "finmarchicus C2"))
cfinC2$DW_CN_mg_m3 <- 0.011 * cfinC2$abundance_m3
cfinC1 <- cfin %>% filter(str_detect(species, "finmarchicus C1"))
cfinC1$DW_CN_mg_m3 <- 0.005 * cfinC1$abundance_m3

cfin_all <- rbind(cfinF, cfinC5, cfinC4, cfinC3, cfinC2, cfinC1)

#C. glacialis
cglac <- biomass_Cfinglac %>% filter(!str_detect(species, "finmarchicus"))
cglacF <- cglac %>% filter(str_detect(species, "glacialis f"))
cglacF$DW_CN_mg_m3 <- 0.987 * cglacF$abundance_m3
cglacC5 <- cglac %>% filter(str_detect(species, "glacialis C5"))
cglacC5$DW_CN_mg_m3 <- 0.640 * cglacC5$abundance_m3
cglacC4 <- cglac %>% filter(str_detect(species, "glacialis C4"))
cglacC4$DW_CN_mg_m3 <- 0.235 * cglacC4$abundance_m3
cglacC3 <- cglac %>% filter(str_detect(species, "glacialis C3"))
cglacC3$DW_CN_mg_m3 <- 0.043 * cglacC3$abundance_m3
cglacC2 <- cglac %>% filter(str_detect(species, "glacialis C2"))
cglacC2$DW_CN_mg_m3 <- 0.05 * cglacC2$abundance_m3
cglacC1 <- cglac %>% filter(str_detect(species, "glacialis C1"))
cglacC1$DW_CN_mg_m3 <- 0.014 * cglacC1$abundance_m3

cglac_all <- rbind(cglacF, cglacC5, cglacC4, cglacC3, cglacC2, cglacC1)

#C. hyperboreus
chypF <- biomass_Chyp %>% filter(str_detect(object_annotation_category, "female"))
chypF$DW_CN_mg_m3 <- 2.929 * chypF$abundance_m3
chypC5 <- biomass_Chyp %>% filter(str_detect(object_annotation_category, "CVstage"))
chypC5$DW_CN_mg_m3 <- 1.393 * chypC5$abundance_m3
chypC4 <- biomass_Chyp %>% filter(str_detect(object_annotation_category, "CIVstage"))
chypC4$DW_CN_mg_m3 <- 0.347 * chypC4$abundance_m3
chypC3 <- biomass_Chyp %>% filter(str_detect(object_annotation_category, "CIIIstage"))
chypC3$DW_CN_mg_m3 <- 0.135 * chypC3$abundance_m3
chypC2 <- biomass_Chyp %>% filter(str_detect(object_annotation_category, "CIIstage"))
chypC2$DW_CN_mg_m3 <- 0.04 * chypC2$abundance_m3
chypC1 <- biomass_Chyp %>% filter(str_detect(object_annotation_category, "CIstage"))
chypC1$DW_CN_mg_m3 <- 0.01 * chypC1$abundance_m3

chyp_all <- rbind(chypF, chypC5, chypC4, chypC3, chypC2, chypC1)

biomass_Calanus_final <- rbind(cfin_all, cglac_all, chyp_all)

write.table(biomass_Calanus_final, "/Users/acornils/Documents/Veröffentlichungen/2022_L&OMethods_PS78/ps78_transect_biomass_Calanus.txt", sep="\t",row.names=F)

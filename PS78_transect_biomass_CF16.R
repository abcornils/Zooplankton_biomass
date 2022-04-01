#Conversion factors

library(tidyverse)
#run PS78_ecotaxa_database.R first, then PS78_transect_PL_ESD_relationship.R

lW <- PS78_estL

#calculate dry weight/ind from conversion factors
#1 = Conversion biovolume - wet weight: Assumption that gravity of water 1 g m-3
#Cnidaria: Kjörboe 2013
biomassCF_Cnidaria <- lW %>% filter(str_detect(object_annotation_hierarchy, "Cnidaria"))
biomassCF_Cnidaria <- biomassCF_Cnidaria %>% filter(!str_detect(object_annotation_hierarchy, "Aglantha"))
biomassCF_Cnidaria$DW_CF_mg_m3 <- 1*0.041*biomassCF_Cnidaria$biovol_maj_mm3_m3
#Aglantha: Ikeda & Skjoldal 1989
biomassCF_Aglantha <- lW %>% filter(str_detect(object_annotation_hierarchy, "Aglantha"))
biomassCF_Aglantha$DW_CF_mg_m3 <- 1*0.053*biomassCF_Aglantha$biovol_maj_mm3_m3

#Amphipoda: from Kjörboe 2013
biomassCF_Amphipoda <- lW %>% filter(str_detect(object_annotation_hierarchy, "Amphipoda"))
biomassCF_Amphipoda <- biomassCF_Amphipoda %>% filter(!str_detect(object_annotation_hierarchy, "Themisto"))
biomassCF_Amphipoda$DW_CF_mg_m3 <- 1* 0.239 * biomassCF_Amphipoda$biovol_maj_mm3_m3
#Themisto: from Ikeda & Skjoldal 1989 
biomassCF_Themisto <- lW %>% filter(str_detect(object_annotation_hierarchy, "Themisto"))
biomassCF_Themisto$DW_CF_mg_m3 <- 1*0.176*biomassCF_Themisto$biovol_maj_mm3_m3

#Clione: Ikeda & Skjoldal 1989
biomassCF_Clione <- lW %>% filter(str_detect(object_annotation_hierarchy, "Clione"))
biomassCF_Clione$DW_CF_mg_m3 <- 1*0.058*biomassCF_Clione$biovol_maj_mm3_m3

#other Gastropoda: Ikeda & Skjoldal 1989
biomassCF_Gastropoda <- lW %>% filter(str_detect(object_annotation_category, "Gastropoda"))
biomassCF_Gastropoda <- biomassCF_Gastropoda %>% filter(!str_detect(object_annotation_category, "Clione"))
biomassCF_Gastropoda$DW_CF_mg_m3 <- 1*0.222*biomassCF_Gastropoda$biovol_maj_mm3_m3

#Polychaeta: Kjörboe 2013 for Tomopteris
biomassCF_Polychaeta <- lW %>% filter(str_detect(object_annotation_hierarchy, "Polychaeta"))
biomassCF_Polychaeta$DW_CF_mg_m3 <- 1*0.139*biomassCF_Polychaeta$biovol_maj_mm3_m3

#Ostracoda: Kjörboe 2013
biomassCF_Ostracoda <- lW %>% filter(str_detect(object_annotation_hierarchy, "Ostracoda"))
biomassCF_Ostracoda$DW_CF_mg_m3 <- 1*0.159*biomassCF_Ostracoda$biovol_maj_mm3_m3

#Hymenodora: Kjörboe 2013 for Crustacea
biomassCF_Hymenodora <- lW %>% filter(str_detect(object_annotation_hierarchy, "Hymenodora"))
biomassCF_Hymenodora$DW_CF_mg_m3 <- 1*0.183*biomassCF_Hymenodora$biovol_maj_mm3_m3

#Appendicularia: Kjörboe 2013 for Tunicata
biomassCF_Appendicularia <- lW %>% filter(str_detect(object_annotation_hierarchy, "Appendicularia"))
biomassCF_Appendicularia$DW_CF_mg_m3 <- 1*0.054*biomassCF_Appendicularia$biovol_maj_mm3_m3

#Chaetognatha: Kjörboe 2013 
biomassCF_Chaetognatha <- lW %>% filter(str_detect(object_annotation_hierarchy, "Chaetognatha"))
biomassCF_Chaetognatha$DW_CF_mg_m3 <- 1*0.077*biomassCF_Chaetognatha$biovol_maj_mm3_m3

#Euphausiacea: Ikeda & Skjoldal 1989
biomassCF_Euphausiacea <- lW %>% filter(str_detect(object_annotation_hierarchy, "Euphausiacea"))
biomassCF_Euphausiacea$DW_CF_mg_m3 <- 1*0.259*biomassCF_Euphausiacea$biovol_maj_mm3_m3

#Isopoda: Kjörboe 2013 for Crustacea
biomassCF_Isopoda <- lW %>% filter(str_detect(object_annotation_hierarchy, "Isopoda"))
biomassCF_Isopoda$DW_CF_mg_m3 <- 1*0.183*biomassCF_Isopoda$biovol_maj_mm3_m3

#Mysida: Kjörboe 2013 for Crustacea
biomassCF_Mysida <- lW %>% filter(str_detect(object_annotation_hierarchy, "Mysida"))
biomassCF_Mysida$DW_CF_mg_m3 <- 1*0.183*biomassCF_Mysida$biovol_maj_mm3_m3

#Echinodermata, unknown, multiple, bivalvia - converted fromm biovolume Postel et al. 2000
biomassCF_Echinodermata <- lW %>% filter(str_detect(object_annotation_hierarchy, "Echinodermata"))
#biomassCF_Unknown <- lW %>% filter(str_detect(object_annotation_hierarchy, "Unknown")) <= ist in Bivalvia mit drin!
biomassCF_Multiple <- lW %>% filter(str_detect(object_annotation_hierarchy, "multiple"))
biomassCF_Bivalvia <- lW %>% filter(str_detect(object_annotation_hierarchy, "Bivalvia"))
biomassCF_Ello <- lW %>% filter(str_detect(object_annotation_hierarchy, "Ellobiopsidae"))

biomassCF_others <- rbind(biomassCF_Echinodermata, biomassCF_Multiple, biomassCF_Bivalvia, biomassCF_Ello)
biomassCF_others$DW_CF_mg_m3 <- 1*0.2*biomassCF_others$biovol_maj_mm3_m3

#combine all calculations of non-copepods
biomassCF_non_Copepoda <- rbind(biomassCF_Cnidaria, biomassCF_Aglantha, biomassCF_Amphipoda, biomassCF_Themisto, biomassCF_Appendicularia, 
                                biomassCF_Chaetognatha, biomassCF_Clione, biomassCF_Euphausiacea, 
                                biomassCF_Gastropoda, biomassCF_Hymenodora, biomassCF_Isopoda, 
                                biomassCF_Mysida, biomassCF_Ostracoda, biomassCF_others, 
                                biomassCF_Polychaeta)

#Copepoda conversion factor 0.16
biomassCF_Copepoda <- lW %>% filter(str_detect(object_annotation_hierarchy, "Copepoda")) 
biomassCF_Copepoda$DW_CF_mg_m3 <- 1*0.162*biomassCF_Copepoda$biovol_maj_mm3_m3
biomassCF_nauplii <- lW %>% filter(str_detect(object_annotation_category, "nauplii<Crustacea"))
biomassCF_nauplii$DW_CF_mg_m3 <- 1*0.162*biomassCF_nauplii$biovol_maj_mm3_m3

lW_y <- rbind(biomassCF_non_Copepoda, biomassCF_Copepoda, biomassCF_nauplii)

DW_CF16_mgm3_aggr <- aggregate(DW_CF_mg_m3 ~ sample + object_depth_min +  
                               + object_depth_max + object_annotation_category,
                             data=lW_y, FUN=sum)

DW_CF16_mgm3_select <- select(DW_CF16_mgm3_aggr, sample, object_annotation_category, DW_CF_mg_m3)
DW_CF16_mgm3 <- spread(DW_CF16_mgm3_select, object_annotation_category, DW_CF_mg_m3)
#DW_CF16_mgm3 <- DW_CF16_mgm3 %>% remove_rownames %>% column_to_rownames(var="sample")
#DW_CF16_mgm3$Total <- rowSums(DW_CF16_mgm3[], na.rm=TRUE)


#write.table(DW_CF16_mgm3, "/Users/acornils/Documents/Veröffentlichungen/2022_L&OMethods_PS78/PS78_PANGAEA/x_ps78_transect_biomass_CF16.txt", sep="\t",row.names=F)

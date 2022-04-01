#Biomass from LW relationships

library(tidyverse)
#run PS78_ecotaxa_database.R first, then PS78_transect_PL_ESD_relationship.R

lW <- PS78_estL

#Bodylength - measured on images
#calculate dry weight/ind from length relationships
#Cnidaria: Matthews & Hestad 1977
biomassLW_Cnidaria <- lW %>% filter(str_detect(object_annotation_hierarchy, "Cnidaria"))
biomassLW_Cnidaria$DW_mg_ind <- 0.00194*biomassLW_Cnidaria$estimated_L^3.05
biomassLW_Cnidaria$DW_mg_m3 <- biomassLW_Cnidaria$acq_sub_part*biomassLW_Cnidaria$DW_mg_ind/biomassLW_Cnidaria$sample_tot_vol

#Amphipoda: from Auel & Werner 2003
biomassLW_Amphipoda <- lW %>% filter(str_detect(object_annotation_hierarchy, "Amphipoda"))
biomassLW_Amphipoda$DW_mg_ind <- 0.006*biomassLW_Amphipoda$estimated_L^2.8220912
biomassLW_Amphipoda$DW_mg_m3 <- biomassLW_Amphipoda$acq_sub_part*biomassLW_Amphipoda$DW_mg_ind/biomassLW_Amphipoda$sample_tot_vol

#Gastropoda: LW relationship for Clione (from Mizdalski 1988)
#LL <- c(2.5,3.5,4.0,4.0,4.5,5.0,6.0,6.0,6.5,6.5,6.5,7.0,7.0,9.5,10.0,15.0,16.0)
#WW <- c(0.4313,0.3342,0.5793,0.5022,2.596,1.4759,1.7420,1.7720,3.3926,2.3303,1.0710,2.4123,1.4377,5.1939,5.7091,17.5083,18.3492)
#a <- data.frame(LL, WW)
#library(drc)
#library(nlme)
#library(aomisc)
#D(expression(a * X^b), "X")
#model <- drm(WW ~ LL, fct=DRC.powerCurve(), data=a)
#plot(model, log="")
#summary(model)
#a=0.02220005, b=2.4379117
biomassLW_Clione <- lW %>% filter(str_detect(object_annotation_hierarchy, "Clione"))
biomassLW_Clione$DW_mg_ind <- 0.02220005*biomassLW_Clione$estimated_L^2.4379117
biomassLW_Clione$DW_mg_m3 <- biomassLW_Clione$acq_sub_part*biomassLW_Clione$DW_mg_ind/biomassLW_Clione$sample_tot_vol

#other Gastropoda derived from Mizdalski 1988
#LL <- c(0.9,0.9,0.9,1.0,1.0,1.1,1.1,1.2,1.2,1.3,1.3,1.3,1.4,1.4,1.5)
#WW <- c(0.1506,0.1581,0.1738,0.1210,0.1332,0.1767,0.1953,0.2331,0.2363,0.2394,0.2600,0.2436,0.2208,0.3125,0.2718)
#a <- data.frame(LL, WW)
#a=0.1678611, b=1.3612853
biomassLW_Gastropoda <- lW %>% filter(str_detect(object_annotation_category, "Gastropoda"))
biomassLW_Gastropoda$DW_mg_ind <- 0.1678611*biomassLW_Gastropoda$estimated_L^1.3612853
biomassLW_Gastropoda$DW_mg_m3 <- biomassLW_Gastropoda$acq_sub_part*biomassLW_Gastropoda$DW_mg_ind/biomassLW_Gastropoda$sample_tot_vol

#Polychaeta: Matthews and Hestad (1977)
biomassLW_Polychaeta <- lW %>% filter(str_detect(object_annotation_hierarchy, "Polychaeta"))
biomassLW_Polychaeta$DW_mg_ind <- 0.005*biomassLW_Polychaeta$estimated_L^2.25
biomassLW_Polychaeta$DW_mg_m3 <- biomassLW_Polychaeta$acq_sub_part*biomassLW_Polychaeta$DW_mg_ind/biomassLW_Polychaeta$sample_tot_vol

#Ostracoda: Richter (1994)
biomassLW_Ostracoda <- lW %>% filter(str_detect(object_annotation_hierarchy, "Ostracoda"))
biomassLW_Ostracoda$DW_mg_ind <- 0.033*biomassLW_Ostracoda$estimated_L^2.37
biomassLW_Ostracoda$DW_mg_m3 <- biomassLW_Ostracoda$acq_sub_part*biomassLW_Ostracoda$DW_mg_ind/biomassLW_Ostracoda$sample_tot_vol

#Hymenodora: DW Wert von Richter (1994), no length-weight relationship available
biomassLW_Hymenodora <- lW %>% filter(str_detect(object_annotation_hierarchy, "Hymenodora"))
biomassLW_Hymenodora$DW_mg_ind <- 41
biomassLW_Hymenodora$DW_mg_m3 <- biomassLW_Hymenodora$acq_sub_part*biomassLW_Hymenodora$DW_mg_ind/biomassLW_Hymenodora$sample_tot_vol

#Appendicularia: for Fritillaria from Hopcroft et al. (2010) (originally Fenaux 1976)
biomassLW_Appendicularia <- lW %>% filter(str_detect(object_annotation_hierarchy, "Appendicularia"))
biomassLW_Appendicularia$DW_mg_ind <- 1/1000*(10^(3.21*log10(biomassLW_Appendicularia$estimated_L*1000)-9.11))
biomassLW_Appendicularia$DW_mg_m3 <- biomassLW_Appendicularia$acq_sub_part*biomassLW_Appendicularia$DW_mg_ind/biomassLW_Appendicularia$sample_tot_vol

#Chaetognatha: from Richter (1994)
biomassLW_Chaetognatha <- lW %>% filter(str_detect(object_annotation_hierarchy, "Chaetognatha"))
#biomassLW_Chaetognatha$DW_mg_m3 <- 0.00032*biomassLW_Chaetognatha$estimated_L^3 => Matthews and Hestad (1977)
biomassLW_Chaetognatha$DW_mg_ind <- 0.041*exp(biomassLW_Chaetognatha$estimated_L*0.165)
biomassLW_Chaetognatha$DW_mg_m3 <- biomassLW_Chaetognatha$acq_sub_part*biomassLW_Chaetognatha$DW_mg_ind/biomassLW_Chaetognatha$sample_tot_vol

#Euphausiacea: from Pinchuk & Hopcroft (2007)
biomassLW_Euphausiacea <- lW %>% filter(str_detect(object_annotation_hierarchy, "Euphausiacea"))
biomassLW_Euphausiacea$DW_mg_ind <- 10^(2.5*log10(biomassLW_Euphausiacea$estimated_L)-1.162)
biomassLW_Euphausiacea$DW_mg_m3 <- biomassLW_Euphausiacea$acq_sub_part*biomassLW_Euphausiacea$DW_mg_ind/biomassLW_Euphausiacea$sample_tot_vol

#Isopoda: DW value from Richter (1994)
biomassLW_Isopoda <- lW %>% filter(str_detect(object_annotation_hierarchy, "Isopoda"))
biomassLW_Isopoda$DW_mg_ind <- 0.03
biomassLW_Isopoda$DW_mg_m3 <- biomassLW_Isopoda$acq_sub_part*biomassLW_Isopoda$DW_mg_ind/biomassLW_Isopoda$sample_tot_vol

#Mysida, calculated as Euphausids
biomassLW_Mysida <- lW %>% filter(str_detect(object_annotation_hierarchy, "Mysida"))
biomassLW_Mysida$DW_mg_ind <- 10^(2.5*log10(biomassLW_Mysida$estimated_L)-1.162)
biomassLW_Mysida$DW_mg_m3 <- biomassLW_Mysida$acq_sub_part*biomassLW_Mysida$DW_mg_ind/biomassLW_Mysida$sample_tot_vol

#Echinodermata, unknown, multiple, bivalvia - converted fromm biovolume Postel et al. 2000
biomassLW_Echinodermata <- lW %>% filter(str_detect(object_annotation_hierarchy, "Echinodermata"))
#biomassLW_Unknown <- lW %>% filter(str_detect(object_annotation_hierarchy, "Unknown")) <= ist in Bivalvia mit drin!
biomassLW_Multiple <- lW %>% filter(str_detect(object_annotation_hierarchy, "multiple"))
biomassLW_Bivalvia <- lW %>% filter(str_detect(object_annotation_hierarchy, "Bivalvia"))
biomassLW_Ello <- lW %>% filter(str_detect(object_annotation_hierarchy, "Ellobiopsidae"))

biomassLW_others <- rbind(biomassLW_Echinodermata, biomassLW_Multiple, biomassLW_Bivalvia, biomassLW_Ello)
biomassLW_others$DW_mg_m3 <- 1.025*0.2*biomassLW_others$biovol_maj_mm3_m3
biomassLW_others$DW_mg_ind <- 1.025*0.2*biomassLW_others$spher_vol_maj 

#combine all calculations of non-copepods
biomassLW_non_Copepoda <- rbind(biomassLW_Cnidaria, biomassLW_Amphipoda, biomassLW_Appendicularia, 
                          biomassLW_Chaetognatha, biomassLW_Clione, biomassLW_Euphausiacea, 
                          biomassLW_Gastropoda, biomassLW_Hymenodora, biomassLW_Isopoda, 
                          biomassLW_Mysida, biomassLW_Ostracoda, biomassLW_others, 
                          biomassLW_Polychaeta)

#Copepoda
#Aetideidae: from Richter (1994)
biomassLW_Aetideidaex <- lW %>% filter(str_detect(object_annotation_hierarchy, "Aetideidae"))
biomassLW_Aetideidae <- biomassLW_Aetideidaex %>% filter(!str_detect(object_annotation_hierarchy, "Aetideopsis"))
biomassLW_Aetideopsis <- lW %>% filter(str_detect(object_annotation_hierarchy, "Aetideopsis"))

biomassLW_Aetideidae$DW_mg_ind <- 0.01*biomassLW_Aetideidae$estimated_L^3.412
biomassLW_Aetideidae$DW_mg_m3 <- biomassLW_Aetideidae$acq_sub_part*biomassLW_Aetideidae$DW_mg_ind/biomassLW_Aetideidae$sample_tot_vol
biomassLW_Aetideopsis$DW_mg_ind <- 0.005*biomassLW_Aetideopsis$estimated_L^4.659
biomassLW_Aetideopsis$DW_mg_m3 <- biomassLW_Aetideopsis$acq_sub_part*biomassLW_Aetideopsis$DW_mg_ind/biomassLW_Aetideopsis$sample_tot_vol

#Augaptilidae: conversion for copepods after Kosobokova & Hirche (2002)
biomassLW_Augaptilidae <- lW %>% filter(str_detect(object_annotation_hierarchy, "Augaptilidae"))
biomassLW_Augaptilidae$DW_mg_m3 <- 1.025*0.16*biomassLW_Augaptilidae$biovol_maj_mm3_m3
biomassLW_Augaptilidae$DW_mg_ind <- 1.025*0.16*biomassLW_Augaptilidae$spher_vol_maj 

#Calanus: length-weight relationship from our study (Power curve) and Hirche & Mumm (1992)
biomassLW_Calanus <- lW %>% filter(str_detect(object_annotation_hierarchy, ">Calanus"))
biomassLW_Cfinglac <- biomassLW_Calanus %>% filter(!str_detect(object_annotation_hierarchy, "Calanus hyperboreus"))
biomassLW_Chyp <- biomassLW_Calanus %>% filter(str_detect(object_annotation_hierarchy, "Calanus hyperboreus"))

biomassLW_Cfinglac$DW_mg_ind <- 0.029014*biomassLW_Cfinglac$estimated_L^2.6932426
biomassLW_Cfinglac$DW_mg_m3 <- biomassLW_Cfinglac$acq_sub_part*biomassLW_Cfinglac$DW_mg_ind/biomassLW_Cfinglac$sample_tot_vol
biomassLW_Chyp$DW_mg_ind <- 0.039802*biomassLW_Chyp$estimated_L^2.285416
biomassLW_Chyp$DW_mg_m3 <- biomassLW_Chyp$acq_sub_part*biomassLW_Chyp$DW_mg_ind/biomassLW_Chyp$sample_tot_vol

#Stadien
#chypF_LW <- biomassLW_Chyp %>% filter(str_detect(object_annotation_category, "female"))
#chypC5_LW <- biomassLW_Chyp %>% filter(str_detect(object_annotation_category, "CVstage"))
#chypC4_LW <- biomassLW_Chyp %>% filter(str_detect(object_annotation_category, "CIVstage"))
#chyp_LW_all <- rbind(chypF_LW, chypC5_LW, chypC4_LW)
#DW_LW_Chyp_mgm3_aggr <- aggregate(DW_mg_m3 ~ sample + object_depth_min +  
#                                      + object_depth_max + object_annotation_category,
#                                    data=chyp_LW_all, FUN=sum)

#DW_LW_Chyp_mgm3_select <- select(DW_LW_Chyp_mgm3_aggr, sample, object_annotation_category, DW_mg_m3)
#DW_LW_Chyp_mgm3 <- spread(DW_LW_Chyp_mgm3_select, object_annotation_category, DW_mg_m3)
#DW_LW_Chyp_mgm3 <- DW_LW_Chyp_mgm3 %>% remove_rownames %>% column_to_rownames(var="sample")
#DW_LW_Chyp_mgm3$Chyperboreus <- rowSums(DW_LW_Chyp_mgm3[], na.rm=TRUE)

#biomassLW_Cfinglac$DW_mg_ind2 <- 0.0095*biomassLW_Cfinglac$estimated_L^3.3838
#biomassLW_Cfinglac$DW_mg_m32 <- biomassLW_Cfinglac$acq_sub_part*biomassLW_Cfinglac$DW_mg_ind2/biomassLW_Cfinglac$sample_tot_vol
#biomassLW_Chyp$DW_mg_ind2 <- 0.0028*biomassLW_Chyp$estimated_L^3.3899
#biomassLW_Chyp$DW_mg_m32 <- biomassLW_Chyp$acq_sub_part*biomassLW_Chyp$DW_mg_ind2/biomassLW_Chyp$sample_tot_vol

#Heterorhabdidae: from Richter (1994)
biomassLW_Heterorhabdidae <- lW %>% filter(str_detect(object_annotation_hierarchy, "Heterorhabdidae"))
biomassLW_Heterorhabdidae$DW_mg_ind <- 0.003*biomassLW_Heterorhabdidae$estimated_L^4.716
biomassLW_Heterorhabdidae$DW_mg_m3 <- biomassLW_Heterorhabdidae$acq_sub_part*biomassLW_Heterorhabdidae$DW_mg_ind/biomassLW_Heterorhabdidae$sample_tot_vol

#Metridia, Pleuromamma, Lucicutia: f. Metridia longa von Hirche and Mumm (1992)
biomassLW_Metridia <- lW %>% filter(str_detect(object_annotation_hierarchy, "Metridinidae"))
biomassLW_Lucicutia <- lW %>% filter(str_detect(object_annotation_hierarchy, "Lucicutia"))
biomassLW_Metridinidae <- rbind(biomassLW_Metridia , biomassLW_Lucicutia)
biomassLW_Metridinidae$DW_mg_ind <- 0.0121*biomassLW_Metridinidae$estimated_L^3.0167
biomassLW_Metridinidae$DW_mg_m3 <- biomassLW_Metridinidae$acq_sub_part*biomassLW_Metridinidae$DW_mg_ind/biomassLW_Metridinidae$sample_tot_vol

#Clausocalanidae:Microcalanus, Pseudocalanus: from Lui & Hopcroft (2008) 
biomassLW_Microcalanus <- lW %>% filter(str_detect(object_annotation_hierarchy, "Microcalanus"))
biomassLW_Pseudocalanus <- lW %>% filter(str_detect(object_annotation_hierarchy, "Pseudocalanus"))
biomassLW_Clausocalanidae <- rbind(biomassLW_Microcalanus,biomassLW_Pseudocalanus)
biomassLW_Clausocalanidae$DW_mg_ind <- (10^(2.85*log10(biomassLW_Clausocalanidae$estimated_L*1000)-7.62))/1000
biomassLW_Clausocalanidae$DW_mg_m3 <- biomassLW_Clausocalanidae$acq_sub_part*biomassLW_Clausocalanidae$DW_mg_ind/biomassLW_Clausocalanidae$sample_tot_vol

#Paraeuchaeta: from Mumm (1991)
biomassLW_Paraeuchaeta <- lW %>% filter(str_detect(object_annotation_hierarchy, "Paraeuchaeta"))
biomassLW_Paraeuchaeta$DW_mg_ind <- 100/90*(0.0075*biomassLW_Paraeuchaeta$estimated_L^3.274)
biomassLW_Paraeuchaeta$DW_mg_m3 <- biomassLW_Paraeuchaeta$acq_sub_part*biomassLW_Paraeuchaeta$DW_mg_ind/biomassLW_Paraeuchaeta$sample_tot_vol

#Scaphocalanus: from Yamaguchi et al. (2020)
biomassLW_Scaphocalanus <- lW %>% filter(str_detect(object_annotation_hierarchy, "Scaphocalanus"))
biomassLW_Scaphocalanus$DW_mg_ind <- 1/1000*(10.69*biomassLW_Scaphocalanus$estimated_L^3.341)
biomassLW_Scaphocalanus$DW_mg_m3 <- biomassLW_Scaphocalanus$acq_sub_part*biomassLW_Scaphocalanus$DW_mg_ind/biomassLW_Scaphocalanus$sample_tot_vol

#Scolecithricella: from Nakamura et al. (2017)
biomassLW_Scolecithricella <- lW %>% filter(str_detect(object_annotation_hierarchy, "Scolecithricella"))
biomassLW_Scolecithricella$DW_mg_ind <- (10^(3.669*log10(biomassLW_Scolecithricella$estimated_L*1000)-9.739))/1000
biomassLW_Scolecithricella$DW_mg_m3 <- biomassLW_Scolecithricella$acq_sub_part*biomassLW_Scolecithricella$DW_mg_ind/biomassLW_Scolecithricella$sample_tot_vol

#Oithonidae: from Nakamura et al. (2017)
biomassLW_Oithonidae <- lW %>% filter(str_detect(object_annotation_hierarchy, "Oithonidae"))
biomassLW_Oithonidae$DW_mg_ind <- (10^(1.997*log10(biomassLW_Oithonidae$estimated_L*1000)-5.3245))/1000
biomassLW_Oithonidae$DW_mg_m3 <- biomassLW_Oithonidae$acq_sub_part*biomassLW_Oithonidae$DW_mg_ind/biomassLW_Oithonidae$sample_tot_vol

#Oncaeidae: from Nakamura et al. (2017)
biomassLW_Oncaeidae <- lW %>% filter(str_detect(object_annotation_hierarchy, "Oncaeidae"))
biomassLW_Oncaeidae$DW_mg_ind <- (10^(2.875*log10(biomassLW_Oncaeidae$estimated_L*1000)-7.458))/1000
biomassLW_Oncaeidae$DW_mg_m3 <- biomassLW_Oncaeidae$acq_sub_part*biomassLW_Oncaeidae$DW_mg_ind/biomassLW_Oncaeidae$sample_tot_vol

#Nauplien, Copepoda (unidentified, badfocus, part), Calanoida, Spinocalanus,Temorites,Tharybis,
#Xanthocalanus, Harpacticoida, Mormonilloida: conversion 0.16 for Arctic Copepoda according to Kososbokova and Hirche (2000)
biomassLW_nauplii <- lW %>% filter(str_detect(object_annotation_category, "nauplii<Crustacea"))
biomassLW_Cop_unid <- lW %>% filter(str_detect(object_annotation_category, "Copepoda"))
biomassLW_Calanoida <- lW %>% filter(str_detect(object_annotation_category, "Calanoida"))
biomassLW_Spinocalanidae <- lW %>% filter(str_detect(object_annotation_hierarchy, "Spinocalanidae"))
biomassLW_Temorites <- lW %>% filter(str_detect(object_annotation_category, "Temorites"))
biomassLW_Xanthocalanus <- lW %>% filter(str_detect(object_annotation_category, "Xanthocalanus"))
biomassLW_Tharybis <- lW %>% filter(str_detect(object_annotation_category, "Tharybis"))
biomassLW_Harpacticoida <- lW %>% filter(str_detect(object_annotation_hierarchy, "Harpacticoida"))
biomassLW_Mormonilla <- lW %>% filter(str_detect(object_annotation_hierarchy, "Mormonilla"))

biomassLW_copepods <- rbind(biomassLW_nauplii, biomassLW_Cop_unid, biomassLW_Calanoida,
                            biomassLW_Spinocalanidae, biomassLW_Temorites, biomassLW_Tharybis,
                            biomassLW_Xanthocalanus, biomassLW_Harpacticoida, biomassLW_Mormonilla)

biomassLW_copepods$DW_mg_m3 <- 1.025*0.16*biomassLW_copepods$biovol_maj_mm3_m3
biomassLW_copepods$DW_mg_ind <- 1.025*0.16*biomassLW_copepods$spher_vol_maj 
                     
biomassLW_Copepoda <- rbind(biomassLW_Cfinglac, biomassLW_Chyp, biomassLW_Heterorhabdidae,
                            biomassLW_Metridinidae, biomassLW_Augaptilidae, biomassLW_Aetideidae,
                            biomassLW_Aetideopsis, biomassLW_Clausocalanidae, biomassLW_Paraeuchaeta,
                            biomassLW_Scaphocalanus, biomassLW_Scolecithricella, biomassLW_Oithonidae,
                            biomassLW_Oncaeidae, biomassLW_copepods)

lW_x <- rbind(biomassLW_non_Copepoda, biomassLW_Copepoda)

DW_LW_mgm3_aggr <- aggregate(DW_mg_m3 ~ sample + object_depth_min +  
                         + object_depth_max + object_annotation_category,
                         data=lW_x, FUN=sum)

DW_LW_mgm3_select <- select(DW_LW_mgm3_aggr, sample, object_annotation_category, DW_mg_m3)
DW_LW_mgm3 <- spread(DW_LW_mgm3_select, object_annotation_category, DW_mg_m3)

#DW_LW_mgm3 <- DW_LW_mgm3 %>% remove_rownames %>% column_to_rownames(var="sample")
#DW_LW_mgm3$Total <- rowSums(DW_LW_mgm3[], na.rm=TRUE)

write.table(DW_LW_mgm3, "/Users/acornils/Documents/Veröffentlichungen/2022_L&OMethods_PS78/PS78_PANGAEA/x_ps78_transect_biomass_LW.txt", sep="\t",row.names=F)

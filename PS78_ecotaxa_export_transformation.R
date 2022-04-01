#PS78 ecotaxa
library(tidyverse)

#import file in R
#"encoding" allows to import µ in header
#biovolume <- read.table("/Users/acornils/Desktop/2021_Manuscript_PS78/ecotaxa_export_2771_20210627_1346.tsv", 
                       # header=TRUE, sep="\t", encoding="latin1")
biovolume <- read.table("/Users/acornils/Desktop/PS78/ecotaxa_export_2771_20220319_1349.tsv", 
                        header=TRUE, sep="\t", encoding="latin1")

#select relevant columns to calculate biovolume
#selected columns for database upload
biovolume$sample <- biovolume$sample_id
biovolume$Haul <- biovolume$sample_stationid
biovolume$Region <- "Fram Strait"
biovolume$Detail_Location <- biovolume$sample_stationid
biovolume$Comment <- ""
biovolume$process_particle_pixel_size_mm <- 0.0106

biovolume_select <- select(biovolume, sample_ship, sample_program, sample_stationid, Haul, Region, Detail_Location, Comment, 
                           object_date, object_time, object_lat, object_lon, sample_bottomdepth, object_depth_min,
                           object_depth_max, object_annotation_category, object_annotation_hierarchy, object_annotation_person_name,
                           sample, object_id, sample_id, sample_tot_vol, acq_sub_part, 
                           object_area, object_major, object_minor, object_area_exc, process_particle_pixel_size_mm)

#delete all non-plankton categories (adjust according to dataset)
biovolume_select<-biovolume_select[!(biovolume_select$object_annotation_category=="bubble" | 
                                       biovolume_select$object_annotation_category=="fiber<detritus"| 
                                       biovolume_select$object_annotation_category=="Ellobiopsidae"|
                                       biovolume_select$object_annotation_category=="multiple<plastic"|
                                       biovolume_select$object_annotation_category=="multiple<other"|
                                       biovolume_select$object_annotation_category=="detritus" | 
                                       biovolume_select$object_annotation_category=="egg<Acartia sinjiensis" | 
                                       biovolume_select$object_annotation_category=="artefact"| 
                                       biovolume_select$object_annotation_category=="antenna<Crustacea" | 
                                       biovolume_select$object_annotation_category=="leg<Crustacea"| 
                                       biovolume_select$object_annotation_category=="dead<Copepoda"| 
                                       biovolume_select$object_annotation_category=="Ostracoda X"| 
                                       biovolume_select$object_annotation_category=="egg sac<egg"| 
                                       biovolume_select$object_annotation_category=="feces" |
                                       biovolume_select$object_annotation_category=="part<Copepoda" |
                                       biovolume_select$object_annotation_category=="Foraminifera"),]



#PS78 specific
biovolume_select <- biovolume_select %>% mutate(sample= gsub("_gr","",sample))
biovolume_select <- biovolume_select %>% mutate(sample= gsub("_kl","",sample))
biovolume_select <- biovolume_select %>% mutate(sample= gsub("_64","",sample))
biovolume_select <- biovolume_select %>% mutate(sample= gsub("_500","",sample))
biovolume_select <- biovolume_select %>% mutate(sample= gsub("_1000","",sample))
biovolume_select <- biovolume_select %>% mutate(sample= gsub("_150","",sample))
#biovolume_select$object_depth_min[biovolume_select$object_depth_min == 400 ] <- 300
#biovolume_select$object_lat[biovolume_select$object_lat == 78.8255 ] <- 78.825
#biovolume_select$object_lon[biovolume_select$object_lon == 7.02667 ] <- 7.002667
#biovolume_select$object_lon[biovolume_select$object_lon == 6.18833 ] <- 6.018833
#biovolume_select$object_lon[biovolume_select$object_lon ==  3.99783333333333] <- 3.997833

#PS78 data transformation for database upload
biovolume_select <- biovolume_select %>% mutate(sample_ship= gsub("ps","Polarstern",sample_ship))
biovolume_select <- biovolume_select %>% mutate(sample_program= gsub("ps","",sample_program))
biovolume_select <- biovolume_select %>% mutate(sample_program= gsub("78","PS78",sample_program))
biovolume_select <- biovolume_select %>% mutate(sample_stationid= gsub("127","PS78_127",sample_stationid))
biovolume_select <- biovolume_select %>% mutate(sample_stationid= gsub("19","PS78_019",sample_stationid))
biovolume_select <- biovolume_select %>% mutate(sample_stationid= gsub("25","PS78_025",sample_stationid))
biovolume_select <- biovolume_select %>% mutate(sample_stationid= gsub("35","PS78_035",sample_stationid))
biovolume_select <- biovolume_select %>% mutate(sample_stationid= gsub("39","PS78_039",sample_stationid))
biovolume_select <- biovolume_select %>% mutate(sample_stationid= gsub("44","PS78_044",sample_stationid))
biovolume_select <- biovolume_select %>% mutate(sample_stationid= gsub("54","PS78_054",sample_stationid))
biovolume_select <- biovolume_select %>% mutate(sample_stationid= gsub("75","PS78_075",sample_stationid))
biovolume_select <- biovolume_select %>% mutate(sample_stationid= gsub("71","PS78_071",sample_stationid))

biovolume_select <- biovolume_select %>% mutate(Haul= gsub("127","7",Haul))
biovolume_select <- biovolume_select %>% mutate(Haul= gsub("19","5",Haul))
biovolume_select <- biovolume_select %>% mutate(Haul= gsub("25","3",Haul))
biovolume_select <- biovolume_select %>% mutate(Haul= gsub("35","2",Haul))
biovolume_select <- biovolume_select %>% mutate(Haul= gsub("39","5",Haul))
biovolume_select <- biovolume_select %>% mutate(Haul= gsub("44","4",Haul))
biovolume_select <- biovolume_select %>% mutate(Haul= gsub("54","5",Haul))
biovolume_select <- biovolume_select %>% mutate(Haul= gsub("75","5",Haul))
biovolume_select <- biovolume_select %>% mutate(Haul= gsub("71","5",Haul))

#biovolume_select$sample_tot_vol[grepl("71_n1", biovolume_select$sample)] <- 25
#biovolume_select$sample_tot_vol[grepl("71_n2", biovolume_select$sample)] <- 52
#biovolume_select$sample_tot_vol[grepl("71_n3", biovolume_select$sample)] <- 23
#biovolume_select$sample_tot_vol[grepl("71_n4", biovolume_select$sample)] <- 35
#biovolume_select$sample_tot_vol[grepl("71_n5", biovolume_select$sample)] <- 14

#only necessary for PS78 due to reprocessing of samples with different minimum thresholds
kl627area <- biovolume_select %>% filter(str_detect(sample_id, "150"))
gr627area <- biovolume_select %>% filter(!str_detect(sample_id, "150")) %>% 
  filter(!str_detect(sample_id, "151")) %>% filter(!str_detect(sample_id, "200")) %>% 
  filter(!str_detect(sample_id, "kl_100"))
kl627area = kl627area[kl627area$object_area <= 627, ]
biovolume_adjust <- rbind(kl627area, gr627area)

#add identified multiples (counted manually), set directory as required
ps78_multiples <- read.table("/Users/acornils/Desktop/PS78/multiples/ps78_multiples_ident.txt", 
                        header=TRUE, sep="\t")
ps78_multiples <-ps78_multiples %>% filter(!object_annotation_category=="Foraminifera")
#duplicate rows according to Number per sample for biovolume calcuclation
ps78_multiples_ind <- as.data.frame(lapply(ps78_multiples, rep, ps78_multiples$Number))
ps78_multiples_ind <-  select(ps78_multiples_ind, -c(Number))

biovolume_final <- rbind(biovolume_adjust, ps78_multiples_ind)

#add Location details for database
biovolume_final <- biovolume_final %>% mutate(Detail_Location= gsub("127","8°E",Detail_Location))
biovolume_final <- biovolume_final %>% mutate(Detail_Location= gsub("19","6°E",Detail_Location))
biovolume_final <- biovolume_final %>% mutate(Detail_Location= gsub("25","7°E",Detail_Location))
biovolume_final <- biovolume_final %>% mutate(Detail_Location= gsub("35","4°E",Detail_Location))
biovolume_final <- biovolume_final %>% mutate(Detail_Location= gsub("39","2°E",Detail_Location))
biovolume_final <- biovolume_final %>% mutate(Detail_Location= gsub("44","0°",Detail_Location))
biovolume_final <- biovolume_final %>% mutate(Detail_Location= gsub("54","2°W",Detail_Location))
biovolume_final <- biovolume_final %>% mutate(Detail_Location= gsub("75","4°W",Detail_Location))
biovolume_final <- biovolume_final %>% mutate(Detail_Location= gsub("71","5°W",Detail_Location))

#transform values to mm
biovolume_final$area_mm2  <- biovolume_final$object_area * (biovolume_final$process_particle_pixel_size_mm**2) 

biovolume_final$major_mm  <- biovolume_final$object_major * biovolume_final$process_particle_pixel_size_mm

biovolume_final$minor_mm  <- biovolume_final$object_minor * biovolume_final$process_particle_pixel_size_mm

biovolume_final$area_exc_mm2  <- biovolume_final$object_area_exc * (biovolume_final$process_particle_pixel_size_mm**2) 

biovolume_final$area_majmin_mm2  <- pi * biovolume_final$major_mm/2 * biovolume_final$minor_mm/2

biovolume_final$esd_mm  <- 2 * (sqrt(biovolume_final$area_mm2/pi))

biovolume_final$esd_exc_mm  <- 2 * (sqrt(biovolume_final$area_exc_mm2/pi))

biovolume_final$esd_maj_min_mm <- 2 * (sqrt(biovolume_final$area_majmin_mm2/pi))

#calculate biovolume
#from major_minor
#Spherical Volume from major_minor - mm3
biovolume_final$spher_vol_maj <- 4/3 * pi * ((biovolume_final$major_mm / 2) * (biovolume_final$minor_mm / 2) * (biovolume_final$minor_mm / 2))
#Biovolume from major_minor - mm3/m3
biovolume_final$biovol_maj_mm3_m3 <- 
  (biovolume_final$spher_vol_maj * biovolume_final$acq_sub_part) / biovolume_final$sample_tot_vol

#abundance per image
biovolume_final$abundance_m3 <- biovolume_final$acq_sub_part / biovolume_final$sample_tot_vol 

#standing stock
biovolume_final$Number_m2 <- biovolume_final$abundance_m3 * (biovolume_final$object_depth_max - biovolume_final$object_depth_min)

#Adjustments for abundance
#drop "part<Cnidaria" categories, are needed for biovolume
#biovolume_final<- biovolume_final %>% filter(!str_detect(object_annotation_category, "part<Cnidaria"))

#create biovolume table
ps78_biobol_aggr <- aggregate(biovol_maj_mm3_m3 ~ sample + object_annotation_category, data=biovolume_final, FUN=sum)
wide <- spread(ps78_biobol_aggr, object_annotation_category,biovol_maj_mm3_m3)
write.table(ps78_biobol_aggr, "/Users/acornils/Desktop/PS78/ps78_biovolume_transect", sep="\t",row.names=F)

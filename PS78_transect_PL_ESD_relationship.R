#ESD - Prosome length relationships for biomass calculations
library(tidyverse)
library(ggplot2)
library(ggpubr)

#extract all Calanus images from script "PS78_ecotaxa_database" from the product "biovolume_final"
Calanus_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Calanus"))
Calanus_fg_all <- Calanus_all %>% filter(!str_detect(object_annotation_hierarchy, "Calanus hyperboreus"))
Calanus_hyp_all <- Calanus_all %>% filter(str_detect(object_annotation_hierarchy, "Calanus hyperboreus"))
Calanus_fg_all <- Calanus_fg_all %>% mutate(object_annotation_category= gsub("Calanus finmarchicus","Calanus",object_annotation_category))
Calanus_fg_all <- Calanus_fg_all %>% mutate(object_annotation_category= gsub("Calanus glacialis","Calanus",object_annotation_category))

Metridia_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Metridia longa"))

#import measured prosome length from images in R
PL <- read.table("/Users/acornils/Documents/Veröffentlichungen/2022_L&OMethods_PS78/PS78_data_analysis/PS78_length_all.tsv", header=TRUE, sep="\t")
PL <- select(PL, object_id, Length..mm.)
Length <- merge(PL, biovolume_final)

Calanus <- Length %>% filter(str_detect(object_annotation_hierarchy, "Calanus"))
Calanus_hyp <- Calanus %>% filter(str_detect(object_annotation_hierarchy, "Calanus hyperboreus"))
Calanus_fg <- Calanus %>% filter(!str_detect(object_annotation_hierarchy, "Calanus hyperboreus"))
Calanus_fg <- filter(Calanus_fg, object_annotation_category!="Calanus")
Calanus_fg <- Calanus_fg %>% mutate(object_annotation_category= gsub("Calanus finmarchicus","Calanus",object_annotation_category))
Calanus_fg <- Calanus_fg %>% mutate(object_annotation_category= gsub("Calanus glacialis","Calanus",object_annotation_category))

#name species based on lenght measurements
Calanus_fg$species <- 
  ifelse(Calanus_fg$object_annotation_category == "female<Calanus" & Calanus_fg$Length..mm. <3.2,"C. finmarchicus f",
         ifelse (Calanus_fg$object_annotation_category == "female<Calanus" & Calanus_fg$Length..mm. >= 3.2,"C. glacialis f",
                 ifelse(Calanus_fg$object_annotation_category == "CVstage<Calanus" & Calanus_fg$Length..mm. <3.0,"C. finmarchicus C5",
                        ifelse (Calanus_fg$object_annotation_category == "CVstage<Calanus" & Calanus_fg$Length..mm. >= 3.0,"C. glacialis C5",
                                ifelse(Calanus_fg$object_annotation_category == "CIVstage<Calanus" & Calanus_fg$Length..mm. <2.2,"C. finmarchicus C4",
                                       ifelse (Calanus_fg$object_annotation_category == "CIVstage<Calanus" & Calanus_fg$Length..mm. >= 2.2,"C. glacialis C4",
                                               ifelse(Calanus_fg$object_annotation_category == "CIIIstage<Calanus" & Calanus_fg$Length..mm. <1.6,"C. finmarchicus C3",
                                                      ifelse (Calanus_fg$object_annotation_category == "CIIIstage<Calanus" & Calanus_fg$Length..mm. >= 1.6,"C. glacialis C3",
                                                              ifelse(Calanus_fg$object_annotation_category == "CIIstage<Calanus" & Calanus_fg$Length..mm. <1.1,"C. finmarchicus C2", 
                                                                     ifelse (Calanus_fg$object_annotation_category == "CIIstage<Calanus" & Calanus_fg$Length..mm. >= 1.1,"C. glacialis C2",
                                                                             ifelse(Calanus_fg$object_annotation_category == "CIstage<Calanus" & Calanus_fg$Length..mm. <0.8,"C. finmarchicus C1", 
                                                                                    ifelse (Calanus_fg$object_annotation_category == "CIstage<Calanus" & Calanus_fg$Length..mm. >= 0.8,"C. glacialis C1",
                                                                                            ifelse(Calanus_fg$object_annotation_category == "male<Calanus", "Calanus m",
                                                                                                   ifelse(Calanus_fg$object_annotation_category == "Calanus", "Calanus",
                                                                                                          ifelse(Calanus_fg$object_annotation_category == "calanus with ectoparasites", "C. finmarchicus f",
                                                                                                                 ""))))))))))))))) 


#extract all Calanus glacialis
Calanus_g <- Calanus_fg %>% filter(str_detect(species, "glacialis"))

#extract all Calanus finmarchicus
Calanus_f <- Calanus_fg %>% filter(!str_detect(species, "glacialis"))

#establish linear relationship between ESD and measured PL
Calanus_hyp <- Calanus_hyp %>% mutate(object_annotation_category= gsub("CVstage<Calanus hyperboreus","C. hyperboreus C5",object_annotation_category))
Calanus_hyp <- Calanus_hyp %>% mutate(object_annotation_category= gsub("CIVstage<Calanus hyperboreus","C. hyperboreus C4",object_annotation_category))
Calanus_hyp <- Calanus_hyp %>% mutate(object_annotation_category= gsub("CIIIstage<Calanus hyperboreus","C. hyperboreus C3",object_annotation_category))
Calanus_hyp <- Calanus_hyp %>% mutate(object_annotation_category= gsub("CIIstage<Calanus hyperboreus","C. hyperboreus C2",object_annotation_category))
Calanus_hyp <- Calanus_hyp %>% mutate(object_annotation_category= gsub("CIstage<Calanus hyperboreus","C. hyperboreus C1",object_annotation_category))
Calanus_hyp <- Calanus_hyp %>% mutate(object_annotation_category= gsub("female<Calanus hyperboreus","C. hyperboreus f",object_annotation_category))
Calanus_hyp <- Calanus_hyp %>% mutate(object_annotation_category= gsub("male<Calanus hyperboreus","C. hyperboreus m",object_annotation_category))

#stage_Chyp <- Calanus_hyp$object_annotation_category
#Calanus_hyp %>%
#  ggplot(aes(esd_maj_min_mm, Length..mm.)) +
#  ylab("Prosome length (mm)") + xlab("ESD (mm)") + xlim(0,8) + ylim(0,8)+
#  ggtitle("Calanus hyperboreus") + geom_point(col="black", size=1) + 
#  theme_classic() + geom_abline(intercept=0, slope=1, col="black",linetype="dashed") + 
#  stat_smooth(method="glm", formula= y ~ (x)) + 
#  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y = 7.5)+
#  stat_regline_equation(label.y = 7.9, formula=y ~ (x))

#Calanus_hyp %>%
#  ggplot(aes(object_annotation_category, Length..mm., group=station_f)) +
#  ylab("Prosome length (mm)") + xlab("developmental stage") + ylim(0,8)+
#  ggtitle("Calanus hyperboreus") + geom_point(aes(color=stage_Chyp), size=1) + geom_boxplot() + theme_classic() 

#Calanus_fg %>%
#  ggplot(aes(esd_maj_min_mm, Length..mm.)) +
#  ylab("Prosome length (mm)") + xlab("ESD (mm)") + xlim(0,4) + ylim(0,4)+
#  ggtitle("Calanus finmarchicus/glacialis") + geom_point(col="black", size=1) + 
#  theme_classic() + geom_abline(intercept=0, slope=1, col="black",linetype="dashed") + 
#  stat_smooth(method="gam", formula= y ~ (x)) + 
#  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y = 3.7)+
#  stat_regline_equation(label.y = 3.9, formula=y ~ (x))

#stage_Cfin <- Calanus_f$species
#Calanus_f %>%
#  ggplot(aes(esd_maj_min_mm, Length..mm.)) +
#  ylab("Prosome length (mm)") + xlab("ESD (mm)") + xlim(0,3.3) + ylim(0,5)+
#  ggtitle("Calanus finmarchicus/glacialis") + geom_point(aes(color=stage_Cfin), size=1) + 
#  theme_classic() + geom_abline(intercept=0, slope=1, col="black",linetype="dashed") + 
#  stat_smooth(method="gam", formula= y ~ (x)) + 
#  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y = 3.1)+
#  stat_regline_equation(label.y = 3.3, formula=y ~ (x))

#Calanus_f %>%
#  ggplot(aes(species, Length..mm.), group=stage_Cfin) +
#  ylab("Prosome length (mm)") + xlab("developmental stage") + ylim(0,4)+
#  ggtitle("Calanus finmarchicus") + geom_point(aes(color=stage_Cfin), size=1) + geom_boxplot()+ theme_classic() 

#stage_Cglac <- Calanus_g$species
#Calanus_g %>%
#  ggplot(aes(esd_maj_min_mm, Length..mm.)) +
#  ylab("Prosome length (mm)") + xlab("ESD (mm)") + xlim(0,4) + ylim(0,4)+
#  ggtitle("Calanus glacialis") + geom_point(aes(color=stage_Cglac), size=1) + 
#  theme_classic() + geom_abline(intercept=0, slope=1, col="black",linetype="dashed") + 
#  stat_smooth(method="glm", formula= y ~ (x)) + 
#  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y = 3.7)+
#  stat_regline_equation(label.y = 3.9, formula=y ~ (x))

#Calanus_g %>%
#  ggplot(aes(species, Length..mm.), group=stage_Cglac) +
#  ylab("Prosome length (mm)") + xlab("developmental stage") + ylim(0,4)+
#  ggtitle("Calanus glacialis") + geom_point(aes(color=stage_Cglac), size=1) + geom_boxplot()+ theme_classic() 


#calculate PL from linear relationship between ESD and measured PL
#Calanus hyperboreus
Calanus_hyp_all$estimated_PL <- 0.23 + 1.3*(Calanus_hyp_all$esd_maj_min_mm)
#Calanus finmarchicus
#Calanus_fg_all$estimated_PL_f <- 0.013 + 1.3*Calanus_fg_all$esd_maj_min_mm
#Calanus glacialis
#Calanus_fg_all$estimated_PL_g <- 0.094 + 1.3*Calanus_fg_all$esd_maj_min_mm

#Calanus fin/glac
#Final formula!!! Calanus_fg_all$estimated_PL
Calanus_fg_all$estimated_PL <- 0.029 + 1.3*Calanus_fg_all$esd_maj_min_mm
#Calanus_fg_all$estimated_PL_g <- 0.019 + 0.75*Calanus_fg_all$major_mm

#determine Calanus hyperboreus
Calanus_hyp_all$species <- Calanus_hyp_all$object_annotation_category
Calanus_hyp_all$species <- 
  ifelse(Calanus_hyp_all$object_annotation_category == "female<Calanus hyperboreus"  ,"C. hyperboreus f",
   ifelse(Calanus_hyp_all$object_annotation_category == "CVstage<Calanus hyperboreus" ,"C. hyperboreus C5",
    ifelse(Calanus_hyp_all$object_annotation_category == "CIVstage<Calanus hyperboreus" ,"C. hyperboreus C4",
     ifelse(Calanus_hyp_all$object_annotation_category == "CIIIstage<Calanus hyperboreus" ,"C. hyperboreus C3",
      ifelse(Calanus_hyp_all$object_annotation_category == "CIIstage<Calanus hyperboreus" ,"C. hyperboreus C2", 
       ifelse(Calanus_hyp_all$object_annotation_category == "CIstage<Calanus hyperboreus" ,"C. hyperboreus C1", 
        ""))))))  

#determine species from estimated length
Calanus_fg_all$species <- 
  ifelse(Calanus_fg_all$object_annotation_category == "female<Calanus" & Calanus_fg_all$estimated_PL <3.2,"C. finmarchicus f",
   ifelse (Calanus_fg_all$object_annotation_category == "female<Calanus" & Calanus_fg_all$estimated_PL >= 3.2,"C. glacialis f",
    ifelse(Calanus_fg_all$object_annotation_category == "CVstage<Calanus" & Calanus_fg_all$estimated_PL <3.0,"C. finmarchicus C5",
      ifelse (Calanus_fg_all$object_annotation_category == "CVstage<Calanus" & Calanus_fg_all$estimated_PL >= 3.0,"C. glacialis C5",
       ifelse(Calanus_fg_all$object_annotation_category == "CIVstage<Calanus" & Calanus_fg_all$estimated_PL <2.2,"C. finmarchicus C4",
        ifelse (Calanus_fg_all$object_annotation_category == "CIVstage<Calanus" & Calanus_fg_all$estimated_PL >= 2.2,"C. glacialis C4",
         ifelse(Calanus_fg_all$object_annotation_category == "CIIIstage<Calanus" & Calanus_fg_all$estimated_PL <1.6,"C. finmarchicus C3",
          ifelse (Calanus_fg_all$object_annotation_category == "CIIIstage<Calanus" & Calanus_fg_all$estimated_PL >= 1.6,"C. glacialis C3",
           ifelse(Calanus_fg_all$object_annotation_category == "CIIstage<Calanus" & Calanus_fg_all$estimated_PL <1.1,"C. finmarchicus C2", 
            ifelse (Calanus_fg_all$object_annotation_category == "CIIstage<Calanus" & Calanus_fg_all$estimated_PL >= 1.1,"C. glacialis C2",
             ifelse(Calanus_fg_all$object_annotation_category == "CIstage<Calanus" & Calanus_fg_all$estimated_PL <0.8,"C. finmarchicus C1", 
              ifelse (Calanus_fg_all$object_annotation_category == "CIstage<Calanus" & Calanus_fg_all$estimated_PL >= 0.8,"C. glacialis C1",
               ifelse(Calanus_fg_all$object_annotation_category == "male<Calanus", "C. finmarchicus m",
                ifelse(Calanus_fg_all$object_annotation_category == "Calanus", "Calanus",
                 ifelse(Calanus_fg_all$object_annotation_category == "calanus with ectoparasites", "Calanus",
                  "")))))))))))))))  

#Metridia longa
Metridia <- Length %>% filter(str_detect(object_annotation_hierarchy, "Metridia"))
Metridia <- filter(Metridia, object_annotation_category!="Metridia lucens")
Metridia <- filter(Metridia, object_annotation_category!="Metridia")
Metridia$object_annotation_category[Metridia$object_annotation_category == "female with ectoparasites<Metridia longa" ] <- "M. longa f"
Metridia <- Metridia %>% mutate(object_annotation_category= gsub("CVstage<Metridia longa","M. longa C5",object_annotation_category))
Metridia <- Metridia %>% mutate(object_annotation_category= gsub("CIVstage<Metridia longa","M. longa C4",object_annotation_category))
Metridia <- Metridia %>% mutate(object_annotation_category= gsub("CIIIstage<Metridia longa","M. longa C3",object_annotation_category))
Metridia <- Metridia %>% mutate(object_annotation_category= gsub("CIIstage<Metridia longa","M. longa C2",object_annotation_category))
Metridia <- Metridia %>% mutate(object_annotation_category= gsub("CIstage<Metridia longa","M. longa C1",object_annotation_category))
Metridia <- Metridia %>% mutate(object_annotation_category= gsub("female<Metridia longa","M. longa f",object_annotation_category))
Metridia <- Metridia %>% mutate(object_annotation_category= gsub("male<Metridia longa","M. longa m",object_annotation_category))

#station_f <- Metridia$object_annotation_category
#Metridia %>%
#  ggplot(aes(esd_maj_min_mm, Length..mm.)) +
#  ylab("Prosome length (mm)") + xlab("ESD (mm)") + xlim(0,5) + ylim(0,5)+
#  ggtitle("Metridia longa") + geom_point(aes(color=station_f), size=1) + 
#  theme_classic() + geom_abline(intercept=0, slope=1, col="black",linetype="dashed") + 
#  geom_smooth(method="gam", formula = y ~ x) +  
#  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y = 4.5)+ 
#  stat_regline_equation(label.y = 4.8)

Metridia_all$object_annotation_category[Metridia_all$object_annotation_category == "female with ectoparasites<Metridia longa" ] <- "M. longa f"
Metridia_all <- Metridia_all %>% mutate(object_annotation_category= gsub("CVstage<Metridia longa","M. longa C5",object_annotation_category))
Metridia_all <- Metridia_all %>% mutate(object_annotation_category= gsub("CIVstage<Metridia longa","M. longa C4",object_annotation_category))
Metridia_all <- Metridia_all %>% mutate(object_annotation_category= gsub("CIIIstage<Metridia longa","M. longa C3",object_annotation_category))
Metridia_all <- Metridia_all %>% mutate(object_annotation_category= gsub("CIIstage<Metridia longa","M. longa C2",object_annotation_category))
Metridia_all <- Metridia_all %>% mutate(object_annotation_category= gsub("CIstage<Metridia longa","M. longa C1",object_annotation_category))
Metridia_all <- Metridia_all %>% mutate(object_annotation_category= gsub("female<Metridia longa","M. longa f",object_annotation_category))
Metridia_all <- Metridia_all %>% mutate(object_annotation_category= gsub("male<Metridia longa","M. longa m",object_annotation_category))

#Metridia length
Metridia_all$estimated_PL <- 0.051 + 1.2*Metridia_all$esd_maj_min_mm

#others
Chaetognatha <- Length %>% filter(str_detect(object_annotation_hierarchy, "Chaetognatha"))
Chaetognatha <- Chaetognatha %>% filter(!str_detect(object_annotation_hierarchy, "Chaetognatha>head"))
Chaetognatha$species <- "Chaetognatha"
Cnidaria <- Length %>% filter(str_detect(object_annotation_hierarchy, "Cnidaria"))
Cnidaria <- Cnidaria %>% filter(!str_detect(object_annotation_hierarchy, "Cnidaria>part"))
Cnidaria$species <- "Cnidaria"
Appendicularia <- Length %>% filter(str_detect(object_annotation_hierarchy, "Appendicularia"))
Appendicularia <- Appendicularia %>% filter(!str_detect(object_annotation_hierarchy, "Appendicularia>trunk"))
Appendicularia$species <- "Appendicularia"
Polychaeta <- Length %>% filter(str_detect(object_annotation_hierarchy, "Polychaeta"))
Polychaeta$species <- "Polychaeta"
Others <- Length %>% filter(!str_detect(object_annotation_hierarchy, "Crustacea"))
Others$species <- "Others"
Others <- Others %>% filter(!str_detect(object_annotation_hierarchy, "Chaetognatha"))

#Crustacea
Amphipoda <- Length %>% filter(str_detect(object_annotation_hierarchy, "Amphipoda"))
Amphipoda$species <- "Amphipoda"
Euphausiacea <- Length %>% filter(str_detect(object_annotation_hierarchy, "Euphausiacea"))
Euphausiacea$species <- "Euphausiacea"
Ostracoda <- Length %>% filter(str_detect(object_annotation_hierarchy, "Ostracoda"))
Ostracoda$species <- "Ostracoda"

Crustacea <-  Length %>% filter(str_detect(object_annotation_hierarchy, "Crustacea"))
Crustacea$species <- "Crustacea"
Crustacea <-  Crustacea %>% filter(!str_detect(object_annotation_hierarchy, "Copepoda"))
Crustacea <-  Crustacea %>% filter(!str_detect(object_annotation_hierarchy, "nauplii"))
Crustacea <-  Crustacea %>% filter(!str_detect(object_annotation_hierarchy, "Amphipoda"))
Crustacea <-  Crustacea %>% filter(!str_detect(object_annotation_hierarchy, "Euphausiacea"))
Crustacea <-  Crustacea %>% filter(!str_detect(object_annotation_hierarchy, "Ostracoda"))

#Copepoda
Calanoida <- Length %>% filter(str_detect(object_annotation_hierarchy, "Calanoida"))
Calanoida$species <- "Calanoida"
Calanoida <- Calanoida %>% filter(!str_detect(object_annotation_hierarchy, "Calanus"))
Calanoida <- Calanoida %>% filter(!str_detect(object_annotation_hierarchy, "Metridia longa"))
Cyclopoida <- Length %>% filter(str_detect(object_annotation_hierarchy, "Cyclopoida"))
Cyclopoida$species <- "Cyclopoida"
Oncaeidae <- Length %>% filter(str_detect(object_annotation_hierarchy, "Oncaeidae"))
Oncaeidae$species <- "Oncaeidae"
Copepoda <- Length %>% filter(str_detect(object_annotation_hierarchy, "Copepoda"))
Copepoda$species <- "Copepoda"
Copepoda <- Copepoda %>% filter(!str_detect(object_annotation_hierarchy, "Calanus"))
Calanus <- Length %>% filter(str_detect(object_annotation_hierarchy, "Calanus"))
Calanus$species <- "Calanus"
Metridinidae <- Length %>% filter(str_detect(object_annotation_hierarchy, "Metridinidae"))
Metridinidae$species <- "Metridinidae"
Euchaetidae <- Length %>% filter(str_detect(object_annotation_hierarchy, "Euchaetidae"))
Euchaetidae$species <- "Euchaetidae"
Microcalanus <- Length %>% filter(str_detect(object_annotation_hierarchy, "Microcalanus"))
Microcalanus$species <- "Microcalanus"
Clausoc <-  Length %>% filter(str_detect(object_annotation_hierarchy, "Clausoc"))
Clausoc$species <- "Clausocalanidae"
Aetideidae <-  Length %>% filter(str_detect(object_annotation_hierarchy, "Aetideidae"))
Aetideidae$species <- "Aetideidae"
Scolecitrichidae <-  Length %>% filter(str_detect(object_annotation_hierarchy, "Scolecitrichidae"))
Scolecitrichidae$species <- "Scolecitrichidae"
Spinocalanidae <-  Length %>% filter(str_detect(object_annotation_hierarchy, "Spinocalanidae"))
Spinocalanidae$species <- "Spinocalanidae"
Heterorhabdidae <-  Length %>% filter(str_detect(object_annotation_hierarchy, "Heterorhabdidae"))
Heterorhabdidae$species <- "Heterorhabdidae"
Harpacticoida <-  Length %>% filter(str_detect(object_annotation_hierarchy, "Harpacticoida"))
Harpacticoida$species <- "Harpacticoida"
Mormonilloida <-  Length %>% filter(str_detect(object_annotation_hierarchy, "Mormonilla"))
Mormonilloida$species <- "Mormonilloida"

others <- rbind(Chaetognatha, Cnidaria, Appendicularia, Polychaeta)
calanoids <- rbind(Aetideidae,Clausoc, Metridinidae, Euchaetidae, Scolecitrichidae, Spinocalanidae, Heterorhabdidae)
#clausocala <- rbind(Microcalanus, Pseudocalanus)
crustacea <- rbind(Amphipoda, Euphausiacea, Ostracoda, Crustacea)
copepods <- rbind(Calanoida, Cyclopoida, Harpacticoida, Oncaeidae, Mormonilloida)

all <- rbind(Copepoda, Calanus)

#test <- others

#station_f <- others$species
#test %>%
#  ggplot(aes((esd_maj_min_mm), (Length..mm.))) +
#  ylab("Body Length (mm)") + xlab("ESD (mm)") + 
#  ggtitle("Crustacea") + geom_point(aes(color=station_f), size=1) + 
#  theme_classic() + geom_abline(intercept=0, slope=1, col="black",linetype="dashed")+
#  stat_smooth(aes(color=station_f),method="lm", formula= (y) ~ (x), ) + 
#  stat_cor(aes(color=station_f, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
#  stat_regline_equation(aes(color=station_f),formula=(y) ~ (x)) + 
  
#scale_y_continuous(trans='log10',breaks=trans_breaks('log10', function(x) 10^x),labels=trans_format('log10', math_format(10^.x)))+
#scale_x_continuous(trans='log10',breaks=trans_breaks('log10', function(x) 10^x),labels=trans_format('log10',math_format(10^.x)))

#estimated length berechnen

#others
Chaetognatha_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Chaetognatha"))
Chaetognatha_all$species <- "Chaetognatha"
Chaetognatha_all$estimated_L <- -0.25 + 4.2 * Chaetognatha_all$esd_maj_min_mm
Cnidaria_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Cnidaria"))
Cnidaria_all$species <- "Cnidaria"
Cnidaria_all$estimated_L <- 0.22 + 0.9 * Cnidaria_all$esd_maj_min_mm
Polychaeta_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Polychaeta"))
Polychaeta_all$species <- "Polychaeta"
Polychaeta_all$estimated_L <- 0.22 + 1.9 * Polychaeta_all$esd_maj_min_mm
Appendicularia_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Appendicularia"))
Appendicularia_all$species <- "Appendicularia"
Appendicularia_all$estimated_L <- 0.074 + 0.59 * Appendicularia_all$esd_maj_min_mm
Others_all <- biovolume_final %>% filter(!str_detect(object_annotation_hierarchy, "Crustacea"))
Others_all <- Others_all %>% filter(!str_detect(object_annotation_hierarchy, "Polychaeta"))
Others_all <- Others_all %>% filter(!str_detect(object_annotation_hierarchy, "Chaetognatha"))
Others_all <- Others_all %>% filter(!str_detect(object_annotation_hierarchy, "Appendicularia"))
Others_all <- Others_all %>% filter(!str_detect(object_annotation_hierarchy, "Cnidaria"))
Others_all$species <- "Others"
Others_all$estimated_L <- 0.14 + 1 * Others_all$esd_maj_min_mm

non_crustacea <- rbind(Chaetognatha_all, Cnidaria_all, Polychaeta_all, Appendicularia_all, Others_all)

#Crustacea
Amphipoda_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Amphipoda"))
Amphipoda_all$species <- "Amphipoda"
Amphipoda_all$estimated_L <- 0.17 + 1.5 * Amphipoda_all$esd_maj_min_mm
Euphausiacea_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Euphausiacea"))
Euphausiacea_all$species <- "Euphausiacea"
Euphausiacea_all$estimated_L <- 0.34 + 2 * Euphausiacea_all$esd_maj_min_mm
Ostracoda_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Ostracoda"))
Ostracoda_all$species <- "Ostracoda"
Ostracoda_all$estimated_L <- 0.097 + 1.4 * Ostracoda_all$esd_maj_min_mm

Crustacea_all <-  biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Crustacea")) %>% 
  filter(!str_detect(object_annotation_hierarchy, "Crustacea>nauplii"))
Crustacea_all$species <- "Crustacea"
Crustacea_all <-  Crustacea_all %>% filter(!str_detect(object_annotation_hierarchy, "Copepoda"))
Crustacea_all <-  Crustacea_all %>% filter(!str_detect(object_annotation_hierarchy, "Amphipoda"))
Crustacea_all <-  Crustacea_all %>% filter(!str_detect(object_annotation_hierarchy, "Euphausiacea"))
Crustacea_all <-  Crustacea_all %>% filter(!str_detect(object_annotation_hierarchy, "Ostracoda"))
Crustacea_all$estimated_L <- 0.064 + 1.7 * Crustacea_all$esd_maj_min_mm

crustacea_non_copepoda <- rbind(Amphipoda_all, Euphausiacea_all, Ostracoda_all, Crustacea_all)

#Copepoda
Metridinidae_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Metridinidae"))
Metridinidae_all <- Metridinidae_all %>% filter(!str_detect(object_annotation_hierarchy, "Metridia longa"))
Metridinidae_all$species <- "Metridinidae"
Metridinidae_all$estimated_L <- 0.054 + 1.2*Metridinidae_all$esd_maj_min
Metridia_all$estimated_L <- 0.051 + 1.2*Metridia_all$esd_maj_min
Metridia_all[,c("estimated_PL")] <- list(NULL)
Metridia_all$species <- "Metridia longa"
Euchaetidae_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Euchaetidae"))
Euchaetidae_all$species <- "Euchaetidae"
Euchaetidae_all$estimated_L <- 0.081 + 1.2*Euchaetidae_all$esd_maj_min

Clausocalanidae_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Clausocalanidae"))
Clausocalanidae_all$species <- "Clausocalanidae"
Clausocalanidae_all$estimated_L <- 0.09 + 1.4*Clausocalanidae_all$esd_maj_min

Aetideidae_all <-  biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Aetideidae"))
Aetideidae_all$species <- "Aetideidae"
Aetideidae_all$estimated_L <- 0.17 + 1.1*Aetideidae_all$esd_maj_min

Scolecitrichidae_all <-  biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Scolecitrichidae"))
Scolecitrichidae_all$species <- "Scolecitrichidae"
Scolecitrichidae_all$estimated_L <- 0.03 + 1.3*Scolecitrichidae_all$esd_maj_min

Spinocalanidae_all <-  biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Spinocalanidae"))
Spinocalanidae_all$species <- "Spinocalanidae"
Spinocalanidae_all$estimated_L <- 0.063 + 1.3*Spinocalanidae_all$esd_maj_min

Heterorhabdidae_all <-  biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Heterorhabdidae"))
Heterorhabdidae_all$species <- "Heterorhabdidae"
Heterorhabdidae_all$estimated_L <- 0.004 + 1.2*Heterorhabdidae_all$esd_maj_min

#Calanus hyperboreus
Calanus_hyp_all$estimated_L <- 0.23 + 1.3*(Calanus_hyp_all$esd_maj_min_mm)
Calanus_hyp_all[,c("estimated_PL")] <- list(NULL)
#Calanus finmachicus/glacialis
Calanus_fg_all$estimated_L <- 0.029 + 1.3*Calanus_fg_all$esd_maj_min_mm
Calanus_fg_all[,c("estimated_PL")] <- list(NULL)

Calanoida_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Calanoida"))
Calanoida_all <- Calanoida_all %>% filter(!str_detect(object_annotation_hierarchy, "Metridinidae"))
Calanoida_all <- Calanoida_all %>% filter(!str_detect(object_annotation_hierarchy, "Calanus"))
Calanoida_all <- Calanoida_all %>% filter(!str_detect(object_annotation_hierarchy, "Euchaetidae"))
Calanoida_all <- Calanoida_all %>% filter(!str_detect(object_annotation_hierarchy, "Clausocalanidae"))
Calanoida_all <- Calanoida_all %>% filter(!str_detect(object_annotation_hierarchy, "Aetideidae"))
Calanoida_all <- Calanoida_all %>% filter(!str_detect(object_annotation_hierarchy, "Scolecitrichidae"))
Calanoida_all <- Calanoida_all %>% filter(!str_detect(object_annotation_hierarchy, "Spinocalanidae"))
Calanoida_all <- Calanoida_all %>% filter(!str_detect(object_annotation_hierarchy, "Heterorhabdidae"))
Calanoida_all$species <- "Calanoida"
Calanoida_all$estimated_L <- 0.038 + 1.2*Calanoida_all$esd_maj_min

Copepoda_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Copepoda"))
Copepoda_all <- Copepoda_all %>% filter(!str_detect(object_annotation_hierarchy, "Calanoida"))
Copepoda_all$species <- "Copepoda"
Copepoda_all$estimated_L <- 0.031 + 1.2*Copepoda_all$esd_maj_min

nauplii_all <- biovolume_final %>% filter(str_detect(object_annotation_hierarchy, "Crustacea>nauplii"))
nauplii_all$species <- "Crustacea nauplii"
nauplii_all$estimated_L <- 0.031 + 1.2*nauplii_all$esd_maj_min
copepoda_all <- rbind(Calanoida_all, Copepoda_all, Metridinidae_all, Euchaetidae_all, Clausocalanidae_all, Aetideidae_all, Scolecitrichidae_all, Spinocalanidae_all, Heterorhabdidae_all, Calanus_hyp_all, Calanus_fg_all,  Metridia_all)

PS78_estL <- rbind(copepoda_all, non_crustacea, crustacea_non_copepoda, nauplii_all)

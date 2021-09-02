library(readr)
IFOP_21_04_2017 <- read_csv("Downloads/IFOP_21-04-2017.csv")

nuts_eb_dictionary <- read_csv("https://raw.githubusercontent.com/gabgilling/Thesis/master/Data/nuts_eb_dictionary.csv")
reg_preds <- read.csv("https://raw.githubusercontent.com/gabgilling/Thesis/master/Data/regional_predictors.csv")

census_age_gender <- read_csv("Downloads/csvoutput_HC55_2021_09_02_19_10.csv")
census_age_gender$GEO <- ifelse(census_age_gender$GEO == "FR3", "FR30", census_age_gender$GEO)
census_age_gender$NUTS2_old <- census_age_gender$GEO
census_age_gender <- merge(census_age_gender, reg_preds, by = "NUTS2_old")

census_age_gender$CAS <- NULL
census_age_gender$TIME <- NULL
census_age_gender$SIE <- NULL
census_age_gender$LOC <- NULL
census_age_gender$FLAGS <- NULL
census_age_gender$FOOTNOTES <- NULL
census_age_gender$COC<- NULL

census_age_gender$age_cat <- with(census_age_gender, ifelse(AGE %in% c("Y18", "Y19", "Y20-24"), "18-24", 
                                  ifelse(AGE %in% c("Y25-29", "Y30-34"), "25-34", 
                                         ifelse(AGE %in% c("Y35-39", "Y40-44", "Y45-49"), "35-49",
                                                ifelse(AGE %in% c("Y50-64"), "50-64", "65+")))))

census_age_gender$region_new <- with(census_age_gender, ifelse(Region.Name %in% c("Basse-Normandie", "Haute-Normandie"), "Normandie", Region.Name))
census_age_gender$region_new <- with(census_age_gender, ifelse(Region.Name %in% c("Poitou-Charentes", "Aquitaine", "Limousin"), "Nouvelle-Aquitaine", region_new))
census_age_gender$region_new <- with(census_age_gender, ifelse(Region.Name %in% c("Picardie", "Nord-Pas-de-Calais"), "Hauts-de-France", region_new))
census_age_gender$region_new <- with(census_age_gender, ifelse(Region.Name %in% c("Bourgogne", "Franche-Comté"), "Bourgogne-Franche-Comté", region_new))
census_age_gender$region_new <- with(census_age_gender, ifelse(Region.Name %in% c("Languedoc-Roussillon", "Midi-Pyrénées"), "Occitanie", region_new))
census_age_gender$region_new <- with(census_age_gender, ifelse(Region.Name %in% c("Alsace", "Lorraine", "Champagne-Ardenne"), "Grand-Est", region_new))
census_age_gender$region_new <- with(census_age_gender, ifelse(Region.Name %in% c("Auvergne", "Rhône-Alpes"), "Auvergne-Rhône-Alpes", region_new))

marginal_age <- census_age_gender %>% group_by(age_cat, region_new) %>% summarise(freq = sum(VALUE))
marginal_gender <- census_age_gender %>% group_by(SEX, region_new) %>% summarise(freq = sum(VALUE))


IFOP_21_04_2017 <- as.data.frame(ifelse(IFOP_21_04_2017 == "-", 0, IFOP_21_04_2017))

marginal_intentions_share_gender <- as.data.frame(cbind(c("M", "F"), as.numeric(IFOP_21_04_2017$`Emmanuel Macron`[3:4]), as.numeric(IFOP_21_04_2017$`Marine Le Pen`[3:4]),
                                                        as.numeric(IFOP_21_04_2017$`François Fillon`[3:4]), as.numeric(IFOP_21_04_2017$`Benoît Hamon`[3:4]),
                                                        as.numeric(IFOP_21_04_2017$`Jean-Luc Mélenchon`[3:4]),
                                                        as.numeric(IFOP_21_04_2017$`Nathalie Arthaud`[3:4]) + 
                                                         as.numeric(IFOP_21_04_2017$`Philippe Poutou`[3:4]) +
                                                          as.numeric(IFOP_21_04_2017$`Jean Lassalle`[3:4]) +
                                                          as.numeric(IFOP_21_04_2017$`Nicolas Dupont-Aignan`[3:4]) +
                                                          as.numeric(IFOP_21_04_2017$`François Asselineau`[3:4]) +
                                                          as.numeric(IFOP_21_04_2017$`Jacques Cheminade`[3:4])))
                                                  # col.names = c("macron", "lepen"))

colnames(marginal_intentions_share_gender) <- c("SEX", "macron_pct", "lepen_pct", "lr_pct", "ps_pct", "melenchon_pct", "other_pct")


marginal_intentions_share_age <- as.data.frame(cbind(c("18-24", "25-34", "35-49", "50-64", "65+"), 
                                                        c(as.numeric(IFOP_21_04_2017$`Emmanuel Macron`[7:8]), as.numeric(IFOP_21_04_2017$`Emmanuel Macron`[10:12])),
                                                        c(as.numeric(IFOP_21_04_2017$`Marine Le Pen`[7:8]),as.numeric(IFOP_21_04_2017$`Marine Le Pen`[10:12])),
                                                        c(as.numeric(IFOP_21_04_2017$`François Fillon`[7:8]), as.numeric(IFOP_21_04_2017$`François Fillon`[10:12])),
                                                        c(as.numeric(IFOP_21_04_2017$`Benoît Hamon`[7:8]), as.numeric(IFOP_21_04_2017$`Benoît Hamon`[10:12])),
                                                        c(as.numeric(IFOP_21_04_2017$`Jean-Luc Mélenchon`[7:8]),as.numeric(IFOP_21_04_2017$`Jean-Luc Mélenchon`[10:12])),
                                                        c(as.numeric(IFOP_21_04_2017$`Nathalie Arthaud`[7:8]), as.numeric(IFOP_21_04_2017$`Nathalie Arthaud`[10:12])) + 
                                                        c(as.numeric(IFOP_21_04_2017$`Philippe Poutou`[7:8]), as.numeric(IFOP_21_04_2017$`Philippe Poutou`[10:12])) +
                                                        c(as.numeric(IFOP_21_04_2017$`Jean Lassalle`[7:8]), as.numeric(IFOP_21_04_2017$`Jean Lassalle`[10:12])) +
                                                        c(as.numeric(IFOP_21_04_2017$`Nicolas Dupont-
  Aignan`[7:8]), as.numeric(IFOP_21_04_2017$`Nicolas Dupont-
                                                                       Aignan`[7:8])) +
                                                        c(as.numeric(IFOP_21_04_2017$`François Asselineau`[7:8]), as.numeric(IFOP_21_04_2017$`François Asselineau`[10:12])) + 
                                                        c(as.numeric(IFOP_21_04_2017$`Jacques Cheminade`[7:8]), as.numeric(IFOP_21_04_2017$`Jacques Cheminade`[10:12]))))

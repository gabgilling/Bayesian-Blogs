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

# should I just use voting age records?
census_age_gender <- census_age_gender %>% group_by(region_new) %>% mutate(pop_region = sum(VALUE))

marginal_age <- census_age_gender %>% group_by(age_cat, region_new) %>% summarise(freq_a = sum(VALUE))
marginal_gender <- census_age_gender %>% group_by(SEX, region_new) %>% summarise(freq_g = sum(VALUE))

IFOP_21_04_2017 <- as.data.frame(apply(IFOP_21_04_2017, MARGIN = 2, FUN = function(x) ifelse(x == "-", 0, x)))

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

marginal_intentions_share_gender[, 2:ncol(marginal_intentions_share_gender)] <- apply(marginal_intentions_share_gender[, 2:ncol(marginal_intentions_share_gender)], MARGIN = 2, as.numeric)/100


colnames(marginal_intentions_share_gender) <- c("SEX", "macron_pct_g", "lepen_pct_g", "lr_pct_g", "ps_pct_g", "melenchon_pct_g", "other_pct_g")


marginal_intentions_share_age <- as.data.frame(cbind(c("18-24", "25-34", "35-49", "50-64", "65+"), 
                                                        c(as.numeric(IFOP_21_04_2017$`Emmanuel Macron`[7:8]), as.numeric(IFOP_21_04_2017$`Emmanuel Macron`[10:12])),
                                                        c(as.numeric(IFOP_21_04_2017$`Marine Le Pen`[7:8]),as.numeric(IFOP_21_04_2017$`Marine Le Pen`[10:12])),
                                                        c(as.numeric(IFOP_21_04_2017$`François Fillon`[7:8]), as.numeric(IFOP_21_04_2017$`François Fillon`[10:12])),
                                                        c(as.numeric(IFOP_21_04_2017$`Benoît Hamon`[7:8]), as.numeric(IFOP_21_04_2017$`Benoît Hamon`[10:12])),
                                                        c(as.numeric(IFOP_21_04_2017$`Jean-Luc Mélenchon`[7:8]),as.numeric(IFOP_21_04_2017$`Jean-Luc Mélenchon`[10:12])),
                                                        c(as.numeric(IFOP_21_04_2017$`Nathalie Arthaud`[7:8]), as.numeric(IFOP_21_04_2017$`Nathalie Arthaud`[10:12])) + 
                                                        c(as.numeric(IFOP_21_04_2017$`Philippe Poutou`[7:8]), as.numeric(IFOP_21_04_2017$`Philippe Poutou`[10:12])) +
                                                        c(as.numeric(IFOP_21_04_2017$`Jean Lassalle`[7:8]), as.numeric(IFOP_21_04_2017$`Jean Lassalle`[10:12])) +
                                                        c(as.numeric(IFOP_21_04_2017$`Nicolas Dupont-Aignan`[7:8]), as.numeric(IFOP_21_04_2017$`Nicolas Dupont-Aignan`[10:12])) +
                                                        c(as.numeric(IFOP_21_04_2017$`François Asselineau`[7:8]), as.numeric(IFOP_21_04_2017$`François Asselineau`[10:12])) + 
                                                        c(as.numeric(IFOP_21_04_2017$`Jacques Cheminade`[7:8]), as.numeric(IFOP_21_04_2017$`Jacques Cheminade`[10:12]))))

marginal_intentions_share_age[, 2:ncol(marginal_intentions_share_age)] <- apply(marginal_intentions_share_age[, 2:ncol(marginal_intentions_share_age)], MARGIN = 2, as.numeric)/100

colnames(marginal_intentions_share_age) <- c("age_cat", "macron_pct_a", "lepen_pct_a", "lr_pct_a", "ps_pct_a", "melenchon_pct_a", "other_pct_a")

marginals <- merge(marginal_age, marginal_intentions_share_age, by = "age_cat")

marginals <- merge(marginals, marginal_gender, by ="region_new")

marginals <- merge(marginals, marginal_intentions_share_gender, by = "SEX")

marginals <- merge(marginals, census_age_gender %>% select(region_new, pop_region), by = "region_new")

marginals <- distinct(marginals)

marginals$macron_freq_a <- with(marginals, freq_a * macron_pct_a / pop_region)
marginals$macron_freq_g <- with(marginals, freq_g * macron_pct_g / pop_region)

marginals$macron_raw_a <- with(marginals, freq_a * macron_pct_a)
marginals$macron_raw_g <- with(marginals, freq_g * macron_pct_g)

## first round results
french_elections_2017_first_round <- read_csv("Documents/french_elections_2017_first_round.csv")
french_elections_2017_first_round <- french_elections_2017_first_round[c(1,grep("_pct", names(french_elections_2017_first_round)))]
fr2017 <- french_elections_2017_first_round %>% mutate(other_pct = dupontaignan_pct + 
                                                         lasalle_pct + pouton_pct + asselineau_pct+ arthaud_pct + cheminade_pct)

fr2017 <- fr2017 %>% select(region_new, macron_pct ,lepen_pct, fillon_pct ,melenchon_pct, hamon_pct,other_pct)

fr2017 <- fr2017 %>% rename(lr_pct = fillon_pct, ps_pct = hamon_pct)

fr2017[, 2:ncol(fr2017)] <- apply(fr2017[, 2:ncol(fr2017)], MARGIN = 2, as.numeric)/100

marginals_results <- merge(marginals, fr2017, by = "region_new")

fit_macron2017 <- stan_glm(data = marginals_results, macron_pct ~ macron_raw_a + macron_raw_g)

summary(lm(data = marginals_results, macron_pct ~ macron_pct_a + macron_pct_g))

plot(fit_macron2017)

fit_macron2017_glmer <- stan_glmer(data = marginals_results, macron_pct ~ macron_freq_a + macron_freq_g + (1|region_new))
plot(fit_macron2017_glmer, digits = 3)

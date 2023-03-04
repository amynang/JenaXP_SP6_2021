library(tidyverse)
library(openxlsx)

############################ in-core Abundances ################################

# the following exclude Hymenoptera, Formicidae, Lumbicidae, Enchytraedae, "Larven, Puppen"
raw.1 = read.xlsx("H:\\JenaSP6_2021\\August 22 Angelos Jena Experiment fertige Zählliste B1, B2,B3, B4_macrofauna.xlsx",
                  sheet = "Block 1",
                  startRow = 2,
                  cols = c(1,3:14,17,18),
                  na.strings = "-",)

raw.2 = read.xlsx("H:\\JenaSP6_2021\\August 22 Angelos Jena Experiment fertige Zählliste B1, B2,B3, B4_macrofauna.xlsx",
                  sheet = "Block 2",
                  startRow = 2,
                  cols = c(1,3:14,17,18),
                  na.strings = "-",)

raw.3 = read.xlsx("H:\\JenaSP6_2021\\August 22 Angelos Jena Experiment fertige Zählliste B1, B2,B3, B4_macrofauna.xlsx",
                  sheet = "Block 3",
                  startRow = 2,
                  cols = c(1,3:14,17,18),
                  na.strings = "-",)

raw.4 = read.xlsx("H:\\JenaSP6_2021\\August 22 Angelos Jena Experiment fertige Zählliste B1, B2,B3, B4_macrofauna.xlsx",
                  sheet = "Block 4",
                  startRow = 2,
                  cols = c(1,3:14,17,18),
                  na.strings = "-",)

str(raw.1)
str(raw.2)
str(raw.3)
str(raw.4)

# remove all doubt, make numeric
raw.2$Staphylinidae = as.numeric(gsub("\\?| ", "", raw.2$Staphylinidae))
raw.4$Staphylinidae = as.numeric(gsub("\\?| ", "", raw.4$Staphylinidae))

# do you see an emoticon having a mental breakdown in there? no? just me then
raw.2$`Rynchota./.Larven` = as.numeric(gsub("\\*|_|/.*|\\\\.*", "", raw.2$`Rynchota./.Larven`))
raw.4$`Rynchota./.Larven` = as.numeric(gsub("\\*|_|/.*|\\\\.*", "", raw.4$`Rynchota./.Larven`))

macro = bind_rows(raw.1, raw.2, raw.3, raw.4) %>% 
  # add plotcode, correct typo
  rename(Plotcode = X1, Opiliones = Opoliones) %>% 
  # NAs are zeros not absent values
  replace(is.na(.), 0) %>%
  # sum adult and juv hemiptera, rename
  mutate(.keep = "unused",
         .after = Coleoptera.Larven,
         Hemiptera = Rynchota + `Rynchota./.Larven`) %>% 
  mutate(.before = Thysanoptera,
         Plot = str_split(.$Plotcode, "-", simplify = T)[,1],
         Treatment = str_split(.$Plotcode, "T", simplify = T)[,2],) %>% 
  select(-Plotcode)

macro$Plot[macro$Plot == "BA305"] = "B3A05"

table(macro$Plot, macro$Treatment)


############################# Body size measurements ###########################
source("wrangling/functions.R")

l_w_list = vector(mode = "list")

################################ Spiders #######################################

Araneae = rbind(tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Aranea_Measurement_List_JenaExp_Angelos.xlsx",
                                               sheet = "Aranea_B1"), "Araneae"),
                tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Aranea_Measurement_List_JenaExp_Angelos.xlsx",
                                               sheet = "Aranea_B2"), "Araneae"),
                tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Aranea_Measurement_List_JenaExp_Angelos.xlsx",
                                               sheet = "Aranea_B3"), "Araneae"),
                tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Aranea_Measurement_List_JenaExp_Angelos.xlsx",
                                               sheet = "Aranea_B4"), "Araneae"))

############################### Rove beetles ###################################

Staphylinidae = rbind(tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Staphylinidae_Measurement_List_JenaExp_Angelos.xlsx",
                                             sheet = "Staphylinidae_B1"), "Staphylinidae"),
              tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Staphylinidae_Measurement_List_JenaExp_Angelos.xlsx",
                                             sheet = "Staphylinidae_B2"), "Staphylinidae"),
              tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Staphylinidae_Measurement_List_JenaExp_Angelos.xlsx",
                                             sheet = "Staphylinidae_B3"), "Staphylinidae"),
              tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Staphylinidae_Measurement_List_JenaExp_Angelos.xlsx",
                                             sheet = "Staphylinidae_B4"), "Staphylinidae"))

################################ Hemiptera #######################################

Hemiptera = rbind(tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Hemiptera_Measurement_List_JenaExp_Angelos.xlsx",
                                                 sheet = "Hemiptera_B1"), "Hemiptera"),
                  tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Hemiptera_Measurement_List_JenaExp_Angelos.xlsx",
                                                 sheet = "Hemiptera_B2"), "Hemiptera"),
                  tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Hemiptera_Measurement_List_JenaExp_Angelos.xlsx",
                                                 sheet = "Hemiptera_B3"), "Hemiptera"),
                  tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Hemiptera_Measurement_List_JenaExp_Angelos.xlsx",
                                                 sheet = "Hemiptera_B4"), "Hemiptera"))

l_w_list[["Araneae"]] = Araneae
l_w_list[["Staphylinidae"]] = Staphylinidae
l_w_list[["Hemiptera"]] = Hemiptera

################################## the rest ####################################

for (i in c("Chilopoda","Diplopoda","Isopoda",
            "Coleoptera","Thysanoptera","Gastropoda",
            "Formicidae", "Lumbricidae")) {
  
  l_w_list[[i]] = tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Measurement_List_JenaExp_Samples_Mona.xlsx",
                                                 sheet = i), i)
  
}


all = do.call(rbind.data.frame, l_w_list) %>% drop_na() %>% 
  # is width larger than length?
  mutate(weird = length_micro < width_micro) %>% 
  # drop those weird ones
  filter(weird != TRUE) %>% select(-weird) %>% 
  # calculate individual fresh bodymasses
  mutate(FreshMass.mg = case_when( # based on Sohlström 2018 10.1002/ece3.4702
                                  # The general relationship (model 3)
                                  TRUE ~ 10^(- .285 + (1.040 * log10(length_micro/1e3)) + (1.585 * log10(width_micro/1e3))),
                                  # group specific coefficients (model 1)
                                  taxon ==       "Araneae" ~ 10^(- .281 + (1.368 * log10(length_micro/1e3)) + (1.480 * log10(width_micro/1e3))),
                                  taxon ==    "Coleoptera" ~ 10^(- .286 + ( .840 * log10(length_micro/1e3)) + (1.954 * log10(width_micro/1e3))),
                                  taxon == "Staphylinidae" ~ 10^(- .286 + ( .840 * log10(length_micro/1e3)) + (1.954 * log10(width_micro/1e3))),
                                  taxon ==       "Diptera" ~ 10^(- .309 + ( .997 * log10(length_micro/1e3)) + (1.595 * log10(width_micro/1e3))),
                                  taxon ==     "Hemiptera" ~ 10^(- .420 + (1.177 * log10(length_micro/1e3)) + (1.431 * log10(width_micro/1e3))),
                                  taxon ==       "Isopoda" ~ 10^(- .453 + ( .898 * log10(length_micro/1e3)) + (1.756 * log10(width_micro/1e3))),
                                  taxon ==     "Chilopoda" ~ 10^(- .549 + (1.416 * log10(length_micro/1e3)) + (1.543 * log10(width_micro/1e3))),
                                  taxon ==     "Diplopoda" ~ 10^(-1.400 + (2.443 * log10(length_micro/1e3)) + (0.215 * log10(width_micro/1e3))))
  )


all %>% as.data.frame() %>% 
  filter(taxon == "Araneae") %>% 
  select(FreshMass.mg) %>% is.numeric()
  density() %>% 
  plot()

plot(density(log(all[all$taxon == "Hemiptera",]$FreshMass.mg)))

hist(log(all[all$taxon == "Chilopoda",]$FreshMass.mg), breaks = 100)

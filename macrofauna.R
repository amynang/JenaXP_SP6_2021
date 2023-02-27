library(tidyverse)
library(openxlsx)

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
  
  

library(openxlsx)
library(tidyverse)
library(stringr)
library(googlesheets4)

############################## Community Composition ###########################

raw = read.xlsx("H:/JenaSP6_2021/id034_nematodes_240-samples_JenaExperiment_DeltaBEF_July2021_Marcel-Ciobanu__240 samples Jena exp SP6 Nematodes Amyntas2021FINAL.xlsx",
                sheet = "Synthesis",
                rows = 5:245,
                cols = 1:65)

raw[is.na(raw)] = 0
# arrange by sample
data = raw %>% arrange(Sample) 

# replace underscore with D to match the Jexis naming scheme 
data$Sample = gsub("_", "D", data$Sample) 

# break that into block, plot, treatment
this = str_split(data$Sample, "A|D", simplify = T)

data.2 = data %>% add_column(block = this[,1],
                             plot = this[,2],
                             treatment = this[,3],
                             .after = "Sample")

# the unlabeled sample is B1A12D3
table(data.2$block,data.2$plot)
# View(filter(data.2, block == "B1" & plot == 12))
data.2[240,1:4] = as.list(c("B1A12D3","B1","12","3"))

# arrange again
data.2 = data.2 %>% arrange(Sample)

# check again
table(data.2$block,data.2$plot)
# View(filter(data.2, block == "B1" & plot == 12))

# This results in dataset 344 in https://jexis.uni-jena.de
# data4jexis = data.2 %>% select(-(2:4)) %>% pivot_longer(!Sample,
#                                                         names_to = "Taxon",
#                                                         values_to = "Individuals")
# colnames(data4jexis) = c("plotcode","scientific name","quantity")
# write.csv(data4jexis, file = "Identified_Nematodes_dBEF_2021.csv",
#           quote = T,
#           row.names = F,
#           sep = ",")

# transform to frequencies
data.2[,5:68] = vegan::decostand(data.2[,5:68],"total",1) 

# get nematode abundances & soil info
gs4_deauth() #does this work?
abun = read_sheet("https://docs.google.com/spreadsheets/d/1YMQmyhLYfr86CcmwpLwRkLQPxwy4Yk2oyrPKXO8Cf0w/edit#gid=0",
                  sheet = "abundance")
soil = read_sheet("https://docs.google.com/spreadsheets/d/1YMQmyhLYfr86CcmwpLwRkLQPxwy4Yk2oyrPKXO8Cf0w/edit#gid=0",
                  sheet = "soil weight")
# B1A06D1 has init.weight 52.53!, likely a typo reversing 2&5
soil[16,"init.weight"] = 25.53
soil[16,"water.content"] = soil[16,"init.weight"] - soil[16,"net.weight"]

# create sample codes to match the ones in comp data
abun = abun %>% add_column(Sample = str_c(abun$Plot, gsub("Treatment", "D", abun$Subplot)),
                           .before = "Plot")
soil = soil %>% add_column(Sample = str_c(soil$Plot, gsub("Treatment", "D", abun$Subplot)),
                           .before = "Plot")

# This results in dataset 345 in https://jexis.uni-jena.de
# data4jexis = cbind(abun,soil) %>% select(c(2,5,10,12,13)) %>% 
#   mutate(water.content = round(water.content/init.weight, 3)) %>% select(-3)
# colnames(data4jexis) = c("plotcode","quantity","soil dry mass","soil water content")
# write.csv(data4jexis, file = "Counted_Nematodes_dBEF_2021.csv",
#           quote = T,
#           row.names = F,
#           sep = ",")

# check, all good
data.2$Sample == abun$Sample 
data.2$Sample == soil$Sample 

# multiply percent composition with the number of nematodes extracted from each soil sample 
# data.3 = data.2
# data.3[,5:68] = data.2[,5:68] * abun$`Number of Nematodes`/100
data.3 = data.2 %>% mutate(across(where(is.numeric), # for all numeric columns
                           # we multiply by total abundance
                                  ~ .*abun$`Number of Nematodes`), 
                           .keep = "unused")


sum(data.3[1,5:68])
rowSums(data.3[,5:68]) 
# this is interesting...
rowSums(data.3[,5:68]) == abun$`Number of Nematodes`

# now we calculate species densities per 100 gram of dry soil
data.4 = data.3 %>% mutate(across(where(is.numeric), # for all numeric columns
                                  # we divide by grams of dry soil
                                  ~ .*100/soil$net.weight), 
                           .keep = "unused")

# # long format for Jexis
# data.5 = data.4 %>% select(-(2:4)) %>% pivot_longer(where(is.numeric),
#                                                     names_to = "Taxon",
#                                                     values_to = "Density")
# # check, OK
# table(data.5$Taxon)
# 
# data.5[,"Density"] = round(data.5[,"Density"], 7)
# 
# write.csv(data.5, file = "Nematode_Community_Composition_dBEF_2021.csv",
#           quote = F,
#           row.names = F,
#           sep = ",")

# grams of dry soil per cubic centimeter 
# a sq meter has 10000 sq centimeters
# we want to calculate grams of dry soil in a "carpet" of 100*100*10

# soil density measurements for treatments 1 & 2
soil.density = read.csv("H:/JenaSP6_2021/282_4_Dataset/282_4_data.csv",sep = ";")
soil.density = soil.density %>% arrange(plot,treatment) %>% 
  add_column(Sample = str_c(.$plot, .$treatment),
             .before = "plot") %>% select(1:3,7) %>% 
  rename(bulk_soil_density = BulkDensity_Soil)

# soil density measurements for main plots (corresponding to treatment 3 ie control)
soil.density3 = read.table("H:\\JenaSP6_2021\\BulkSoilDensity_mainExp_2020.txt",
                           header = TRUE,
                           sep = "\t") %>% 
  # we only have density for 0-5cm depth in the other two treatments
  # also dropping non-dBEF plots
  filter(upper.depth == 0 & Plot %in% unique(soil.density$plot)) %>% 
  add_column(Sample = str_c(.$Plot, "D3"),
             .before = "Plot") %>% 
  add_column(treatment = "D3",
             .after = "Plot") %>% 
  rename(plot = Plot) %>% 
  select(1:3,6)

# names(soil.density)
# names(soil.density3)

# together
soil.density = soil.density %>% rbind(., soil.density3) %>% 
  arrange(plot, treatment)
  

# now we calculate species densities per gram of dry soil
data.6 = data.3 %>% mutate(across(where(is.numeric), # for all numeric columns
                                  # we divide by grams of dry soil
                                  ~ ./soil$net.weight), 
                           .keep = "unused")

# now we calculate species densities per square meter of land (10cm deep)
data.7 = data.6 %>% #filter(!(treatment == "3")) %>% 
  mutate(across(where(is.numeric), # for all numeric columns
                # we multiply by grams of dry soil per 100*100*10 cubic meters
                ~ .*soil.density$bulk_soil_density*1e05), 
         .keep = "unused")

#> think about rounding the above numbers up to integers if I want to sample 
#> from bodymass distributions
  

############################ Ecophysioligical traits ###########################

taxa = names(raw)[-1]
# Change to synonym accepted by Nemaplex
taxa[taxa == "Macroposthonia"] = "Criconemoides"
# why are these together?
taxa[taxa == "Rhabditidae-dauer.larvae"] = "Rhabditidae"
# query_nemaplex can be found here:
# https://github.com/amynang/marcel/blob/main/R/functions.R
# source("https://raw.githubusercontent.com/amynang/marcel/main/R/functions.R")
# nemaplex = query_nemaplex(taxa)
# write.csv(nemaplex, "wrangling/nemaplex.csv")
nemaplex = read.csv("wrangling/nemaplex.csv", row.names = 1, header = TRUE)

#replace feeding codes with their meaning
nemaplex <- nemaplex %>% mutate(feeding.type = case_when(feeding == "1" ~ "herbivore",
                                                         feeding == "2" ~ "fungivore",
                                                         feeding == "3" ~ "bacterivore",
                                                         feeding == "4" ~ "detritivore",
                                                         feeding == "5" ~ "predator",
                                                         feeding == "6" ~ "eucaryvore",
                                                         feeding == "8" ~ "omnivore"),
                                .keep ="all", 
                                .after = feeding)

nemaplexx <- nemaplex %>% mutate(feeding.type = case_when(feeding.type == "herbivore"   ~ "h.nematodes",
                                                          feeding.type == "fungivore"   ~ "f.nematodes",
                                                          feeding.type == "bacterivore" ~ "b.nematodes",
                                                          feeding.type == "detritivore" ~ "detritivore",
                                                          feeding.type == "predator"    ~ "p.nematodes",
                                                          feeding.type == "eucaryvore"  ~ "eucaryvore",
                                                          feeding.type == "omnivore"    ~ "o.nematodes"),
                                .keep ="all", 
                                .after = feeding) %>% arrange()

nemaplexx$feeding.type = factor(nemaplexx$feeding.type,
                                levels = c("h.nematodes",  
                                           "f.nematodes", "b.nematodes", 
                                           "o.nematodes", "p.nematodes"))

nemaplexx = nemaplexx %>% arrange(feeding.type)


# calculate st deviation from st error
nemaplex$StDevMass = nemaplex$StderrMass * sqrt(nemaplex$N)
# make taxon a column then drop row names
nemaplex = nemaplex %>% mutate(Taxon = rownames(nemaplex),
                               .keep ="all", 
                               .before = cp_value)
rownames(nemaplex) = NULL


#bodymass data from 10.1890/11-0546.1
muldervonk = read.csv("https://raw.githubusercontent.com/amynang/MulderVonk2011/main/Mulder%26Vonk2011_bodymass%26feeding.csv",
                      sep = ";", dec = ",")

#create an empty-ish dataframe
ecophys = data.frame(Taxon = nemaplex$Taxon,
                     AvgMass = NA)

# we will rely on Mulder & Vonk (2011) for bodymass information
# if a taxon is not there we resort to nemaplex
ecophys$AvgMass = nemaplex$AvgMass[match(ecophys$Taxon, nemaplex$Taxon)]
ecophys$StDevMass = nemaplex$StDevMass[match(ecophys$Taxon, nemaplex$Taxon)]


ecophys = ecophys %>% add_column(feeding.type = nemaplex$feeding.type,
                                 .before = "AvgMass") %>% 
  arrange(Taxon)

Bacterivore.nematodes = ecophys$Taxon[ecophys$feeding.type == "bacterivore"]
  Fungivore.nematodes = ecophys$Taxon[ecophys$feeding.type == "fungivore"]
  Herbivore.nematodes = ecophys$Taxon[ecophys$feeding.type == "herbivore"] 
   Omnivore.nematodes = ecophys$Taxon[ecophys$feeding.type == "omnivore"] 
   Predator.nematodes = ecophys$Taxon[ecophys$feeding.type == "predator"] 

micro = data.7 %>% 
  mutate(.after = plot,
         .keep = "unused",
         Plot = str_split(.$Sample, "D", simplify = T)[,1],
         Treatment = paste0("Treatment", treatment)) %>% 
  select(-c(Sample, block, plot)) %>% 
  rename(Rhabditidae = `Rhabditidae-dauer.larvae`,
         Criconemoides.y = Macroposthonia,
         Criconemoides.x = Criconemoides) %>% 
  rowwise() %>% 
  mutate(.keep = "unused",
         Bacterivore.nematodes = sum(pick(any_of(Bacterivore.nematodes))),
           Fungivore.nematodes = sum(pick(any_of(Fungivore.nematodes))),
           Herbivore.nematodes = sum(pick(any_of(Herbivore.nematodes))),
            Omnivore.nematodes = sum(pick(any_of(Omnivore.nematodes))),
            Predator.nematodes = sum(pick(any_of(Predator.nematodes)))) %>% 
  ungroup()



mean.micro.masses = ecophys %>% 
  mutate(AvgMass = (AvgMass*1e-3),
         StDevMass = (StDevMass*1e-3)) %>% 
  group_by(feeding.type) %>% 
  summarise(N = n(),
            MeanMass.mg = median(AvgMass),
            StDMass.mg = sqrt(sum(StDevMass^2)/N)) %>% select(-N) %>% 
  mutate(feeding.type = case_when(feeding.type == "herbivore"   ~ "Herbivore.nematodes",
                                  feeding.type == "fungivore"   ~ "Fungivore.nematodes",
                                  feeding.type == "bacterivore" ~ "Bacterivore.nematodes",
                                  feeding.type == "predator"    ~ "Predator.nematodes",
                                  feeding.type == "omnivore"    ~ "Omnivore.nematodes")) %>% 
  rename(taxon = feeding.type)

library(readxl)
library(tidyverse)
library(stringr)
library(googlesheets4)

############################## Community Composition ###########################

raw = read_xlsx("H:/JenaSP6_2021/id034_nematodes_240-samples_JenaExperiment_DeltaBEF_July2021_Marcel-Ciobanu__240 samples Jena exp SP6 Nematodes Amyntas2021FINAL.xlsx",
                sheet = "Synthesis",
                range = "BP5:EB245")

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
View(filter(data.2, block == "B1" & plot == 12))
data.2[240,1:4] = as.list(c("B1A12D3","B1","12","3"))

# arrange again
data.2 = data.2 %>% arrange(Sample)

# check again
table(data.2$block,data.2$plot)
View(filter(data.2, block == "B1" & plot == 12))

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

# check, all good
data.2$Sample == abun$Sample 
data.2$Sample == soil$Sample 

# multiply percent composition with the number of nematodes extracted from each soil sample 
# data.3 = data.2
# data.3[,5:68] = data.2[,5:68] * abun$`Number of Nematodes`/100
data.3 = data.2 %>% mutate(across(where(is.numeric), # for all numeric columns
                           # we multiply by total abundance and divide by 100
                                  ~ .*abun$`Number of Nematodes`/100), 
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

# long format for Jexis
data.5 = data.4 %>% select(-(2:4)) %>% pivot_longer(where(is.numeric),
                                                    names_to = "Taxon",
                                                    values_to = "Density")
# check, OK
table(data.5$Taxon)

data.5[,"Density"] = round(data.5[,"Density"], 7)

write.csv(data.5, file = "Nematode_Community_Composition_dBEF_2021.csv",
          quote = F,
          row.names = F,
          sep = ",")




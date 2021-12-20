library(readxl)
library(tidyverse)
library(stringr)

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

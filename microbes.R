library(readxl)
library(tidyverse)
library(stringr)

# read the raw data 
raw = read_xlsx("H:/JenaSP6_2021/final_RMS_JE SP6_2021_Alfred.xlsx",
                sheet = "Tabelle1")

data.1 = raw %>% arrange( # arranges samples by block, then plot, then treatment
                          # date of measurement places remeasured after original     
                          `sample name 1`,
                          `sample name 2`,
                          `sample name 3`,
                          `date of measurement`)

data.2 = data.1 %>% filter(# removes original measurement of samples that needed remeasuring
                         remarks != "original measurement; will be remeasured")

nrow(data.2) # now we have our 240 samples (no missing values :D) 

table(data.2$`sample name 1`,  #oops I spoke too soon! plots 18 & 20 from block 1 have one extra sample
      data.2$`sample name 2`)

# plots 18 & 20 from block 1 have one extra sample, while the same plots from 
# block 2 are missing a sample! for "18" we are lucky; treatment 3 of block 1 
# needed remeasuring and was remeasured the following day, so those two doubles 
# marked "original measurement" and "remeasurement" are "married"; they both 
# belong to B1A18. The odd one out can be safely assumed to be B2A18T3
# No such luck for "20" one sample 1|20|2 is actually 2|20|2 but we don't know which
# UPDATE: After checking the lab sheets, it is clear that "DIETER 7433" is the missing 
# B2A20T2. It was written as A20T2 and transferred to the spreadsheet as 1|20|2
# The lab sheets also confirm that "KLAUS 8909" is in fact B2A18T3
View(filter(data.1, `sample name 2` == 18))
View(filter(data.1, `sample name 2` == 20))


# this does not do the trick
# raw.3$`sample name 1`[raw.3$`sample name 1` == 1 &
#                       raw.3$`sample name 2` == 18 & 
#                       raw.3$`sample name 3` == 3 &
#                       raw.3$`date of measurement` == "2021-06-28"] = 2

# this works, "KLAUS 8909" is B2A18T3
data.2[48,4] = 2
# "DIETER 7433" is B2A20T2
data.2[55,4] = 2

# rearrange again
data.2 = data.2 %>% arrange(`sample name 1`,
                            `sample name 2`,
                            `sample name 3`)

# all normal
View(filter(data.2, `sample name 2` == 18))
View(filter(data.2, `sample name 2` == 20))
table(data.2$`sample name 1`, 
      data.2$`sample name 2`)

# make plot numbers double digit
data.2$`sample name 2` = sprintf("%02d", data.2$`sample name 2`)

# combine block number, plot number and treatment number to get sample ID
data.3 = data.2 %>% add_column(plot = str_c("B", data.2$`sample name 1`,
                                            "A", data.2$`sample name 2`,
                                            "D", data.2$`sample name 3`),
                               .before = "sample name 1")

data.4 = data.3[,c(4:8,10:12)] %>% add_column(Year = 2021)

names(data.4) = c("plot",
                  "Block",
                  "Plot",
                  "Treatment",
                  "basal_respiration",
                  "soil_microbial_biomass_C",
                  "respiratory_quotient",
                  "soil_water_content",
                  "year")

data.5 = data.4[,-(2:4)]

data.5[,2:5] = round(data.5[,2:5], 7)

write.csv(data.5, file = "Soil_microbial_respiration_and_biomass_dBEF_2021_UPDATE.csv",
          quote = F,
          row.names = F,
          sep = ",")

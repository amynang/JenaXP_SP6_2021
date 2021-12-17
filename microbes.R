library(readxl)
library(tidyverse)
library(stringr)

raw.1 = read_xlsx("H:/JenaSP6_2021/final_RMS_JE SP6_2021_Alfred.xlsx",
                sheet = "Tabelle1")

raw.2 = raw.1 %>% arrange( # arranges samples by block, then plot, then treatment
                           # date of measurement places remeasured after oiriginal     
                          `sample name 1`,
                          `sample name 2`,
                          `sample name 3`,
                          `date of measurement`)

raw.3 = raw.2 %>% filter(# removes original measurement of samples that needed remeasuring
                         remarks != "original measurement; will be remeasured")

nrow(raw.3) # now we have our 240 samples (no missing values :D) 

table(raw.3$`sample name 1`,  #oops I spoke too soon! plots 18 & 20 from block 1 have one extra sample
      raw.3$`sample name 2`)

# plots 18 & 20 from block 1 have one extra sample, while the same plots from 
# block 2 are missing a sample! for "18" we are lucky; treatment 3 of block 1 
# needed remeasuring and was remeasured the following day, so those two doubles 
# marked "original measurement" and "remeasurement" are "married"; they both 
# belong to B1A18. The odd one out can be safely assumed to be B2A18T3
# No such luck for "20" one sample 1|20|2 is actually 2|20|2 but we don't know which
View(filter(raw.2, `sample name 2` == 18))
View(filter(raw.2, `sample name 2` == 20))


# this does not do the trick
# raw.3$`sample name 1`[raw.3$`sample name 1` == 1 &
#                       raw.3$`sample name 2` == 18 & 
#                       raw.3$`sample name 3` == 3 &
#                       raw.3$`date of measurement` == "2021-06-28"] = 2

# this works, B1A18T3 becomes B2A18T3
raw.3[48,4] = 2

# rearrange again
raw.3 = raw.3 %>% arrange(`sample name 1`,
                          `sample name 2`,
                          `sample name 3`)

View(filter(raw.3, `sample name 2` == 18))

raw.3$`sample name 2` = sprintf("%02d", raw.3$`sample name 2`)

raw.4 = raw.3 %>% add_column(plot = str_c("B", raw.3$`sample name 1`,
                                          "A", raw.3$`sample name 2`,
                                          "D", raw.3$`sample name 3`),
                             .before = "sample name 1")



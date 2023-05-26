library(tidyverse)
library(httr)
library(jsonlite)
library(XML)
library(rBExIS)
bexis.options(base_url = "https://jexis.uni-jena.de")

main.plot = bexis.get.dataset_by(id = 90)

roots = read.csv("H:\\JenaSP6_2021\\Root biomass data in dBEF and main experiment in 2021.csv",
                 sep=";")
table(roots$plot)
table(roots$treatment, roots$plot)

root.mass = as_tibble(roots) %>% 
  select(-c(year,month,coarse_root.bm,fine_root.bm)) %>% 
  filter(depth.min == 0.00) %>% 
  select(-c(depth.min,depth.max)) %>% 
  rename(Plot = plot,
         Treatment = treatment) %>% 
  mutate(.keep = "unused",
         Treatment = str_replace(Treatment, "D", "Treatment"),
         root.mass.mg = root.bm * 1e3)

################################## 1000 ########################################

thou = read.csv("fluxes/fluxes1000_imputed_dir1000.csv") %>% 
  mutate(.before = Plot,
         Block = str_split(.$Plot, "A", simplify = T)[,1]) %>% 
  # we add a sowndiv column whose elements are filled from main.plot
  # based on plot matching
  mutate(.after = Plot,
         Plant.Richness = main.plot$sowndiv[match(.$Plot, main.plot$plotcode)])

thou = thou %>% mutate(herb.press = herb.flux / root.mass$root.mass.mg[match(paste(.$Plot, 
                                                                                   .$Treatment),
                                                                             paste(root.mass$Plot, 
                                                                                   root.mass$Treatment))],
                       control = thou$down / thou$up)

write.csv(thou, "fluxes/fluxes1000_imputed_dir1000.csv",
          row.names = F)

################################## 100 #########################################

thou = read.csv("fluxes/fluxes1000_imputed_dir100.csv") %>% 
  mutate(.before = Plot,
         Block = str_split(.$Plot, "A", simplify = T)[,1]) %>% 
  # we add a sowndiv column whose elements are filled from main.plot
  # based on plot matching
  mutate(.after = Plot,
         Plant.Richness = main.plot$sowndiv[match(.$Plot, main.plot$plotcode)])

thou = thou %>% mutate(herb.press = herb.flux / root.mass$root.mass.mg[match(paste(.$Plot, 
                                                                                   .$Treatment),
                                                                             paste(root.mass$Plot, 
                                                                                   root.mass$Treatment))],
                       control = thou$down / thou$up)

write.csv(thou, "fluxes/fluxes1000_imputed_dir100.csv",
          row.names = F)

################################### 10 #########################################

thou = read.csv("fluxes/fluxes1000_imputed_dir10.csv") %>% 
  mutate(.before = Plot,
         Block = str_split(.$Plot, "A", simplify = T)[,1]) %>% 
  # we add a sowndiv column whose elements are filled from main.plot
  # based on plot matching
  mutate(.after = Plot,
         Plant.Richness = main.plot$sowndiv[match(.$Plot, main.plot$plotcode)])

thou = thou %>% mutate(herb.press = herb.flux / root.mass$root.mass.mg[match(paste(.$Plot, 
                                                                                   .$Treatment),
                                                                             paste(root.mass$Plot, 
                                                                                   root.mass$Treatment))],
                       control = thou$down / thou$up)

write.csv(thou, "fluxes/fluxes1000_imputed_dir10.csv",
          row.names = F)


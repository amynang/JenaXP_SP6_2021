source("wrangling/nematodes.R")
source("wrangling/mesofauna.R")
source("wrangling/macrofauna.R")

micro.meso.macro = full_join(micro, meso, by = join_by(Plot,Treatment)) %>% 
  full_join(., macro, by = join_by(Plot,Treatment))

mean.masses = mean.micro.masses %>% 
  full_join(mean.meso.masses %>% select(-N)) %>% 
  full_join(., mean.macro.masses %>% select(-N)) %>% 
  mutate(taxon = str_replace(.$taxon," ", "."))

write.csv(micro.meso.macro, "soil_fauna_abun_msq.csv",
          row.names = F)

write.csv(mean.masses, "mean_sd_masses.csv",
          row.names = F)
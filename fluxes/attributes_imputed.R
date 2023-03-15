library(tidyverse)
library(mice)
source("wrangling/functions.R")

micro.meso.macro = read.csv("soil_fauna_abun_msq.csv")

mean.masses = read.csv("mean_sd_masses.csv")

# We are going to impute missing values for mite and collembola taxa
set.seed(404)
imp = mice(micro.meso.macro,
           m = 100) # number of multiple iputations
# every subplot now has 100 versions
# they reflect uncertainty of imputed values
imputed = complete(imp, 
                   action = "long") 

att = as_tibble(imputed) %>% 
  mutate(.before = Plot,
         ID = paste0(Plot,Treatment)) %>% 
  pivot_longer(6:31,
               names_to = "taxon",
               values_to = "abundance") %>% 
  mutate(#add mean and sd of bodymass for each taxon
         MeanMass.mg = mean.masses$MeanMass.mg[match(.$taxon, mean.masses$taxon)],
         StDMass.mg = mean.masses$StDMass.mg[match(.$taxon, mean.masses$taxon)]) %>%
  # remove taxa absent from each subplot
  filter(abundance>0) %>% 
  # finally, create a list: each element is a subplot, with 100 imputations (.imp)
  split(., with(.,ID))


########################## parallelised  #######################################
library(foreach)
# https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
parallel::detectCores()
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(24, type = "PSOCK")
#check cluster definition (optional)
print(my.cluster)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()
#how many workers are available? (optional)
foreach::getDoParWorkers()

# this will take a while...
# re-check everything!!!!!!!!!!!
set.seed(404)
att = foreach(i = 1:length(att), 
              #.combine = "c",
              .packages = c("tidyverse")) %dopar% {
                att[[i]] = att[[i]] %>%   
                  # biomass and population level metabolism (J/h)
                  # the function will take as arguments the nth element of vectors Abundance, 
                  # MeanMass.mg, StDMass.mg and return a list of vectors
                  # every vector contains sampled bodymasses for the nth taxon
                  mutate(random.individuals = pmap(list(ceiling(att[[i]]$abundance), # how many draws
                                                        att[[i]]$MeanMass.mg,        # mean
                                                        att[[i]]$StDMass.mg,         # sd
                                                        .99),                        # quantile
                                                   rlnormtrunc.intuitive)) %>% 
                  rowwise() %>% 
                  # now we sum the mass of all individuals of a taxon
                  mutate(Biomass.mg = sum(random.individuals)) %>% 
                  # and we also calculate metabolic losses of every individiual and sum them 
                  # to get population level losses
                  mutate(Pop.met.rate.J_h = case_when(# based on Ehnes 2011 10.1111/j.1461-0248.2011.01660.x
                    # The general relationship (linear model)
                    TRUE ~ sum(exp(23.055335 + .6950710*log(random.individuals) - .6864200*(1/(8.62*1e-5*(20+273.15))))),
                    # group specific coefficients (phylogenetic model)
                    taxon ==      "Araneae" ~ sum(exp(24.581475 + .5652537*log(random.individuals) - .7093476*(1/(8.62*1e-5*(20+273.15))))),
                    taxon %in% c("Coleoptera",
                                 "Hemiptera",
                                 "Thysanoptera",
                                 "Diptera.larvae") ~ sum(exp(21.972050 + .7588950*log(random.individuals) - .6574038*(1/(8.62*1e-5*(20+273.15))))),
                    taxon ==      "Isopoda" ~ sum(exp(23.168652 + .5544768*log(random.individuals) - .6867293*(1/(8.62*1e-5*(20+273.15))))),
                    taxon ==    "Chilopoda" ~ sum(exp(28.252911 + .5580991*log(random.individuals) - .8030069*(1/(8.62*1e-5*(20+273.15))))),
                    taxon ==    "Diplopoda" ~ sum(exp(22.347024 + .5713411*log(random.individuals) - .6700449*(1/(8.62*1e-5*(20+273.15))))),
                    taxon ==    "Oribatida" ~ sum(exp(22.022770 + .6793706*log(random.individuals) - .7060855*(1/(8.62*1e-5*(20+273.15))))),
                    taxon ==  "Prostigmata" ~ sum(exp(10.281495 + .6599399*log(random.individuals) - .4125318*(1/(8.62*1e-5*(20+273.15))))),
                    taxon == "Mesostigmata" ~ sum(exp(9.6740230 + .6904864*log(random.individuals) - .3792541*(1/(8.62*1e-5*(20+273.15))))))) %>% 
                  select(-random.individuals) %>% 
                  ungroup()
                
              }
parallel::stopCluster(cl = my.cluster)
beepr::beep(9)

# we split the imputations of each subplot to elements of a list
# we were already inside a list so... listception
tta = vector(mode = "list", 240)
for (i in 1:length(tta)) {
  tta[[i]] = att[[i]] %>% split(., with(.,.imp))
}

# now in each of the sublist elements we add information about the basal resources
for (i in 1:240) {
  for (j in 1:100) {
    tta[[i]][[j]] = tta[[i]][[j]] %>% 
      add_row(#.before = 1,
        # add basal resources at the top of the dataframes (check that order matches that in mat)
        taxon = c("roots","detritus","bacteria","fungi"), 
        Plot = first(.$Plot),
        Treatment = first(.$Treatment),
        Biomass.mg = 1, # makes relative biomass matter only among animal resources (only used for standardization of omnivores)
        Pop.met.rate.J_h = 0) %>% 
      mutate(# resource based assimilation efficiency, from Lang et al. 2017
        temp.kT = ((273.15+20)-293.15)/(8.62*1e-5*(273.15+20)*293.15), # explain
        efficiency = case_when(taxon == "detritus" ~ exp(-1.670)*exp(.164*temp.kT) / (1 + exp(-1.670)*exp(.164*temp.kT)),
                               taxon ==   "roots" ~ exp(0.179) *exp(.164*temp.kT) / (1 + exp(0.179) *exp(.164*temp.kT)),
                               # everything else (including fungi & bacteria???) gets animal prey efficiency
                               TRUE ~ exp(2.266) *exp(.164*temp.kT) / (1 + exp(2.266) *exp(.164*temp.kT)))) %>% 
      select(-temp.kT)
  }
}

att = tta
rm(tta)

saveRDS(att, "attributes_imputed.RData")
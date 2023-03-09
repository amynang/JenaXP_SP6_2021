library(tidyverse)
library(cubature)
source("wrangling/functions.R")

micro.meso.macro = full_join(micro, meso, by = join_by(Plot,Treatment)) %>% 
                   full_join(., macro, by = join_by(Plot,Treatment))
  


mean.masses = mean.micro.masses %>% 
  full_join(mean.meso.masses %>% select(-N)) %>% 
  full_join(., mean.macro.masses %>% select(-N)) %>% 
  mutate(taxon = str_replace(.$taxon," ", "."))

nematodes = names(micro)[-(1:2)]
meso      =  names(meso)[-(1:2)]  
macro     = names(macro)[-(1:2)]

mat = matrix(NA,
             nrow = length(names(micro.meso.macro))+2,
             ncol = length(names(micro.meso.macro))+2,
             dimnames = list(c("roots","detritus","bacteria","fungi",mean.masses$taxon),
                             c("roots","detritus","bacteria","fungi",mean.masses$taxon)))


mat["bacteria",    "Bacterivore.nematodes"] = 1
mat["fungi",    "Fungivore.nematodes"] = 1
mat["roots",    "Herbivore.nematodes"] = 1
mat[c("roots","bacteria","fungi", nematodes),    "Omnivore.nematodes"] = c(.25,.25,.25,rep(.25/length(nematodes),
                                                                                           length(nematodes)))
mat[nematodes,    "Predator.nematodes"] = 1 # what about the loop?

mat[c("detritus","fungi"),    "Protura"] = c(.1,.9)
mat[c("roots","detritus","fungi"),    "Pauropoda"] = 1
mat[c("roots","detritus",nematodes,meso),    "Symphyla"] = c(.25,.25,rep(.25/length(nematodes),
                                                                         length(nematodes)),
                                                                     rep(.25/length(meso),
                                                                     length(meso)))

mat[c("roots","bacteria","fungi")             ,    "Edaphic.Entomobryomorpha"] = 1
mat[c("roots","bacteria","fungi")             ,    "Edaphic.Neelipleona"     ] = 1
mat[c("bacteria","fungi")                     ,    "Edaphic.Poduromorpha"    ] = 1
mat[c("roots","detritus","bacteria","fungi")  ,    "Epigeic.Symphypleona"    ] = 1
mat[c("roots","detritus","bacteria","fungi")  ,    "Epigeic.Entomobryomorpha"] = 1
mat[c("bacteria","fungi",nematodes)           ,    "Epigeic.Poduromorpha"    ] = c(1/3,1/3,rep(1/(3*length(nematodes)),
                                                                                               length(nematodes)))

mat[c("detritus","bacteria","fungi", nematodes),          "Oribatida"] = c(.25,.25,.25,rep(.25/length(nematodes),
                                                                                           length(nematodes)))

mat[c("roots","detritus","fungi", nematodes, meso), "Prostigmata"] = c(.2,.2,.2,rep(.2/length(nematodes),
                                                                                     length(nematodes)),
                                                                                 rep(.2/length(meso),
                                                                                     length(meso)))
mat[c(nematodes, meso), "Mesostigmata"] = 1
  
mat[c("roots","fungi"),    "Thysanoptera"] = 1
mat["roots",    "Hemiptera"] = 1
mat[c("roots","detritus","bacteria","fungi") ,    "Gastropoda"] = c(.1,.3,.3,.3)
mat[c("detritus","bacteria","fungi")         ,       "Isopoda"] = 1
mat[c("detritus","fungi","bacteria")         ,     "Diplopoda"] = c(.75,.25/2,.25/2)
mat[c("roots","detritus","fungi", nematodes, meso, macro) ,"Diptera.larvae"] = c(.1,.18,.18,
                                                                                 rep(.18/length(nematodes), length(nematodes)),
                                                                                 rep(.18/length(meso), length(meso)),
                                                                                 rep(.18/length(macro), length(macro)))
mat[c(meso, macro), "Chilopoda"] = 1
mat[c("Epigeic.Symphypleona",    
      "Epigeic.Entomobryomorpha",
      "Epigeic.Poduromorpha",
      "Protura","Pauropoda","Symphyla",
      macro[!(macro %in% c("Chilopoda"))])    , "Araneae"] = 1
mat[c("roots","detritus","fungi", 
      meso, 
      macro[!(macro %in% c("Araneae","Chilopoda"))])    , "Coleoptera"] = 1


mat[is.na(mat)] = 0
mat = vegan::decostand(mat,"total", 2)

colSums(mat)


View(mat[c(nematodes,meso,macro),
         c(nematodes,meso,macro)])

colSums(mat[c(nematodes,meso,macro),
            c(nematodes,meso,macro)])

# If I have 1 bodymass distribution per taxon across all locations I can do this once
# Repeating 135 times is... crazy

# https://rpsychologist.com/calculating-the-overlap-of-two-normal-distributions-using-monte-carlo-integration
int_f <- function(x, mu1, mu2, sd1, sd2) {
  f1 <- dlnormtrunc.intuitive(x, m=mu1, s=sd1, p=1)
  f2 <- dlnormtrunc.intuitive(x, m=mu2, s=sd2, p=1)
  pmin(f1, f2)
}

#for (i in 1:length(att)) { 
  # number of taxa
  n = nrow(mean.masses)
  # this is not wrong but probably not the simplest way?
  body.mat = replicate(n, mean.masses$MeanMass.mg)
  bodymat = body.mat[1:n,]
  bodymat[,] = 0
  
  # which cells in the interaction matrix are non zero
  ind = which(mat[c(nematodes,meso,macro),
                  c(nematodes,meso,macro)] != 0, arr.ind = T)
  # we will use this to standardize animal predation in omnivores
  std = colSums(mat[c(nematodes,meso,macro),
                    c(nematodes,meso,macro)])
  
  # vector of bodymasses
  meanmass = mean.masses$MeanMass.mg
  sdmass = mean.masses$StDMass.mg
  
  for (j in 1:nrow(ind)) { # for every predator-prey pair
    # we calculate prey suitability as the integral of the overlap of that prey's 
    # bodymass distribution and the optimal prey distribution for that predator
    # assuming OPPMR = 10^.6 (optimal prey ~4 times smaller than predator cf Brose 2006)
    overlap = cubintegrate(int_f, 0, Inf,
                           mu1=meanmass[ind[j,][2]]/10^.6, # predator/10^.6
                             sd1=sdmass[ind[j,][2]]/10^.6,
                           mu2=meanmass[ind[j,][1]],       # prey
                             sd2=sdmass[ind[j,][1]])$integral
    bodymat[ind[j,][1],ind[j,][2]] <- overlap
  }
 
  checkthat = mat[c(nematodes,meso,macro),
                  c(nematodes,meso,macro)]*bodymat
  # first standardization: animal preferences sum to 1
  checkthat = vegan::decostand(checkthat,"total", 2)
  # second, we have them sum to the complement of whatever else they eat
  # so if we expect that an omnivore eats 50% plants and 50% animals, animal preferences sum to .5
  # we do this by multiplying every row in the matrix with the std vector
  checkthat = checkthat*rep(std, each=nrow(checkthat))
  mat[c(nematodes,meso,macro),
      c(nematodes,meso,macro)] = checkthat
  
colSums(mat)

























att = micro.meso.macro %>% 
  mutate(.before = everything(),
  ID = paste0(Plot,Treatment)) %>% 
  pivot_longer(4:29,
               names_to = "taxon",
               values_to = "abundance") %>% 
  mutate(MeanMass.mg = mean.masses$MeanMass.mg[match(.$taxon, mean.masses$taxon)],
          StDMass.mg = mean.masses$StDMass.mg[match(.$taxon, mean.masses$taxon)]) %>%
  filter(abundance>0) %>% 
  split(., with(.,ID))



# this will take a while...
for (i in 1:240) {
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
    mutate(Pop.met.rate = case_when(# based on Ehnes 2011 10.1111/j.1461-0248.2011.01660.x
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
      taxon == "Mesostigmata" ~ sum(exp(9.6740230 + .6904864*log(random.individuals) - .3792541*(1/(8.62*1e-5*(20+273.15)))))))
  
  cat('\014')
  #cat(paste0(round((m/1600)*100), '%'))
  cat(paste0(i, '/', length(att)))
  #Sys.sleep(.05)
  if (i == length(att)) cat('- Done!')
}

beepr::beep(9)



library(tidyverse)
library(cubature)
source("wrangling/functions.R")

potapov = read.xlsx("H:\\Literature\\brv12857-sup-0003-tables2.xlsx",
                    sheet = "GroupList") 

micro.meso.macro = read.csv("soil_fauna_abun_msq.csv")

mean.masses = read.csv("mean_sd_masses.csv")


# traits that modify feeding interactions
feeding.traits = mean.masses %>% 
  mutate(Abbreviation = c("Ne-B","Ne-F","Ne-H","Ne-O","Ne-P",
                          "Cla-D","Cla-D","Cla-D","Cla-A","Cla-A","Cla-A",
                          "Me","Ori","Pau","Prost","Protu","Sym",
                          "Ar","Chi","Cpt","Dpod","Ga","He","Iso","Thy","Dipt")) %>% 
  mutate(           Agility = potapov$Agility[match(.$Abbreviation, potapov$Abbreviation)],
                    PhysicalProtection = potapov$PhysicalProtection[match(.$Abbreviation, potapov$Abbreviation)],
                    Metabolites = potapov$Metabolites[match(.$Abbreviation, potapov$Abbreviation)],
                    above = potapov$above[match(.$Abbreviation, potapov$Abbreviation)],
                    epi = potapov$epi[match(.$Abbreviation, potapov$Abbreviation)],
                    hemi = potapov$hemi[match(.$Abbreviation, potapov$Abbreviation)],
                    eu = potapov$eu[match(.$Abbreviation, potapov$Abbreviation)])

# vertical stratification similarity
ver = feeding.traits %>% 
      select(above,epi,hemi,eu) %>% 
      vegan::vegdist(method = "bray") %>% 
      as.matrix() 
vertical = 1- ver


nematodes = names(micro.meso.macro)[3:7]
meso      =  names(micro.meso.macro)[8:19]  
macro     = names(micro.meso.macro)[20:28]


########################## Trophic interaction matrix ##########################
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
mat[c(meso, macro[!(macro %in% c("Chilopoda"))]), "Araneae"] = 1
mat[c("roots","detritus","fungi", 
      meso, 
      macro[!(macro %in% c("Araneae","Chilopoda"))])    , "Coleoptera"] = 1


mat[is.na(mat)] = 0
mat = vegan::decostand(mat,"total", 2)

colSums(mat)


# https://rpsychologist.com/calculating-the-overlap-of-two-normal-distributions-using-monte-carlo-integration
int_f <- function(x, mu1, mu2, sd1, sd2) {
  f1 <- dlnormtrunc.intuitive(x, m=mu1, s=sd1, p=1)
  f2 <- dlnormtrunc.intuitive(x, m=mu2, s=sd2, p=1)
  pmin(f1, f2)
}

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

# now we multiply by three vectors that modify this relationship based on prey
# agility, physical protection or metabolites 
# and finaly, a matrix of vertical stratification similarity
checkthat = checkthat*
            feeding.traits$Agility*
            feeding.traits$PhysicalProtection*
            feeding.traits$Metabolites*
            vertical

# first standardization: animal preferences sum to 1
checkthat = vegan::decostand(checkthat,"total", 2)

# second, we have them sum to the complement of whatever else they eat
# so if we expect that an omnivore eats 50% plants and 50% animals, animal preferences sum to .5
# we do this by multiplying every row in the matrix with the std vector
checkthat = checkthat*rep(std, each=nrow(checkthat))
mat[c(nematodes,meso,macro),
    c(nematodes,meso,macro)] = checkthat
  
colSums(mat)





######################### plot specific attributes #############################
imputed = micro.meso.macro %>% 
  pivot_longer(3:28,
               names_to = "taxon",
               values_to = "abundance") %>% 
  group_by(taxon) %>% 
  summarise(mean.abund = median(abundance, na.rm = T),
            sd.abund = sd(abundance, na.rm = T))

att = micro.meso.macro %>% 
  mutate(.before = everything(),
  ID = paste0(Plot,Treatment)) %>% 
  pivot_longer(4:29,
               names_to = "taxon",
               values_to = "abundance") %>% 
  mutate(MeanMass.mg = mean.masses$MeanMass.mg[match(.$taxon, mean.masses$taxon)],
          StDMass.mg = mean.masses$StDMass.mg[match(.$taxon, mean.masses$taxon)]) %>%
  filter(abundance>0 | is.na(abundance)) %>% 
  split(., with(.,ID))

# for now, only foodwebs without missing taxa
att = purrr::discard(att, ~any(is.na(.x)))


########################## parallelised  #######################################
library(foreach)
# https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
parallel::detectCores()
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
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
    ungroup() %>% 
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
parallel::stopCluster(cl = my.cluster)
beepr::beep(9)


# a list for feeding preference matrices
mat.prefs = vector(mode = "list",
                   length=length(att))

web = vector(mode = "list", 
             length=length(att))
for (i in 1:length(att)) {
  web[[i]] = mat[att[[i]]$taxon,
                 att[[i]]$taxon]
}

for (i in 1:length(att)) {
  ####################   Omnivores' Balanced Diet Plan   #########################
  # add biomass values in the matrix to 'manually' define the preferences
  # first create a matrix with species biomasses
  mat.bioms = replicate(length(att[[i]]$Biomass.mg), att[[i]]$Biomass.mg)
  # mat.prefs contains preference of predators based on their prey biomasses
  mat.prefs[[i]] = web[[i]] * mat.bioms
  
  basals = which(att[[i]]$taxon %in% c("roots","detritus","bacteria","fungi"))
  animals = which(!(att[[i]]$taxon %in% c("roots","detritus","bacteria","fungi")))
  #omnivores that feed on basals and animals
  omnivores = which(colSums(mat.prefs[[i]][basals,])>0 &
                      colSums(mat.prefs[[i]][animals,])>0)
  # normalize preferences of omnivores over animals to 1: (sum of prey prefs for omn is 1)
  mat.prefs[[i]][animals, omnivores] = mat.prefs[[i]][animals, omnivores, drop=FALSE] %*%
    diag(1/colSums(as.matrix(mat.prefs[[i]][animals, omnivores, drop=FALSE])),
         length(omnivores),length(omnivores)) #diag(4)!=diag(4,1,1) important if single omnivore
  
  # now we additionally make this sum to the complement of whatever else they eat
  std = colSums(web[[i]][animals, 
                         omnivores])
  mat.prefs[[i]][animals, 
                 omnivores] = mat.prefs[[i]][animals, 
                                             omnivores]*rep(std, 
                                                            each=nrow(mat.prefs[[i]][animals, 
                                                                                     omnivores]))
  
  mat.prefs[[i]] = vegan::decostand(mat.prefs[[i]], "total", MARGIN = 2)
  
}


library(fluxweb)

# you need the original mat.prefs list!!!
thousand = vector(mode = "list",length=1000)
set.seed(404)
for (k in 1:1000) { 
  # a list for flux matrices
  fluxes = vector(mode = "list",length=length(att))
  
  allmetrics = data.frame(Plot = character(length(att)),
                          Treatment = character(length(att)),
                          tot.flux = numeric(length(att)),   # total energy flux
                          pred.flux = numeric(length(att)),  # predation flux
                          herb.flux = numeric(length(att)),  # herbivory flux
                          detr.flux = numeric(length(att)),
                          secon.decomp.flux = numeric(length(att)),
                          top.down = numeric(length(att)),   # from herbivores per unit herbivore biomass
                          bot.up = numeric(length(att)),     # to herbivores per unit herbivore biomass
                          herb.press = numeric(length(att))) # to herbivores per unit plant biomass
  
  
  for (i in 1:length(att)) {
    
    ################################# Uncertainty ##################################
    # Here we take each consumer in the foodweb and replace its fixed preferences 
    # with a random sample from a dirichlet distribution whose component probabilities 
    # are given by the vector of the original preferences. The vector is multiplied 
    # by a scalar that modifies the shape of the distribution (larger = less uncertainty)
    # Across several iterations our expectations regarding what consumers feed on 
    # are met, on average. But in each iteration consumer preferences deviate somewhat
    # from those expected based on intrinsic preference and/or relative availability.
    for (j in 1: nrow(mat.prefs[[i]])) { 
      mat.prefs[[i]][,j] = LaplacesDemon::rdirichlet(1, mat.prefs[[i]][,j]*100)
    }
    mat.prefs[[i]][is.nan(mat.prefs[[i]])] = 0 #removes NaNs from basal node "preferences"
    ################################################################################
    
    # down-weighing cannibalism
    diag(mat.prefs[[i]]) = diag(mat.prefs[[i]])*0.01
    
    fluxes[[i]] <- fluxing(mat.prefs[[i]],
                               att[[i]]$Biomass.mg, 
                               att[[i]]$Pop.met.rate.J_h,
                               att[[i]]$efficiency,
                               bioms.prefs = F,
                               bioms.losses = F,
                               ef.level = "prey")
    
    animals = which(!(att[[i]]$taxon %in% c("roots","detritus","bacteria","fungi")))
     plants = which(att[[i]]$taxon == "roots")
   detritus = which(att[[i]]$taxon == "detritus")
    microbs = which(att[[i]]$taxon %in% c("bacteria","fungi"))
    
    herbivores = which(colSums(fluxes[[i]][c(animals,
                                             detritus,
                                             microbs),,drop = FALSE]) == 0 &
                         colSums(fluxes[[i]][plants,,drop = FALSE])>0)
    
    predators = which(colSums(fluxes[[i]][c(plants,
                                            detritus,
                                            microbs),,drop = FALSE]) == 0 &
                        colSums(fluxes[[i]][animals,]) > 0)
    
    allmetrics[i,]$Plot = unique(att[[i]]$Plot)
    allmetrics[i,]$Treatment = unique(att[[i]]$Treatment)
    allmetrics[i,]$tot.flux = sum(fluxes[[i]])                         # total energy flux              
    allmetrics[i,]$pred.flux = sum(fluxes[[i]][animals, ])             # predation flux
    allmetrics[i,]$herb.flux = sum(fluxes[[i]][plants, ])              # herbivory flux
    allmetrics[i,]$detr.flux = sum(fluxes[[i]][detritus, ])            # detritivory flux
    allmetrics[i,]$secon.decomp.flux = sum(fluxes[[i]][microbs, ])             # secondary decomposers flux
    
    allmetrics[i,]$top.down = sum(fluxes[[i]][herbivores, predators]) # from herbivores per unit herbivore biomass
    allmetrics[i,]$bot.up = sum(fluxes[[i]][plants, herbivores])     # to herbivores per unit herbivore biomass
    # allmetrics[i,]$herb.press = sum(fluxes[[i]][plants, herbivores])/  # to herbivores per unit plant biomass 
    #   sum(unique(je.att[[i]]$plant.biomass))     
    
  }
  
  thousand[[k]] = allmetrics
  
  ############################## Show loop progress ##############################
  cat('\014')
  cat(paste0(round((k/1000) * 100), '% completed'))
  Sys.sleep(.05)
  if (k == 1000) cat(': Done')
  ################################################################################ 
}  
    
    
    
thou = do.call(rbind, thousand) %>% 
  mutate(.after = Plot,
         Plant.Richness = main.plot$sowndiv[match(.$Plot, main.plot$plotcode)])

total = thou %>% 
  group_by(Plot, Plant.Richness, Treatment) %>% 
  summarise(tot.flux.m = mean(tot.flux),
            tot.flux.sd = sd(tot.flux)) %>% 
  ungroup() %>% 
  mutate(.before = Plot,
         Block = str_split(.$Plot, "A", simplify = T)[,1]) %>% 
  mutate(Plant.Richness = (log2(Plant.Richness) - mean(log2(Plant.Richness)))/sd(log2(Plant.Richness)))

total$Treatment = factor(total$Treatment, levels = c("Treatment3","Treatment2","Treatment1"))

library(brms)
m = brm(bf(tot.flux.m | mi(tot.flux.sd) ~ 1 
           + Plant.Richness 
           + Treatment 
           + Plant.Richness:Treatment
           + (1 + Treatment|Plot)
           ),
        #family = Gamma(link = "log"),
        chains = 3,
        iter = 2000,
        cores = 3,
        control = list(adapt_delta = 0.95),
        backend = "cmdstanr", 
        data = total)

summary(m)
pp_check(m, 
         ndraws = 100)
#plot(m)
plot(conditional_effects(m), ask = FALSE)

library(tidybayes)
library(modelr)

total %>%
  group_by(Block, 
           Plot, 
           Treatment) %>%
  data_grid(#ID = levels(properties$ID),
    Plant.Richness = seq_range(Plant.Richness, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_epred_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m, ndraws = 500, re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness, y = tot.flux.m, color = Treatment)) +
  geom_line(aes(y = (.epred), group = paste(Block, 
                                                 Treatment, 
                                                 .draw)), alpha = .25) +
  #geom_point(data = dd[complete.cases(dd),],
  #           position = position_jitter(width = .1)) +
  #stat_lineribbon(aes(y = log10(.epred)), .width = .95, alpha = .5) +
  #scale_fill_manual(values = "grey") +
  scale_color_manual(values = c("#F3BE61",
                                "#AA422E",
                                "#6C6F80")) +
                                           #scale_fill_manual(values = c("#F3BE61","#AA422E","#6C6F80")) +
  theme_classic()





ggplot(thou[thou$Plot =="B1A15" & thou$Treatment %in% c("Treatment1",
                                                        "Treatment2",
                                                        "Treatment3"), ], 
       aes(tot.flux, color = Treatment)) + 
  geom_line(stat="density") + 
  theme_classic() +
  theme(legend.position = "none")

library(ggridges)
library(viridis)
library(hrbrthemes)

ggplot(thou, aes(x = log(pred.flux), 
                 y = Plot, 
                 fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, 
                               rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", 
                     option = "C") +
  #labs(title = 'Temperatures in Lincoln NE in 2016') +
  #theme_ipsum() +
  #facet_grid(Plant.Richness~.) +
  facet_grid(vars(Plant.Richness),
             vars(Treatment)) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )  

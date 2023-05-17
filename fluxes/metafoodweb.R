library(tidyverse)
library(openxlsx)
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
mat[c("roots","detritus",nematodes,meso),    "Symphyla"] = c(1/3,1/3,rep(1/(3*length(c(nematodes,meso))),
                                                                         length(nematodes)),
                                                                     rep(1/(3*length(c(nematodes,meso))),
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

mat[c("roots","detritus","fungi", nematodes, meso), "Prostigmata"] = c(.25,.25,.25,rep(.25/length(c(nematodes,meso)),
                                                                                     length(nematodes)),
                                                                                 rep(.25/length(c(nematodes,meso)),
                                                                                     length(meso)))
mat[c(nematodes, meso), "Mesostigmata"] = 1
  
mat[c("roots","fungi"),    "Thysanoptera"] = 1
mat["roots",    "Hemiptera"] = 1
mat[c("roots","detritus","bacteria","fungi") ,    "Gastropoda"] = c(.1,.3,.3,.3)
mat[c("detritus","bacteria","fungi")         ,       "Isopoda"] = 1
mat[c("detritus","fungi","bacteria")         ,     "Diplopoda"] = c(.75,.25/2,.25/2)
mat[c("roots","detritus","fungi", nematodes, meso, macro) ,"Diptera.larvae"] = c(.1,.3,.3,
                                                                                 rep(.3/length(c(nematodes,meso,macro)), length(nematodes)),
                                                                                 rep(.3/length(c(nematodes,meso,macro)), length(meso)),
                                                                                 rep(.3/length(c(nematodes,meso,macro)), length(macro)))
mat[c(meso, macro), "Chilopoda"] = 1
mat[c(meso, macro[!(macro %in% c("Chilopoda"))]), "Araneae"] = 1
mat[c("roots","detritus","fungi", 
      meso, 
      macro[!(macro %in% c("Araneae","Chilopoda"))])    , "Coleoptera"] = c(.25,.25,.25,
                                                                            rep(.25/length(c(meso,macro[!(macro %in% c("Araneae","Chilopoda"))])), length(meso)),
                                                                            rep(.25/length(c(meso,macro[!(macro %in% c("Araneae","Chilopoda"))])), length(macro[!(macro %in% c("Araneae","Chilopoda"))])))

#mat[c(#"roots","detritus","fungi", 
#      meso, 
#      macro[!(macro %in% c("Araneae","Chilopoda"))])    , "Coleoptera"] = 1 #c(.1/3,.1/3,.1/3,
#                                                                            #  rep(.45/length(meso), length(meso)),
#                                                                            #  rep(.45/(length(macro)-2), length(macro)-2))

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

write.csv(mat, "fluxes/metafoodweb.csv")


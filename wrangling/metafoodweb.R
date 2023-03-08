micro.meso.macro = full_join(micro, meso, by = join_by(Plot,Treatment)) %>% 
                   full_join(., macro, by = join_by(Plot,Treatment))

nematodes = names(micro)[-(1:2)]
meso      =  names(meso)[-(1:2)]  
macro     = names(macro)[-(1:2)]

mat = matrix(NA,
             nrow = length(names(micro.meso.macro))+2,
             ncol = length(names(micro.meso.macro))+2,
             dimnames = list(c("roots","detritus","bacteria","fungi",names(micro.meso.macro)[3:28]),
                             c("roots","detritus","bacteria","fungi",names(micro.meso.macro)[3:28])))


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

mat[c("roots","detritus","fungi", nematodes, meso), "ProAstigmata"] = c(.2,.2,.2,rep(.2/length(nematodes),
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
colSums(mat)


View(mat[c(nematodes,meso,macro),
         c(nematodes,meso,macro)])

library(readxl)
library(openxlsx)
library(tidyverse)
library(googlesheets4)
library(stringr)



# col = read_xlsx("H:/JenaSP6_2021/Coll family.xlsx",
#                 sheet = "Sheet1",
#                 na = "")
# 
# df_new = col %>% select(-contains("length"))
# 
# unc = as.data.frame(df_new)
# unc$Subplot = gsub("Treatment", "D", unc$Subplot)
# unc$PlotSub = paste(unc$Plot, unc$Subplot, sep = "")
# unc$PlotSub = gsub(", bottom only", "b", unc$PlotSub)
# unc$PlotSub = gsub(", top only", "t", unc$PlotSub)
# row.names(unc) = unc[,15]
# unc=unc[,-c(1,2,3,15)]
# 
# hum = 1-vegan::vegdist(unc, method = "jaccard", binary=TRUE)
# as.matrix(hum)[4:6,1:3]

########################## Protura, Pauropoda, Symphyla ########################



################################### Collembola #################################

raw = read_xlsx("H:/JenaSP6_2021/Coll family.xlsx",
                sheet = "Tabelle1",
                na = "")

df_new = raw %>% 
  # drop bodylength columns and comment column
  select(-c(contains("length"),26)) %>% 
  arrange(Plot, Subplot) %>% 
  # remove unmatched half samples 
  slice(-c(135,141,194,246:248)) %>% 
  # the next few lines sum samples that were split into more than one vial
  mutate(.after = Plot,
         .keep = "unused",
         Subplot = str_split(.$Subplot, " ", simplify = T)[,1]) %>% 
  pivot_longer(3:14,
               names_to = "Species",
               values_to = "abun") %>% 
  group_by(Plot, Subplot, Species) %>% 
  summarise(abun = sum(abun)) %>% 
  pivot_wider(names_from = Species,
              values_from = abun)

# get 
gs4_deauth() #does this work?
in_core = read_sheet("https://docs.google.com/spreadsheets/d/1Zy3SbxS-n7lGFYuEcQ6X7sMgmP6lJMa7PGIc1n7C4xg/edit#gid=0",
                     sheet = "Counts") %>% 
  slice(-c(131,137,190,241:243))

comp = in_core %>% #rename(Treatment = Subplot) %>% 
  full_join(., df_new, by = join_by(Plot,Subplot)) %>% 
  select(1:3,9) %>% 
  #arrange(Plot,Treatment) %>% 
  rowwise() %>% 
  mutate(.after = `Collembola total`,
         d = `Collembola total` - Collembola,
         r = `Collembola total` / Collembola )



mmm = raw[ ,1:2] %>% arrange(Plot, Subplot)
mmmm = mmm[rep(seq_len(nrow(mmm)), each = 10), ]
#row.names(mmmm) <- NULL


# mmmm = str_split(col[1,]$`length (mm)...5`, pattern = ";", simplify = TRUE)
# names(mmmm) = rownames(unc)
# df <- data.frame(matrix(unlist(mmmm), nrow=6, byrow=TRUE),stringsAsFactors=FALSE)

coll = raw %>% rename_at(vars(starts_with('length')), funs(paste(colnames(df_new[,4:14]), .))) %>% 
  arrange(Plot, Subplot)
coll = as.data.frame(coll)

colll = coll %>% select(contains("length"))
colll = as.data.frame(colll)







for(i in colnames(df_new[4:14])) { 
  mmmm[,i] <- NA
}

#i=1
for (i in 1:11) {
  mmmmm = as.data.frame(str_split(colll[ ,i], 
                                  pattern = ";|:|; |: |. |, ", 
                                  simplify = T))
  n=dim(mmmmm)[2]
  if (n >= 10) {
    mmmmm = mmmmm[,1:10]
  } else {
    m=10-n
    nam = c(paste0("col",1:m))
    mmmmm[ , nam] <- NA
  }
  mmmmm = as.matrix(mmmmm)
  new_vector <- c(t(mmmmm))
  mmmm[,2+i] <- new_vector
}

mmmm = mmmm %>% mutate_all(function(x) gsub(" ","",x)) %>% mutate_all(function(x) gsub(",",".",x))
mmmm[,3:13] = mmmm[,3:13] %>% mutate_if(is.character,as.numeric)

# write.csv(mmmm, file = "H:/aaaarrrgh.csv")


##################################### Acari ####################################

raw.acari.1 = read.xlsx("H:\\JenaSP6_2021\\Jena treatment 1 2 3 densities.xlsx",
                        sheet = "Treatment 1")
raw.acari.2 = read.xlsx("H:\\JenaSP6_2021\\Jena treatment 1 2 3 densities.xlsx",
                        sheet = "Treatment 2")
raw.acari.3 = read.xlsx("H:\\JenaSP6_2021\\Jena treatment 1 2 3 densities.xlsx",
                        sheet = "Treatment 3")

raw.acari = rbind(raw.acari.1,raw.acari.2,raw.acari.3) %>% 
  relocate(Plot, Treatment) %>% 
  arrange(Plot, Treatment) %>% 
  mutate(Treatment = str_c("Treatment", Treatment))


comp = in_core %>% rename(Treatment = Subplot) %>% 
  full_join(., raw.acari, by = join_by(Plot,Treatment)) %>% 
  select(1:2,4,9:11) %>% 
  #arrange(Plot,Treatment) %>% 
  rowwise() %>% 
  mutate(.after = Acari,
         sumIDd = sum(Oribatida,Mesostigmata,`Prostigmata.+.Astigmata`),
         d = sumIDd - Acari,
         r = sumIDd / Acari )

# B1A12T1 <-> B1A122T3




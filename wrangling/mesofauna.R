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

# get 
gs4_deauth() #does this work?
in_core = read_sheet("https://docs.google.com/spreadsheets/d/1Zy3SbxS-n7lGFYuEcQ6X7sMgmP6lJMa7PGIc1n7C4xg/edit#gid=0",
                     sheet = "Counts") %>% 
  slice(-c(131,137,190,241:243)) %>% 
  rename(Treatment = Subplot)

other.lengths = read_sheet("https://docs.google.com/spreadsheets/d/1Zy3SbxS-n7lGFYuEcQ6X7sMgmP6lJMa7PGIc1n7C4xg/edit#gid=1042370017",
                 sheet = "Others_Bodylength",
                 range = "A1:E1441",
                 na = "") %>% 
  rename(Treatment = Subplot)


# check your numbers again!
other.masses = other.lengths %>% 
  pivot_longer(3:5,
               names_to = "taxon",
               values_to = "length.mm") %>%  # Is it really mm???????
  mutate(FreshMass.mg = (10^(1.339 + (1.992 * log10(length.mm))))/1e3) %>% 
  drop_na()

mean.masses = other.masses %>% 
  group_by(Plot, Treatment, taxon) %>% 
  summarise(meanMass.mg = mean(FreshMass.mg, na.rm = T))

#plot(other.masses$length.mm, other.masses$FreshMass.mg)

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
              values_from = abun)%>% 
  rename(Treatment = Subplot)


coll.traits = data.frame(Family = names(df_new)[4:14],
                         Order = NA,
                         form = NA)

coll.traits = coll.traits %>% mutate(Order = case_when(Family %in% c("Dicyrtomidae",
                                                                     "Katiannidae",
                                                                     "Sminthuridae",
                                                                     "Synphypleona juv.") ~ "Symphypleona",
                                                       Family %in% c("Hypogastruridae",
                                                                     "Neanuridae",
                                                                     "Onychiuridae",
                                                                     "Tullbergiidae") ~ "Poduromorpha",
                                                       Family %in% c("Entomobryidae",
                                                                     "Isotomidae") ~ "Entomobryomorpha",
                                                       Family == "Neelidae" ~ "Neelipleona",
                                                       TRUE ~ "missing"),
                                     form = case_when(Family %in% c("Dicyrtomidae",
                                                                    "Katiannidae",
                                                                    "Sminthuridae",
                                                                    "Synphypleona juv.",
                                                                    "Entomobryidae",
                                                                    "Hypogastruridae",
                                                                    "Neanuridae") ~ "Epigeic",
                                                     TRUE ~ "Edaphic")) %>% 
  arrange(form) %>% 
  mutate(form_Order = paste(form, Order))

# # get 
# gs4_deauth() #does this work?
# in_core = read_sheet("https://docs.google.com/spreadsheets/d/1Zy3SbxS-n7lGFYuEcQ6X7sMgmP6lJMa7PGIc1n7C4xg/edit#gid=0",
#                      sheet = "Counts") %>% 
#   slice(-c(131,137,190,241:243))

# comp.1 = in_core %>% #rename(Treatment = Subplot) %>% 
#   full_join(., df_new, by = join_by(Plot,Subplot)) %>% 
#   select(1:3,9) %>% 
#   #arrange(Plot,Treatment) %>% 
#   rowwise() %>% 
#   mutate(.after = `Collembola total`,
#          d = `Collembola total` - Collembola,
#          r = `Collembola total` / Collembola )


# create a plot-subplot dataframe
plot_sub = raw[ ,1:2] %>% arrange(Plot, Subplot)
# expand so every subplot gets 10 rows
plot_sub_expanded = plot_sub[rep(seq_len(nrow(plot_sub)), each = 10), ]

# column name index
name.index = raw %>% select(-c(contains("length"),26)) 
# add family names to length columns
coll = raw %>% rename_at(vars(starts_with('length')), funs(paste(colnames(name.index[,4:14]), .))) %>% 
  arrange(Plot, Subplot)
coll = as.data.frame(coll)
# keep lengths only
colll = coll %>% select(contains("length"))

# add family columns into expanded dataframe
for(i in colnames(name.index[4:14])) { 
  plot_sub_expanded[,i] <- NA
}

# now we fill the expanded dataframe with body lengths
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
  plot_sub_expanded[,2+i] <- new_vector
}

collembola.lengths = plot_sub_expanded %>% 
  # remove spaces
  mutate_all(function(x) gsub(" ","",x)) %>% 
  # change decimal
  mutate_all(function(x) gsub(",",".",x))
# one last typo (if only)
collembola.lengths[collembola.lengths == "07"] = "0.7"
# turn to numeric
collembola.lengths[,3:13] = collembola.lengths[,3:13] %>% 
  mutate_if(is.character,as.numeric)
# couple more issues
collembola.lengths[collembola.lengths == 0] = NA
collembola.lengths[collembola.lengths == 9.4] = NA

collembola.masses = collembola.lengths %>% 
  rename(Treatment = Subplot) %>% 
  pivot_longer(3:13,
               names_to = "taxon",
               values_to = "length.mm") %>% 
  mutate(FreshMass.mg = (10^(1.339 + (1.992 * log10(length.mm))))/1e3) %>% 
  drop_na() %>% 
  arrange(Plot, Treatment, taxon)

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

acari.lengths = read.xlsx("H:\\JenaSP6_2021\\Jena Experiment Mite Length-D2-from measurements.xlsx",
                          sheet = "All measurements",
                          rows = c(1,3:252)) %>%
  # fuck yes!!!
  fill(Plot) %>% 
  select(-2) %>% 
  add_column(.after = "Plot",
             Treatment = "Treatment2")

acari.masses = acari.lengths %>% 
  pivot_longer(3:5,
               names_to = "taxon",
               values_to = "length.micro") %>%  # Is it really mm???????
  mutate(FreshMass.mg = case_when(# based on Mercer 2001 10.1017/S0954102001000219
    # (dividing by 1e3 because Mercer works with micrograms for these taxa)
    taxon ==    "Oribatida" ~ (10^(2.146 + (2.770 * log(length.micro/1e3)))) / 1e3,
    taxon ==  "Prostigmata" ~ (10^(2.124 + (2.808 * log(length.micro/1e3)))) / 1e3,
    taxon == "Mesostigmata" ~ (10^(2.064 + (2.857 * log(length.micro/1e3)))) / 1e3)) %>% 
  drop_na()

#plot(acari.masses$length.micro/1e3, acari.masses$FreshMass.mg)





# comp.2 = in_core %>% #rename(Treatment = Subplot) %>% 
#   full_join(., raw.acari, by = join_by(Plot,Treatment)) %>% 
#   select(1:2,4,9:11) %>% 
#   #arrange(Plot,Treatment) %>% 
#   rowwise() %>% 
#   mutate(.after = Acari,
#          sumIDd = sum(Oribatida,Mesostigmata,`Prostigmata.+.Astigmata`),
#          d = sumIDd - Acari,
#          r = sumIDd / Acari )

# B1A12T1 <-> B1A122T3

# !!!!!!!!!!! rowwise is needed once! use ungroup() to revert to columnwise
meso = # mesofauna as counted during sorting to groups
       in_core %>% 
  # combined with identified acari
  full_join(., raw.acari, by = join_by(Plot,Treatment)) %>% 
  # and identified collembola
  full_join(., df_new, by = join_by(Plot,Treatment)) %>% 
  # drop comments
  select(-Comments) %>% 
  # *.in is the number we counted, .out is the number that were identified
  rename(Collembola.in = Collembola,
         Collembola.out = `Collembola total`,
         Acari.in = Acari,
         Prostigmata = `Prostigmata.+.Astigmata`) %>% 
  rowwise() %>% 
  mutate(.after = Acari.in,
         Acari.out = sum(Oribatida,Mesostigmata,Prostigmata),
         # there are small discrepancies between in and out, I am assuming that 
         # the largest number is more reliable... (worth a sensitivity analysis)
         Acari = max(Acari.in,Acari.out, na.rm = T),
         Collembola = max(Collembola.in,Collembola.out, na.rm = T)) %>% 
  select(-c(Acari.in,Acari.out,Collembola.in,Collembola.out,)) %>% 
  # this will scale proportions of the tree groups to the number of mites counted
  # (only relevant for cases where we counted more than were identified
  rowwise() %>%
  mutate(  denominator = sum(Oribatida,Mesostigmata,Prostigmata),
            Oribatida = Acari*(Oribatida    / denominator),
         Mesostigmata = Acari*(Mesostigmata / denominator),
         Prostigmata = Acari*(Prostigmata / denominator)) %>% 
  select(-c(Acari, denominator)) %>% 
  # grouping Collembola to functional groups
  mutate(.keep = "unused", 
          Edaphic.Entomobryomorpha = Isotomidae,
               Edaphic.Neelipleona = Neelidae,
              Edaphic.Poduromorpha = sum(Onychiuridae, 
                                         Tullbergiidae),
              Epigeic.Symphypleona = sum(Dicyrtomidae,
                                         Katiannidae,
                                         Sminthuridae,
                                         `Synphypleona juv.`),
          Epigeic.Entomobryomorpha = Entomobryidae,
              Epigeic.Poduromorpha = sum(Hypogastruridae, 
                                         Neanuridae)) %>% 
  # same as for mites but for Collembola
  # (only relevant for cases where we counted more than were identified
  rowwise() %>%
  mutate(denominator = sum(Edaphic.Entomobryomorpha, 
                           Edaphic.Neelipleona, 
                           Edaphic.Poduromorpha, 
                           Epigeic.Symphypleona,
                           Epigeic.Entomobryomorpha,
                           Epigeic.Poduromorpha),
         Edaphic.Entomobryomorpha = Collembola*(Edaphic.Entomobryomorpha / denominator),
         Edaphic.Neelipleona      = Collembola*(Edaphic.Neelipleona      / denominator),
         Edaphic.Poduromorpha     = Collembola*(Edaphic.Poduromorpha     / denominator),
         Epigeic.Symphypleona     = Collembola*(Epigeic.Symphypleona     / denominator),
         Epigeic.Entomobryomorpha = Collembola*(Epigeic.Entomobryomorpha / denominator),
         Epigeic.Poduromorpha     = Collembola*(Epigeic.Poduromorpha     / denominator)) %>% 
  select(-c(Collembola, denominator)) %>% 
  # (in.core divided by surface sampled to get density per cm^2, 
  #  then multiplying by 1e4 to get density per m^2)
  mutate(across(where(is.numeric), ~ (./(pi*(2.5)^2))*1e4),
         # then I round up to get whole individuals 
         # (necessary for sampling bodymass distributions)
         across(where(is.numeric), ceiling))

meso.masses = other.masses %>% 
              full_join(collembola.masses) %>% 
              select(-length.mm) %>% 
              full_join(.,
                        acari.masses %>% select(-length.micro))

mean.meso.masses = meso.masses %>% 
                   mutate(taxon = case_when(taxon %in% coll.traits$Family ~ coll.traits$form_Order[match(.$taxon, coll.traits$Family)],
                                            TRUE ~ taxon)) %>% 
                   group_by(taxon) %>% 
                   summarise(N = n(),
                             MeanMass.mg = mean(FreshMass.mg),
                              StDMass.mg = sd(FreshMass.mg))
                        
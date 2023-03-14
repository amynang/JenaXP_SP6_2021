library(tidyverse)
library(openxlsx)

############################ in-core Abundances ################################

# the following exclude Hymenoptera, Formicidae, Lumbricidae, Enchytraedae, "Larven, Puppen"
raw.1 = read.xlsx("H:\\JenaSP6_2021\\August 22 Angelos Jena Experiment fertige Zählliste B1, B2,B3, B4_macrofauna.xlsx",
                  sheet = "Block 1",
                  startRow = 2,
                  cols = c(1,3:14,17,18),
                  na.strings = "-",)

raw.2 = read.xlsx("H:\\JenaSP6_2021\\August 22 Angelos Jena Experiment fertige Zählliste B1, B2,B3, B4_macrofauna.xlsx",
                  sheet = "Block 2",
                  startRow = 2,
                  cols = c(1,3:14,17,18),
                  na.strings = "-",)

raw.3 = read.xlsx("H:\\JenaSP6_2021\\August 22 Angelos Jena Experiment fertige Zählliste B1, B2,B3, B4_macrofauna.xlsx",
                  sheet = "Block 3",
                  startRow = 2,
                  cols = c(1,3:14,17,18),
                  na.strings = "-",)

raw.4 = read.xlsx("H:\\JenaSP6_2021\\August 22 Angelos Jena Experiment fertige Zählliste B1, B2,B3, B4_macrofauna.xlsx",
                  sheet = "Block 4",
                  startRow = 2,
                  cols = c(1,3:14,17,18),
                  na.strings = "-",)

str(raw.1)
str(raw.2)
str(raw.3)
str(raw.4)

# remove all doubt, make numeric
raw.2$Staphylinidae = as.numeric(gsub("\\?| ", "", raw.2$Staphylinidae))
raw.4$Staphylinidae = as.numeric(gsub("\\?| ", "", raw.4$Staphylinidae))

# do you see an emoticon having a mental breakdown in there? no? just me then
raw.2$`Rynchota./.Larven` = as.numeric(gsub("\\*|_|/.*|\\\\.*", "", raw.2$`Rynchota./.Larven`))
raw.4$`Rynchota./.Larven` = as.numeric(gsub("\\*|_|/.*|\\\\.*", "", raw.4$`Rynchota./.Larven`))

macro = bind_rows(raw.1, raw.2, raw.3, raw.4) %>% 
  # add plotcode, correct typo
  rename(Plotcode = X1, Opiliones = Opoliones) %>% 
  # NAs are zeros not absent values
  replace(is.na(.), 0) %>%
  # sum adult and juv hemiptera, rename
  mutate(.keep = "unused",
         .after = Coleoptera.Larven,
         Hemiptera = Rynchota + `Rynchota./.Larven`) %>% 
  mutate(.before = Thysanoptera,
         Plot = str_split(.$Plotcode, "-", simplify = T)[,1],
         Treatment = str_split(.$Plotcode, "T", simplify = T)[,2],
         Treatment = paste0("Treatment", Treatment)) %>% 
  rowwise() %>% 
  mutate(.keep = "unused",
         # this is terrible but we don't know what the larvae are
         Coleoptera = sum(Staphylinidae, Coleptera, Coleoptera.Larven),
         # I am assuming that the adult flies found in the sample 
         # emerged from the soil between sampling and extraction
         # (rather than coming through the window)
         Diptera.larvae = sum(Diptera, Diptera.Larven)) %>% 
  ungroup() %>%  
  select(-c(Plotcode, Pseudoscorpiones, Opiliones)) %>% 
  # (in.core divided by surface sampled to get density per cm^2, 
  #  then multiplying by 1e4 to get density per m^2)
  mutate(across(where(is.numeric), ~ (./(pi*(7.5)^2))*1e4),
         # then I round up to get whole individuals 
         # (necessary for sampling bodymass distributions)
         across(where(is.numeric), ceiling))

macro$Plot[macro$Plot == "BA305"] = "B3A05"

table(macro$Plot, macro$Treatment)


############################# Body size measurements ###########################
source("wrangling/functions.R")

l_w_list = vector(mode = "list")

################################ Spiders #######################################

Araneae = rbind(tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Aranea_Measurement_List_JenaExp_Angelos.xlsx",
                                               sheet = "Aranea_B1"), "Araneae"),
                tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Aranea_Measurement_List_JenaExp_Angelos.xlsx",
                                               sheet = "Aranea_B2"), "Araneae"),
                tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Aranea_Measurement_List_JenaExp_Angelos.xlsx",
                                               sheet = "Aranea_B3"), "Araneae"),
                tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Aranea_Measurement_List_JenaExp_Angelos.xlsx",
                                               sheet = "Aranea_B4"), "Araneae"))

############################### Rove beetles ###################################

Staphylinidae = rbind(tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Staphylinidae_Measurement_List_JenaExp_Angelos.xlsx",
                                             sheet = "Staphylinidae_B1"), "Staphylinidae"),
              tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Staphylinidae_Measurement_List_JenaExp_Angelos.xlsx",
                                             sheet = "Staphylinidae_B2"), "Staphylinidae"),
              tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Staphylinidae_Measurement_List_JenaExp_Angelos.xlsx",
                                             sheet = "Staphylinidae_B3"), "Staphylinidae"),
              tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Staphylinidae_Measurement_List_JenaExp_Angelos.xlsx",
                                             sheet = "Staphylinidae_B4"), "Staphylinidae"))

################################ Hemiptera #######################################

Hemiptera = rbind(tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Hemiptera_Measurement_List_JenaExp_Angelos.xlsx",
                                                 sheet = "Hemiptera_B1"), "Hemiptera"),
                  tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Hemiptera_Measurement_List_JenaExp_Angelos.xlsx",
                                                 sheet = "Hemiptera_B2"), "Hemiptera"),
                  tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Hemiptera_Measurement_List_JenaExp_Angelos.xlsx",
                                                 sheet = "Hemiptera_B3"), "Hemiptera"),
                  tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Hemiptera_Measurement_List_JenaExp_Angelos.xlsx",
                                                 sheet = "Hemiptera_B4"), "Hemiptera"))

l_w_list[["Araneae"]] = Araneae
l_w_list[["Staphylinidae"]] = Staphylinidae
l_w_list[["Hemiptera"]] = Hemiptera

################################## the rest ####################################

for (i in c("Chilopoda","Diplopoda","Isopoda",
            "Coleoptera","Thysanoptera","Gastropoda",
            "Formicidae", "Lumbricidae")) {
  
  l_w_list[[i]] = tidybodymeasurements(read.xlsx("H:\\JenaSP6_2021\\Measurement_List_JenaExp_Samples_Mona.xlsx",
                                                 sheet = i), i)
  
}


macro.masses = do.call(rbind.data.frame, l_w_list) %>% drop_na() %>% 
  # is width larger than length?
  mutate(weird = length_micro < width_micro) %>% 
  # drop those weird ones
  filter(weird != TRUE) %>% select(-weird) %>% 
  # calculate individual fresh bodymasses
  mutate(FreshMass.mg = case_when( # based on Sohlström 2018 10.1002/ece3.4702
                                  # The general relationship (model 3)
                                  TRUE ~ 10^(- .285 + (1.040 * log10(length_micro/1e3)) + (1.585 * log10(width_micro/1e3))),
                                  # group specific coefficients (model 1)
                                  taxon ==       "Araneae" ~ 10^(- .281 + (1.368 * log10(length_micro/1e3)) + (1.480 * log10(width_micro/1e3))),
                                  taxon ==    "Coleoptera" ~ 10^(- .286 + ( .840 * log10(length_micro/1e3)) + (1.954 * log10(width_micro/1e3))),
                                  taxon == "Staphylinidae" ~ 10^(- .286 + ( .840 * log10(length_micro/1e3)) + (1.954 * log10(width_micro/1e3))),
                                  taxon ==       "Diptera" ~ 10^(- .309 + ( .997 * log10(length_micro/1e3)) + (1.595 * log10(width_micro/1e3))),
                                  taxon ==     "Hemiptera" ~ 10^(- .420 + (1.177 * log10(length_micro/1e3)) + (1.431 * log10(width_micro/1e3))),
                                  taxon ==       "Isopoda" ~ 10^(- .453 + ( .898 * log10(length_micro/1e3)) + (1.756 * log10(width_micro/1e3))),
                                  taxon ==     "Chilopoda" ~ 10^(- .549 + (1.416 * log10(length_micro/1e3)) + (1.543 * log10(width_micro/1e3))),
                                  taxon ==     "Diplopoda" ~ 10^(-1.400 + (2.443 * log10(length_micro/1e3)) + (0.215 * log10(width_micro/1e3))))
  )

mean.macro.masses = macro.masses %>% 
                    mutate(taxon = ifelse(taxon == "Staphylinidae", "Coleoptera", taxon)) %>% 
                    group_by(taxon) %>% 
                    summarise(N = n(),
                              MeanMass.mg = mean(FreshMass.mg),
                              StDMass.mg = sd(FreshMass.mg)) %>% 
  # I know things
  add_row(taxon = "Diptera.larvae",
          MeanMass.mg = 0.249,
          StDMass.mg = 0.0578) %>% 
  filter(!(taxon %in% c("Lumbricidae","Formicidae")))



# macro.masses %>% as.data.frame() %>% 
#   filter(taxon == "Araneae") %>% 
#   select(FreshMass.mg) %>% is.numeric()
#   density() %>% 
#   plot()
# 
# plot(density((macro.masses[macro.masses$taxon == "Araneae",]$FreshMass.mg)))
# 
# hist(log(macro.masses[macro.masses$taxon == "Hemiptera",]$FreshMass.mg), breaks = 100)
# 
# 
# min(macro.masses[macro.masses$taxon == "Chilopoda",]$FreshMass.mg);max(macro.masses[macro.masses$taxon == "Chilopoda",]$FreshMass.mg);mean(macro.masses[macro.masses$taxon == "Chilopoda",]$FreshMass.mg);sd(macro.masses[macro.masses$taxon == "Chilopoda",]$FreshMass.mg)
# plot(density((macro.masses[macro.masses$taxon == "Chilopoda",]$FreshMass.mg)))
# dum = rlnormtrunc.intuitive(5000, 
#                             mean(macro.masses[macro.masses$taxon == "Chilopoda",]$FreshMass.mg), 
#                             sd(macro.masses[macro.masses$taxon == "Chilopoda",]$FreshMass.mg),
#                             min = min(macro.masses[macro.masses$taxon == "Chilopoda",]$FreshMass.mg),
#                             max = max(macro.masses[macro.masses$taxon == "Chilopoda",]$FreshMass.mg))
# min(dum);max(dum);mean(dum);sd(dum)
# plot(density(dum))
# 
# rlnormtrunc.intuitive(5000, 
#                       mean(macro.masses[macro.masses$taxon == "Hemiptera",]$FreshMass.mg), 
#                       sd(macro.masses[macro.masses$taxon == "Hemiptera",]$FreshMass.mg),
#                       max = max(macro.masses[macro.masses$taxon == "Hemiptera",]$FreshMass.mg)) 
# 
# 
# dlnormtrunc.intuitive = function(x, m, s, p=.9) {
#   trnc <- EnvStats::dlnormTrunc(x, 
#                                 meanlog = log(m^2 / sqrt(s^2 + m^2)), 
#                                 sdlog = sqrt(log(1 + (s^2 / m^2))), 
#                                 min = qlnorm((1-p)/2, 
#                                              meanlog = log(m^2 / sqrt(s^2 + m^2)), 
#                                              sdlog = sqrt(log(1 + (s^2 / m^2)))), 
#                                 max = qlnorm(1-(1-p)/2, 
#                                              meanlog = log(m^2 / sqrt(s^2 + m^2)), 
#                                              sdlog = sqrt(log(1 + (s^2 / m^2)))))
#   return(trnc)
# }
# 
# rlnormtrunc.intuitive = function(n, m, s, p=.9) {
#   trnc <- EnvStats::rlnormTrunc(n, 
#                                 meanlog = log(m^2 / sqrt(s^2 + m^2)), 
#                                 sdlog = sqrt(log(1 + (s^2 / m^2))), 
#                                 min = qlnorm((1-p)/2, 
#                                              meanlog = log(m^2 / sqrt(s^2 + m^2)), 
#                                              sdlog = sqrt(log(1 + (s^2 / m^2)))), 
#                                 max = qlnorm(1-(1-p)/2, 
#                                              meanlog = log(m^2 / sqrt(s^2 + m^2)), 
#                                              sdlog = sqrt(log(1 + (s^2 / m^2)))))
#   return(trnc)
# }
# 
# rlnormtrunc.intuitive = function(n, m, s, min, max) {
#   trnc <- EnvStats::rlnormTrunc(n, 
#                                 meanlog = log(m^2 / sqrt(s^2 + m^2)), 
#                                 sdlog = sqrt(log(1 + (s^2 / m^2))), 
#                                 min = min, 
#                                 max = max)
#   return(trnc)
# }
# 
# 
# 
# 
# 
# df <- data.frame(
#   Data=factor(rep(c("D1", "D2"), each=2000)),
#   weight=c(log(rlnormtrunc.intuitive(2000, m=1.672674/4, s=4.982685/4, p=1)),
#            log(rlnormtrunc.intuitive(2000, m=1.672674,  s=4.982685, p=1)))
# )
# #df$weight = log10(df$weight)
# d1dens <- with(df, density(weight[Data == "D1"], 
#                            from = min(weight), 
#                            to = max(weight)))
# d2dens <- with(df, density(weight[Data == "D2"], 
#                            from = min(weight),
#                            to = max(weight)))
# joint <- pmin(d1dens$y, d2dens$y)
# 
# df2 <- data.frame(x = rep(d1dens$x, 3), 
#                   y = c(d1dens$y, d2dens$y, joint),
#                   Data = rep(c("D1", "D2", "overlap"), each = length(d1dens$x)))
# 
# ggplot(df2, aes(x, y, fill = Data)) + 
#   geom_area(position = position_identity(), color = "black") +
#   scale_fill_brewer(palette = "Pastel2") +
#   theme_bw()
# 
# 
# 
# library(brms)
# m = brm(bf(log10(FreshMass.mg) #| trunc(lb = 0.02, ub = 44) 
#            ~ 1 + (1|Probe),
#            sigma ~ 1 + (1|Probe)), 
#         data = all[all$taxon == "Araneae",],
#         #family = lognormal,
#         family = gaussian(),
#         #prior = c(prior(lognormal(-0.4581454, 0.6999362), class = Intercept),
#         #          prior(exponential(1), class = sd)),
#         cores = 4, 
#         chains = 4, 
#         control = list( adapt_delta = .95),
#         backend = "cmdstanr")
# pp_check(m, ndraws = 100)
# summary(m)
# 
# m %>% conditional_effects(effect = "Probe",
#                           #robust = F,
#                           re_formula = NULL)
# 
# 
# m = brm(bf(log10(FreshMass.mg) #| trunc(lb = 0.02, ub = 44) 
#            ~ taxon + (taxon|Probe),
#            sigma ~ taxon + (taxon|Probe)), 
#         data = all,
#         #family = lognormal,
#         family = gaussian(),
#         #prior = c(prior(lognormal(-0.4581454, 0.6999362), class = Intercept),
#         #          prior(exponential(1), class = sd)),
#         cores = 4, 
#         chains = 4, 
#         control = list( adapt_delta = .9),
#         backend = "cmdstanr")
# pp_check(m, ndraws = 100)
# summary(m)
# 
# 
# 
# 
# 
# 
# curve((.159 + .33 *log10(x)), from = 1e-5, to = 1e1)
# curve(10^(.159 + .33 *log10(x)), from = 1e-3, to = 1e1)
# curve(10^(.175 + .281*log10(x)), from = 1e-5, to = 1e1, add = T)
# curve(10^(.143 + .379*log10(x)), from = 1e-5, to = 1e1, add = T)
# curve(10^(.175 + .379*log10(x)), from = 1e-5, to = 1e1, add = T)
# curve(10^(.143 + .281*log10(x)), from = 1e-5, to = 1e1, add = T)
# 
# 
# curve(10^(.159 + .33 *log10(x)), from = 1e-3, to = 1e1)
# abline(h = 1)

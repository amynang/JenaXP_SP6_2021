library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)
library(emmeans)

thou = read.csv("fluxes/fluxes1000_imputed_dir100.csv") 


######################### Community level energy flux ##########################


total = thou %>% 
  group_by(Block, Plot, Plant.Richness, Treatment) %>% 
  summarise(tot.flux.m = mean(log10(tot.flux)),
            tot.flux.sd = sd(log10(tot.flux))) %>% 
  ungroup()%>% 
  mutate(Plant.Richness.sc = (log2(Plant.Richness) - mean(log2(Plant.Richness)))/sd(log2(Plant.Richness)))

total$Treatment = factor(total$Treatment, levels = c("Treatment3","Treatment2","Treatment1"))


m.tot = brm(bf(tot.flux.m|mi(tot.flux.sd) ~ 1  
           + Plant.Richness.sc 
           + Treatment 
           + Plant.Richness.sc:Treatment
           + (1 + Treatment|Plot))
           ,family = gaussian(), #link = "log"
           chains = 4,
           iter = 4000,
           cores = 4,
           control = list(adapt_delta = 0.95),
           backend = "cmdstanr",
           seed = 404,
           data = total,
           file = "analysis_main/m.tot")

summary(m.tot, prob = 0.9)

# slopes for each treatment
tot.emt = emtrends(m.tot, "Treatment", var = "Plant.Richness.sc")
summary(tot.emt, point.est = mean, level = .9)
tot.em = emmeans (m.tot, pairwise  ~ Treatment | Plant.Richness.sc)
tot.em = emmeans (m.tot, pairwise  ~ Treatment | Plant.Richness.sc,
                  at = list(Plant.Richness.sc = c(-1.3786776, 1.1333247, 2.3308531)))
summary(tot.em, point.est = mean, level = .9)
# pairwise comparisons
tot.pairs = pairs(tot.emt)
summary(tot.pairs, point.est = mean, level = .9)

pp_check(m.tot, 
         ndraws = 100)
#plot(m.tot)


total %>% 
  group_by(Plot, tot.flux.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_epred_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.tot, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (tot.flux.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.epred), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = total, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Community level energy flux log10(J/h\u00b7m\u00b2)",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)))






################################ Predatory fluxes ##############################

pred = thou %>% 
  group_by(Block, Plot, Plant.Richness, Treatment) %>% 
  summarise(pred.flux.m = mean(log10(pred.flux)),
            pred.flux.sd = sd(log10(pred.flux))) %>% 
  ungroup()%>% 
  mutate(Plant.Richness.sc = (log2(Plant.Richness) - mean(log2(Plant.Richness)))/sd(log2(Plant.Richness)))

pred$Treatment = factor(pred$Treatment, levels = c("Treatment3","Treatment2","Treatment1"))


m.pred = brm(bf(pred.flux.m|mi(pred.flux.sd) ~ 1  
               + Plant.Richness.sc 
               + Treatment 
               + Plant.Richness.sc:Treatment
               + (1 + Treatment|Plot))
            ,family = gaussian(), #link = "log"
            chains = 4,
            iter = 4000,
            cores = 4,
            control = list(adapt_delta = 0.99),
            backend = "cmdstanr",
            seed = 404,
            data = pred,
            file = "analysis_main/m.pred")

summary(m.pred, prob = 0.9)

# slopes for each treatment
pred.emt = emtrends(m.pred, "Treatment", var = "Plant.Richness.sc")
summary(pred.emt, point.est = mean, level = .9)
pred.em = emmeans (m.pred, pairwise  ~ Treatment | Plant.Richness.sc)
pred.em = emmeans (m.pred, pairwise  ~ Treatment | Plant.Richness.sc,
                   at = list(Plant.Richness.sc = c(-1.3786776, 1.1333247, 2.3308531)))
summary(pred.em, point.est = mean, level = .9)
# pairwise comparisons
pred.pairs = pairs(pred.emt)
summary(pred.pairs, point.est = mean, level = .9)

pp_check(m.pred, ndraws = 100)
#plot(m.pred)


pred %>%
  group_by(Plot, pred.flux.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_epred_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.pred, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (pred.flux.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.epred), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = pred, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Predation log10(J/h\u00b7m\u00b2)",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)))


################################## Herbivory ###################################

herb = thou %>% 
  group_by(Block, Plot, Plant.Richness, Treatment) %>% 
  summarise(herb.flux.m = mean(log10(herb.flux)),
            herb.flux.sd = sd(log10(herb.flux))) %>% 
  ungroup()%>% 
  mutate(Plant.Richness.sc = (log2(Plant.Richness) - mean(log2(Plant.Richness)))/sd(log2(Plant.Richness)))

herb$Treatment = factor(herb$Treatment, levels = c("Treatment3","Treatment2","Treatment1"))


m.herb = brm(bf(herb.flux.m|mi(herb.flux.sd) ~ 1  
                + Plant.Richness.sc 
                + Treatment 
                + Plant.Richness.sc:Treatment
                + (1 + Treatment|Plot))
             ,family = gaussian(), #link = "log"
             chains = 4,
             iter = 4000,
             cores = 4,
             control = list(adapt_delta = 0.95),
             backend = "cmdstanr",
             seed = 404,
             data = herb,
             file = "analysis_main/m.herb")

summary(m.herb, prob = 0.9)

# slopes for each treatment
herb.emt = emtrends(m.herb, "Treatment", var = "Plant.Richness.sc")
summary(herb.emt, point.est = mean, level = .9)
herb.em = emmeans (m.herb, pairwise  ~ Treatment | Plant.Richness.sc)
herb.em = emmeans (m.herb, pairwise  ~ Treatment | Plant.Richness.sc,
                  at = list(Plant.Richness.sc = c(-1.3786776, 1.1333247, 2.3308531)))
summary(herb.em, point.est = mean, level = .9)
# pairwise comparisons
herb.pairs = pairs(herb.emt)
summary(herb.pairs, point.est = mean, level = .9)


pp_check(m.herb, ndraws = 100)
#plot(m.herb)


herb %>%
  group_by(Plot, herb.flux.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_eherb_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.herb, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (herb.flux.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.eherb), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = herb, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Herbivory log10(J/h\u00b7m\u00b2)",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)))


############################ Herbivory Pressure ################################


press = thou %>% 
  group_by(Block, Plot, Plant.Richness, Treatment) %>% 
  summarise(herb.press.m = mean(log10(herb.press)),
            herb.press.sd = sd(log10(herb.press))) %>% 
  ungroup()%>% 
  mutate(Plant.Richness.sc = (log2(Plant.Richness) - mean(log2(Plant.Richness)))/sd(log2(Plant.Richness)))

press$Treatment = factor(press$Treatment, levels = c("Treatment3","Treatment2","Treatment1"))

m.pres = brm(bf(herb.press.m|mi(herb.press.sd) ~ 1  
           + Plant.Richness.sc 
           + Treatment 
           + Plant.Richness.sc:Treatment
           + (1 + Treatment|Plot))
        ,family = gaussian(), #link = "log"
        chains = 4,
        iter = 4000,
        cores = 4,
        control = list(adapt_delta = 0.95),
        backend = "cmdstanr",
        seed = 404,
        data = press,
        file = "analysis_main/m.pres")

summary(m.pres, prob = 0.9)

# slopes for each treatment
pres.emt = emtrends(m.pres, "Treatment", var = "Plant.Richness.sc")
summary(pres.emt, point.est = mean, level = .9)
pres.em = emmeans (m.pres, pairwise  ~ Treatment | Plant.Richness.sc)
pres.em = emmeans (m.pres, pairwise  ~ Treatment | Plant.Richness.sc,
                   at = list(Plant.Richness.sc = c(-1.3786776, 1.1333247, 2.3308531)))
summary(pres.em, point.est = mean, level = .9)
# pairwise comparisons
pres.pairs = pairs(pres.emt)
summary(pres.pairs, point.est = mean, level = .9)

pp_check(m.pres, 
         ndraws = 100)
#plot(m.pres)

press %>%
  group_by(Plot, herb.press.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_epred_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.pres, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (herb.press.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.epred), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = press, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Herbivory pressure on Plants log10()",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)))


############################## Herbivory Control ###############################


contr = thou %>% 
  group_by(Block, Plot, Plant.Richness, Treatment) %>% 
  summarise(contr.m = mean(down/up),
            contr.sd = sd(down/up)) %>% 
  ungroup()%>% 
  mutate(Plant.Richness.sc = (log2(Plant.Richness) - mean(log2(Plant.Richness)))/sd(log2(Plant.Richness)))

contr$Treatment = factor(contr$Treatment, levels = c("Treatment3","Treatment2","Treatment1"))

m.cont = brm(bf(contr.m|mi(contr.sd) ~ 1  
           + Plant.Richness.sc 
           + Treatment 
           + Plant.Richness.sc:Treatment
           + (1 + Treatment|Plot))
        ,family = Beta(), #link = "log"
        chains = 4,
        iter = 4000,
        cores = 4,
        control = list(adapt_delta = 0.95),
        backend = "cmdstanr",
        seed = 404,
        data = contr,
        file = "analysis_main/m.cont")

summary(m.cont, prob = 0.9)

# slopes for each treatment
cont.emt = emtrends(m.cont, "Treatment", var = "Plant.Richness.sc")
summary(cont.emt, point.est = mean, level = .9)
cont.em = emmeans (m.cont, pairwise  ~ Treatment | Plant.Richness.sc)
cont.em = emmeans (m.cont, pairwise  ~ Treatment | Plant.Richness.sc,
                   at = list(Plant.Richness.sc = c(-1.3786776, 1.1333247, 2.3308531)))
summary(cont.em, point.est = mean, level = .9)
# pairwise comparisons
cont.pairs = pairs(cont.emt)
summary(cont.pairs, point.est = mean, level = .9)

pp_check(m.cont, 
         ndraws = 100)
#plot(m.cont)


contr %>%
  group_by(Plot, contr.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_epred_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.cont, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (contr.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.epred), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = contr, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Control of herbivory (outfluxes/influxes)",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)))







################################## Detritivory #################################


detr = thou %>% 
  group_by(Block, Plot, Plant.Richness, Treatment) %>% 
  summarise(detr.flux.m = mean(log10(detr.flux)),
            detr.flux.sd = sd(log10(detr.flux))) %>% 
  ungroup()%>% 
  mutate(Plant.Richness.sc = (log2(Plant.Richness) - mean(log2(Plant.Richness)))/sd(log2(Plant.Richness)))

detr$Treatment = factor(detr$Treatment, levels = c("Treatment3","Treatment2","Treatment1"))


m.detr = brm(bf(detr.flux.m|mi(detr.flux.sd) ~ 1  
                + Plant.Richness.sc 
                + Treatment 
                + Plant.Richness.sc:Treatment
                + (1 + Treatment|Plot))
             ,family = gaussian(), #link = "log"
             chains = 4,
             iter = 8000,
             cores = 4,
             control = list(adapt_delta = 0.99),
             backend = "cmdstanr",
             seed = 404,
             data = detr,
             file = "analysis_main/m.detr")

summary(m.detr, prob = 0.9)

# slopes for each treatment
detr.emt = emtrends(m.detr, "Treatment", var = "Plant.Richness.sc")
summary(detr.emt, point.est = mean, level = .9)
detr.em = emmeans (m.detr, pairwise  ~ Treatment | Plant.Richness.sc)
detr.em = emmeans (m.detr, pairwise  ~ Treatment | Plant.Richness.sc,
                   at = list(Plant.Richness.sc = c(-1.3786776, 1.1333247, 2.3308531)))
summary(detr.em, point.est = mean, level = .9)
# pairwise comparisons
detr.pairs = pairs(detr.emt)
summary(detr.pairs, point.est = mean, level = .9)

pp_check(m.detr, ndraws = 100)
#plot(m.detr)


detr %>%
  group_by(Plot, detr.flux.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_edetr_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.detr, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (detr.flux.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.edetr), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = detr, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Detritivory log10(J/h\u00b7m\u00b2)",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)))




####################### Secondary decomposer flux ##############################






secon.decomp = thou %>% 
  group_by(Block, Plot, Plant.Richness, Treatment) %>% 
  summarise(secon.decomp.flux.m = mean(log10(secon.decomp.flux)),
            secon.decomp.flux.sd = sd(log10(secon.decomp.flux))) %>% 
  ungroup()%>% 
  mutate(Plant.Richness.sc = (log2(Plant.Richness) - mean(log2(Plant.Richness)))/sd(log2(Plant.Richness)))

secon.decomp$Treatment = factor(secon.decomp$Treatment, levels = c("Treatment3","Treatment2","Treatment1"))


m.secon.decomp = brm(bf(secon.decomp.flux.m|mi(secon.decomp.flux.sd) ~ 1  
                + Plant.Richness.sc 
                + Treatment 
                + Plant.Richness.sc:Treatment
                + (1 + Treatment|Plot))
             ,family = gaussian(), #link = "log"
             chains = 4,
             iter = 8000,
             cores = 4,
             control = list(adapt_delta = 0.99),
             backend = "cmdstanr",
             seed = 404,
             data = secon.decomp,
             file = "analysis_main/m.secon.decomp")

summary(m.secon.decomp, prob = 0.9)

# slopes for each treatment
secon.decomp.emt = emtrends(m.secon.decomp, "Treatment", var = "Plant.Richness.sc")
summary(secon.decomp.emt, point.est = mean, level = .9)
secon.decomp.em = emmeans (m.secon.decomp, pairwise  ~ Treatment | Plant.Richness.sc)
secon.decomp.em = emmeans (m.secon.decomp, pairwise  ~ Treatment | Plant.Richness.sc,
                   at = list(Plant.Richness.sc = c(-1.3786776, 1.1333247, 2.3308531)))
summary(secon.decomp.em, point.est = mean, level = .9)
# pairwise comparisons
secon.decomp.pairs = pairs(secon.decomp.emt)
summary(secon.decomp.pairs, point.est = mean, level = .9)

pp_check(m.secon.decomp, ndraws = 100)
#plot(m.secon.decomp)


secon.decomp %>%
  group_by(Plot, secon.decomp.flux.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_esecon.decomp_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.secon.decomp, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (secon.decomp.flux.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.esecon.decomp), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = secon.decomp, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Microbivory log10(J/h\u00b7m\u00b2)",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)))






################################## Figures #####################################

library(patchwork)

 p1 = total %>% 
  group_by(Plot, tot.flux.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_epred_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.tot, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (tot.flux.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.epred), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = total, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Community level energy flux log10(J/h\u00b7m\u00b2)",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        legend.position = "bottom")



p2 = pred %>%
  group_by(Plot, pred.flux.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_epred_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.pred, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (pred.flux.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.epred), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = pred, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Predation log10(J/h\u00b7m\u00b2)",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        legend.position = "none")


p3 = herb %>%
  group_by(Plot, herb.flux.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_eherb_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.herb, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (herb.flux.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.eherb), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = herb, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Herbivory log10(J/h\u00b7m\u00b2)",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        legend.position = "none")


p4 = detr %>%
  group_by(Plot, detr.flux.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_edetr_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.detr, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (detr.flux.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.edetr), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = detr, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Detritivory log10(J/h\u00b7m\u00b2)",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        legend.position = "none")


p5 = secon.decomp %>%
  group_by(Plot, secon.decomp.flux.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_esecon.decomp_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.secon.decomp, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (secon.decomp.flux.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.esecon.decomp), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = secon.decomp, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Microbivory log10(J/h\u00b7m\u00b2)",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        legend.position = "none")


p1 + ((p3+p2)/(p4+p5))





p6 = press %>%
  group_by(Plot, herb.press.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_epred_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.pres, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (herb.press.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.epred), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = press, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Herbivory pressure on Plants log10()",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        legend.position = "none")


p7 = contr %>%
  group_by(Plot, contr.sd,
           #latitude, longitude,
           Treatment) %>%
  data_grid(Plant.Richness.sc = seq_range(Plant.Richness.sc, n = 101)) %>%
  # NOTE: this shows the use of ndraws to subsample within add_epred_draws()
  # ONLY do this IF you are planning to make spaghetti plots, etc.
  # NEVER subsample to a small sample to plot intervals, densities, etc.
  add_epred_draws(m.cont, #ndraws = 500, 
                  re_formula = NA) %>%
  ggplot(aes(x = Plant.Richness.sc, 
             y = (contr.m), 
             color = Treatment, 
             fill = Treatment)) +
  #geom_line(aes(y = (.epred), group = paste(Treatment, .draw)), alpha = .25) +
  geom_point(data = contr, alpha = .75,
             position = position_jitter(width = .1)) +
  stat_lineribbon(aes(y = (.epred)), 
                  .width = .9,
                  point_interval = "mean_hdi") +  
  scale_x_continuous(breaks = c(-1.3786776, -0.7506770, -0.1226765, 
                                0.5053241, 1.1333247, 2.3308531),
                     labels = c('1', '2', '4', '8', '16', '60')) +
  scale_fill_manual(values = c("#F3BE6140","#AA422E40","#6C6F8040"), 
                    name = "history treatment",
                    labels = c("soil (+), plant (+)", 
                               "soil (+), plant (--)", 
                               "soil (--), plant (--)")) +
  labs(#title = "Energy flux in the soil invertebrate food-web",
    y = "Control of herbivory (outfluxes/influxes)",
    x = "Plant richness") +
  scale_color_manual(values = c("#F3BE61","#AA422E","#6C6F80"), 
                     name = "history treatment",
                     labels = c("soil (+), plant (+)", 
                                "soil (+), plant (--)", 
                                "soil (--), plant (--)")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        legend.position = "none")

p6 + p7

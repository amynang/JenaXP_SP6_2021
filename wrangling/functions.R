


############## to extract body measurement data ######################
tidybodymeasurements <- function(df, taxon = "Taxon") {
  df = df %>% 
    # select lengths only
    select(starts_with("Probe"),
           contains("ä")) %>%
    mutate(across(contains("ä"), ~ gsub(",", "\\.", .))) %>%
    mutate(across(contains("ä"), ~ as.numeric(gsub(" \\s*\\([^\\)]+\\)", "", .)))) %>%
    pivot_longer(-1,
                 names_to = "specimen",
                 values_to = "length_micro") %>% 
    # simplify specimen names
    mutate(.after = Probe,
           .keep = "unused",
           specimen = str_split(.$specimen, "\\.", simplify = T)[,1] %>% 
             str_replace(., "Tier", taxon)
    ) %>% 
    # we combine this with...
    full_join(., 
              # now the same as above, for width
              df %>% 
                select(starts_with("Probe"),
                       contains("Breite")) %>%
                mutate(across(contains("Breite"), ~ gsub(",", "\\.", .))) %>%
                mutate(across(contains("Breite"), ~ as.numeric(gsub(" \\s*\\([^\\)]+\\)", "", .)))) %>%
                pivot_longer(-1,
                             names_to = "specimen",
                             values_to = "width_micro") %>% 
                mutate(.after = Probe,
                       .keep = "unused",
                       specimen = str_split(.$specimen, "\\.", simplify = T)[,1] %>% 
                         str_replace(., "Tier", taxon)), 
              by = join_by(Probe,specimen)) %>% 
    mutate(.before = specimen,
           taxon = gsub("[0-9]", "", specimen))
  
}

######## the log-normal of a given mean and sd NOT ON THE LOG SCALE ############
dlnormtrunc.intuitive = function(x, m, s, p=.9) {
  trnc <- EnvStats::dlnormTrunc(x, 
                                meanlog = log(m^2 / sqrt(s^2 + m^2)), 
                                sdlog = sqrt(log(1 + (s^2 / m^2))), 
                                min = qlnorm((1-p)/2, 
                                             meanlog = log(m^2 / sqrt(s^2 + m^2)), 
                                             sdlog = sqrt(log(1 + (s^2 / m^2)))), 
                                max = qlnorm(1-(1-p)/2, 
                                             meanlog = log(m^2 / sqrt(s^2 + m^2)), 
                                             sdlog = sqrt(log(1 + (s^2 / m^2)))))
  return(trnc)
}
######## random draws (from a log-normal) of a desired mean and sd #############
rlnormtrunc.intuitive = function(n, m, s, p=.9) {
  trnc <- EnvStats::rlnormTrunc(n, 
                                meanlog = log(m^2 / sqrt(s^2 + m^2)), 
                                sdlog = sqrt(log(1 + (s^2 / m^2))), 
                                min = qlnorm((1-p)/2, 
                                             meanlog = log(m^2 / sqrt(s^2 + m^2)), 
                                             sdlog = sqrt(log(1 + (s^2 / m^2)))), 
                                max = qlnorm(1-(1-p)/2, 
                                             meanlog = log(m^2 / sqrt(s^2 + m^2)), 
                                             sdlog = sqrt(log(1 + (s^2 / m^2)))))
  return(trnc)
}
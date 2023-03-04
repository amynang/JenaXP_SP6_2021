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


## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
score_spring_m <- function(metrics.long) {
  metric.vec <- c("total_phyto_biomass_chla_ratio",
                  "surface_chla",
                  "diatom_biomass",
                  "dinoflagellate_biomass",
                  "doc",
                  "pheophytin",
                  "prorocentrum_minimum_abundance",
                  "total_phyto_biomass")
  #----------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "spring", 
           salzone == "m",
           metric %in% metric.vec)
  #----------------------------------------------------------------------------
  if (any(!metric.vec %in% unique(metrics.sub$metric))) {
    stop(paste("score_spring_m requires the following metric(s):",
               paste(metric.vec[!metric.vec %in% unique(metrics.sub$metric)],
                     collapse = ", ")))
  }
  #----------------------------------------------------------------------------
  final.df <- metrics.sub %>% 
    mutate(score = case_when(
      # Jackie's code 69.52 and 45.04
      # Lacouture (2006) 69.5 and 45
      metric == "total_phyto_biomass_chla_ratio" & value < 45.04 ~ 1,
      metric == "total_phyto_biomass_chla_ratio" & value >= 45.04 & value <= 69.52 ~ 3,
      metric == "total_phyto_biomass_chla_ratio" & value > 69.52 ~ 5,
      metric == "surface_chla" & (value < 2.60 | value > 8.00) ~ 1,
      metric == "surface_chla" & ((value >= 2.60 & value <= 2.90) |(value >= 6.17 & value <= 8.00)) ~ 3,
      metric == "surface_chla" & value > 2.90 & value < 6.17 ~ 5,
      # Jackie's code 275.4
      # Lacouture (2006) 275
      metric == "diatom_biomass" & (value < 149 | value >= 2513) ~ 1,
      metric == "diatom_biomass" & value >= 149 & value <= 275.4 ~ 3,
      metric == "diatom_biomass" & value > 275.4 & value < 2513 ~ 5,
      # Jackie's code 28.3, 156.9, and 268.2
      # Lacouture (2006) 157 and 268
      metric == "dinoflagellate_biomass" & (value < 28.3 | value > 268.2) ~ 1,
      metric == "dinoflagellate_biomass" & value >= 156.9 & value <= 268.2 ~ 3,
      metric == "dinoflagellate_biomass" & value > 28.3 & value < 156.9 ~ 5,
      metric == "doc" & value > 3.17 ~ 1,
      metric == "doc" & value >= 2.84 & value <= 3.17 ~ 3,
      metric == "doc" & value < 2.84 ~ 5,
      metric == "pheophytin" & value > 1.03 ~ 1,
      metric == "pheophytin" & value >= 1.00 & value <= 1.03 ~ 3,
      metric == "pheophytin" & value < 1.00 ~ 5,
      metric == "prorocentrum_minimum_abundance" & value > 1477600 ~ 1,
      metric == "prorocentrum_minimum_abundance" & value <= 1477600 ~ as.numeric(NA),
      metric == "total_phyto_biomass"  & value > 1150 ~ 1,
      metric == "total_phyto_biomass"  & value <= 1150 ~ as.numeric(NA),
      TRUE ~ as.numeric(NA)))
  return(final.df)
}


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
score_spring_p <- function(metrics.long) {
  metric.vec <- c("total_phyto_biomass_chla_ratio",
                  "surface_chla",
                  "pct_cryptophyte",
                  "doc",
                  "pheophytin",
                  "prorocentrum_minimum_abundance",
                  "total_phyto_biomass")
  #----------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "spring", 
           salzone == "p",
           metric %in% metric.vec)
  #----------------------------------------------------------------------------
  if (any(!metric.vec %in% unique(metrics.sub$metric))) {
    stop(paste("score_spring_p requires the following metric(s):",
               paste(metric.vec[!metric.vec %in% unique(metrics.sub$metric)], collapse = ", ")))
  }
  #----------------------------------------------------------------------------
  final.df <- metrics.sub %>% 
    mutate(score = case_when(
      metric == "total_phyto_biomass_chla_ratio" & value < 71.0 ~ 1,
      metric == "total_phyto_biomass_chla_ratio" & value >= 71.0 & value <= 107.5 ~ 3,
      metric == "total_phyto_biomass_chla_ratio" & value > 107.5 ~ 5,
      metric == "surface_chla" & value > 4.00 ~ 1,
      metric == "surface_chla" & value >= 2.80 & value <= 4.00 ~ 3,
      metric == "surface_chla" & value < 2.80 ~ 5,
      # Jackie's code 4.93 and 7.06
      # Lacouture (2006) 4.9 and 7.1
      metric == "pct_cryptophyte" & value > 7.06 ~ 1,
      metric == "pct_cryptophyte" & value >= 4.93 & value <= 7.06 ~ 3,
      metric == "pct_cryptophyte" & value < 4.93 ~ 5,
      metric == "doc" & value > 2.61 ~ 1,
      metric == "doc" & value >= 2.50 & value <= 2.61 ~ 3,
      metric == "doc" & value < 2.50 ~ 5,
      metric == "pheophytin" & value > 0.90 ~ 1,
      metric == "pheophytin" & value >= 0.55 & value <= 0.90 ~ 3,
      metric == "pheophytin" & value < 0.55 ~ 5,
      metric == "prorocentrum_minimum_abundance" & value > 7488 ~ 1,
      metric == "prorocentrum_minimum_abundance" & value >= 672 & value <= 7488 ~ 3,
      metric == "prorocentrum_minimum_abundance" & value < 672 ~ 5,
      # Jackie's code 1061.7
      # Lacouture (2006) 1062
      metric == "total_phyto_biomass"  & value > 1061.7 ~ 1,
      metric == "total_phyto_biomass"  & value <= 1061.7 ~ as.numeric(NA),
      TRUE ~ as.numeric(NA)))
  return(final.df)
}


## ------------------------------------------------------------------------
score_summer_m <- function(metrics.long, pico.warning) {
  metric.vec <- c("total_phyto_biomass_chla_ratio",
                  "surface_chla",
                  "dinoflagellate_biomass", 
                  "doc",
                  "pheophytin",
                  "total_phyto_biomass")
  pico.vec <- "picoplankton_abundance"
  #----------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "summer", 
           salzone == "m",
           metric %in% c(metric.vec, pico.vec))
  #----------------------------------------------------------------------------
  if (any(!metric.vec %in% unique(metrics.sub$metric))) {
    stop(paste("score_summer_m requires the following metric(s):",
               paste(metric.vec[!metric.vec %in% unique(metrics.sub$metric)], collapse = ", ")))
  }
  
  if (pico.warning == TRUE && any(!pico.vec %in% unique(metrics.sub$metric))) {
    warning("Warning: picoplankton_abundance missing from score_summer_m")
  }
  #----------------------------------------------------------------------------
  final.df <- metrics.sub %>% 
    mutate(score = case_when(
      metric == "total_phyto_biomass_chla_ratio" & value < 32.2 ~ 1,
      metric == "total_phyto_biomass_chla_ratio" & value >= 32.2 & value <= 36.9 ~ 3,
      metric == "total_phyto_biomass_chla_ratio" & value > 36.9 ~ 5,
      metric == "surface_chla" & value >= 9.74 ~ 1,
      metric == "surface_chla" & ((value >= 7.70 & value < 9.74) | value <= 4.00) ~ 3,
      metric == "surface_chla" & value > 4.00 & value < 7.70 ~ 5,
      # Jackie's code 200.92, 31.22, 55.98
      # Lacouture (2006) 201, 31, 56
      metric == "dinoflagellate_biomass" & (value <= 31.22 | value > 200.92) ~ 1,
      metric == "dinoflagellate_biomass" & value > 31.22 & value <= 55.98 ~ 3, 
      metric == "dinoflagellate_biomass" & value > 55.98 & value < 200.92 ~ 5,
      metric == "doc" & value > 3.35 ~ 1,
      metric == "doc" & value >= 2.99 & value <= 3.35 ~ 3,
      metric == "doc" & value < 2.99 ~ 5,
      metric == "pheophytin" & value > 1.60 ~ 1,
      metric == "pheophytin" & value >= 1.23 & value <= 1.60 ~ 3,
      metric == "pheophytin" & value < 1.23 ~ 5,
      # Jackie's code 598720000
      # Lacouture (2006) 598700000
      metric == "picoplankton_abundance" & value < 352000000 ~ 1,
      metric == "picoplankton_abundance" & value >= 352000000 & value <= 598720000 ~ 3,
      metric == "picoplankton_abundance" & value > 598720000 ~ 5,
      metric == "total_phyto_biomass"  & value > 660 ~ 1,
      metric == "total_phyto_biomass"  & value <= 660 ~ as.numeric(NA),
      TRUE ~ as.numeric(NA)))
  return(final.df)
}

## ------------------------------------------------------------------------
score_summer_p <- function(metrics.long, pico.warning) {
  
  metric.vec <- c("total_phyto_biomass_chla_ratio",
                  "surface_chla",
                  "pct_cryptophyte",
                  "diatom_biomass",
                  "dinoflagellate_biomass",
                  "doc",
                  "pheophytin",
                  "total_phyto_biomass")
  pico.vec <- "picoplankton_abundance"
  #----------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "summer", 
           salzone == "p",
           metric %in% c(metric.vec, pico.vec))
  #----------------------------------------------------------------------------
  if (any(!metric.vec %in% unique(metrics.sub$metric))) {
    stop(paste("score_summer_p requires the following metric(s):",
               paste(metric.vec[!metric.vec %in% unique(metrics.sub$metric)], collapse = ", ")))
  }
  
  if (pico.warning == TRUE && any(!pico.vec %in% unique(metrics.sub$metric))) {
    warning("Warning: picoplankton_abundance missing from score_summer_p")
  }
  #----------------------------------------------------------------------------
  final.df <- metrics.sub %>% 
    mutate(score = case_when(
      metric == "total_phyto_biomass_chla_ratio" & value < 37.7 ~ 1,
      metric == "total_phyto_biomass_chla_ratio" & value >= 37.7 & value <= 74.5 ~ 3,
      metric == "total_phyto_biomass_chla_ratio" & value > 74.5 ~ 5,
      metric == "surface_chla" & value > 5.33 ~ 1,
      metric == "surface_chla" & value >= 4.52 & value <= 5.33 ~ 3,
      metric == "surface_chla" & value < 4.52 ~ 5,
      metric == "pct_cryptophyte" & value > 6.5 ~ 1,
      metric == "pct_cryptophyte" & value >= 3.9 & value <= 6.5 ~ 3,
      metric == "pct_cryptophyte" & value < 3.9 ~ 5,
      metric == "diatom_biomass" & (value >= 799 | value < 137) ~ 1,
      metric == "diatom_biomass" & value >= 137 & value <= 181 ~ 3,
      metric == "diatom_biomass" & value > 181 & value < 799 ~ 5,
      metric == "dinoflagellate_biomass" & (value < 23 | value >= 544) ~ 1,
      metric == "dinoflagellate_biomass" & value >= 23 & value <= 37 ~ 3, 
      metric == "dinoflagellate_biomass" & value > 37 & value < 554 ~ 5,
      metric == "doc" & value > 2.80 ~ 1,
      metric == "doc" & value >= 2.58 & value <= 2.80 ~ 3,
      metric == "doc" & value < 2.58 ~ 5,
      metric == "pheophytin" & value > 1.50 ~ 1,
      metric == "pheophytin" & value >= 0.93 & value <= 1.50 ~ 3,
      metric == "pheophytin" & value < 0.93 ~ 5,
      metric == "picoplankton_abundance" & value < 208600000 ~ 1,
      metric == "picoplankton_abundance" & value >= 208600000 & value <= 269500000 ~ 3,
      metric == "picoplankton_abundance" & value > 269500000 ~ 5,
      # In Jackie's code 718
      # Lacouture (2006) 711
      metric == "total_phyto_biomass"  & (value < 181 | value > 831) ~ 1,
      metric == "total_phyto_biomass"  & ((value >= 181 & value <= 207) | (value >= 718 & value <= 831)) ~ 3,
      metric == "total_phyto_biomass"  & value > 207 & value < 718 ~ 5,
      TRUE ~ as.numeric(NA)))
  return(final.df)
}

## ------------------------------------------------------------------------
score_phyto <- function(metrics.long, pico.warning) {
  #----------------------------------------------------------------------------

  #spring.f <- score_spring_f(metrics.long)
  #spring.o <- score_spring_o(metrics.long)
  spring.m <- score_spring_m(metrics.long)
  spring.p <- score_spring_p(metrics.long)
  #----------------------------------------------------------------------------
  #summer.f <- score_summer_f(metrics.long)
  #summer.o <- score_summer_o(metrics.long)
  summer.m <- score_summer_m(metrics.long, pico.warning)
  summer.p <- score_summer_p(metrics.long, pico.warning)
  #----------------------------------------------------------------------------
  final.df <- bind_rows(#spring.f, spring.o, 
    spring.m, spring.p,
                        #summer.f, summer.o, 
    summer.m, summer.p) %>%
    select(-layer)
  #----------------------------------------------------------------------------
  return(final.df)
}

## ------------------------------------------------------------------------
scores.df <- metrics.long %>% 
  separate(unique_id, c("station", "layer", "samplenumber",
                        "sampledate", "season", "salzone"), sep = "_",
           remove = FALSE) %>% 
  rename(date = sampledate) %>% 
  score_phyto(pico.warning = FALSE) %>% 
  select(-samplenumber)

## ------------------------------------------------------------------------
scores.df <- scores.df %>% 
  group_by(unique_id) %>% 
  mutate(ibi_score = mean(score, na.rm = TRUE),
         metric_count = n(),
         present_count = sum(!is.na(score)),
         na_count = sum(is.na(score))) %>% 
  ungroup() %>% 
  filter(present_count >= 4)

## ------------------------------------------------------------------------
rate_phyto <- function(scores.df) {
  final.df <- scores.df %>% 
    mutate(
      ibi_score = round(ibi_score, 2),
      rating = case_when(
        ibi_score < 2 ~ "poor",
        ibi_score >= 2 & ibi_score < 2.67 ~ "fair_poor",
        ibi_score >= 2.67 & ibi_score < 3.33 ~ "fair",
        ibi_score >= 3.33 & ibi_score < 4 ~ "fair_good",
        ibi_score >= 4 ~ "good",
        TRUE ~ "ERROR"
      ))
  return(final.df)
}

## ------------------------------------------------------------------------
ratings.df <- rate_phyto(scores.df) %>% 
  mutate(date = as.Date(date))


## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------

  metric.vec <- c("total_phyto_biomass_chla_ratio",
                  "surface_chla",
                  "cyanophyte_biomass",
                  "doc",
                  "pheophytin",
                  "total_phyto_biomass")


## ------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "spring", 
           salzone == "f",
           metric %in% metric.vec)


## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
# test_station_tf.df <- phyto.df %>%
#   filter(str_detect(station, "tf"))
# 
# test_station_ret.df <- phyto.df %>%
#   filter(str_detect(station, "ret"))


## ------------------------------------------------------------------------
wq.raw <- data.table::fread(file.path(project.dir, "data/water_quality/cedr_wq.csv"),
                            data.table = FALSE,
                           na.strings = c("")) %>% 
  filter(is.na(problem),
         parameter %in% c("chla", "doc", "pheo"))

## ------------------------------------------------------------------------
stations.df <- wq.raw %>% 
  dplyr::select(station, agency, source, latitude, longitude) %>% 
  distinct() %>% 
  mutate(longitude = jitter(longitude, amount = 0.0005),
         latitude = jitter(latitude, amount = 0.0005))

leaflet(stations.df) %>% 
    addProviderTiles(providers$CartoDB.Positron,
                   options = leaflet::tileOptions(minZoom = 7, maxZoom = 18)) %>% 
  addCircleMarkers( ~longitude, ~latitude,
                    stroke = FALSE,
                    fillOpacity = 0.5,
                    popup = paste("Station:", stations.df$station, "<br/>",
                                  "Agency:", stations.df$agency, "<br/>",
                                  "Source:", stations.df$source, "<br/>",
                                  "Latitude:", stations.df$latitude, "<br/>",
                                  "Longitude:", stations.df$longitude)) %>% 
   leaflet::setMaxBounds(lng1 = -78, lat1 = 36, lng2 = -75, lat2 = 40.5) %>% 
  leaflet::setView(-76.4, lat = 38, zoom = 7) 

## ------------------------------------------------------------------------
wq.df <- wq.raw %>% 
  mutate(sampledate = as.Date(sampledate))


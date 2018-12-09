## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

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
wq1.df <- wq.raw %>% 
  mutate(sampledate = as.Date(sampledate))

## ------------------------------------------------------------------------
wq2.df <- wq1.df %>%
  mutate(pycnocline = case_when(
    is.na(upperpycnocline) ~ FALSE,
    upperpycnocline == 0 ~ FALSE,
    TRUE ~ TRUE
  ))

## ------------------------------------------------------------------------
issue.df <- wq2.df %>%
  select(monitoringstation, sampledate,
         pycnocline, upperpycnocline, lowerpycnocline) %>%
  distinct() %>%
  group_by(monitoringstation, sampledate) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count == 2,
         complete.cases(.)) %>%
  rename(up = upperpycnocline,
         lp = lowerpycnocline) %>%
  select(-pycnocline)


wq3.df <- left_join(wq2.df, issue.df, by = c("monitoringstation", "sampledate")) %>%
  mutate(pycnocline = if_else(is.na(count), pycnocline, TRUE),
         upperpycnocline = if_else(is.na(up), upperpycnocline, up),
         lowerpycnocline = if_else(is.na(lp), lowerpycnocline, lp)
         )

## ------------------------------------------------------------------------
wq4.df <- wq3.df %>% 
  select(station, source, sampledate, samplereplicatetype,
         depth, layer, 
         pycnocline, upperpycnocline, lowerpycnocline,
         #pdepth,
         parameter, measurevalue) %>% 
  distinct()

## ------------------------------------------------------------------------
wq5.df <- wq4.df %>% 
  filter(layer == "s", 
         parameter == "chla") %>% 
  unite(parameter, c("layer", "parameter"), remove = FALSE) %>% 
  bind_rows(wq4.df)

## ------------------------------------------------------------------------
avg_wq <- function(x) {
  final.df <- x %>% 
    group_by_at(vars(-measurevalue, -samplereplicatetype)) %>% 
    summarize(measurevalue = mean(measurevalue, na.rm = TRUE)) %>% 
    group_by_at(vars(-measurevalue, -layer)) %>%
    summarize(measurevalue = mean(measurevalue, na.rm = TRUE)) %>%
    group_by_at(vars(-measurevalue, -depth)) %>% 
    summarize(measurevalue = mean(measurevalue, na.rm = TRUE)) %>% 
    ungroup()
}

## ------------------------------------------------------------------------
# avg_wq_test <- function(data.df) {
#   final.df <- data.df %>%
#     mutate(new_var = mean(salinity, doc, chla, pheo))
#     
#   #return(final.df)
# }
#  
# test_wq.df <- avg_wq_test(wq.df) 

## ------------------------------------------------------------------------

  # case_when(
  #   sailinity_as_ppt >0 & sal <= 0.5 ~ "F",
  #   sal > .5 &  sal <= 5 ~ "O",
  #   sal > 5 & sal <= 18 ~ "M",
  #   sal > 18~ "P"
  # )

## ------------------------------------------------------------------------
pdepth.df <- events.df %>% 
  filter(layer %in% c("ap", "wc")) %>% 
  mutate(sampledate = as.Date(sampledate)) %>% 
  select(station, sampledate, pdepth) %>% 
  dplyr::distinct()

wq6.df <- left_join(wq5.df, pdepth.df, by = c("station", "sampledate"))
wq7.df <- wq6.df %>% 
  filter(depth <= pdepth) %>% 
  avg_wq() %>% 
  rename(date = sampledate)


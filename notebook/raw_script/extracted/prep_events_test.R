## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
events.df <- data.table::fread(file.path(project.dir, "data/phytoplankton2/VA_ODU_phyto_event.csv"#cedr_phyto_event.csv"
                                         ),
                            data.table = FALSE,
                            na.strings = "")

## ------------------------------------------------------------------------
events.df <- events.df %>% 
  mutate(sampledate = as.Date(sampledate)) 

## ------------------------------------------------------------------------
events_test.df <- events.df%>%

mutate(salzone = case_when(
    events.df$station %in% c("tf3.3","tf4.2","tf5.5")~ "f",
    #events_test.df$station == starts_with("cb")~ "good",
    events.df$station %in% c("ret3.1","ret4.3","ret3.1")~ "o",
    events.df$station %in% c("le3.6","le5.2","le5.4","le5.5-w")~ "p"
    

  )
  )


## ------------------------------------------------------------------------
events.df <- events.df %>%
  mutate(sampledate = as.Date(sampledate),
         salzone = case_when(
           salzone %in% c("tf", "fe") ~ "f",
           #salzone %in% c("f","tf", "fe") ~ "f",#L.V.:add back in and comment out first line as needed
           salzone %in% c("m", "me") ~ "m",
           salzone %in% c("o", "oe") ~ "o",
           salzone %in% c("p", "pe") ~ "p",
           TRUE ~ as.character(NA)
         ))

## ------------------------------------------------------------------------
events.sub <- events.df %>% 
  select(source, station, sampledate, layer, salzone) %>% 
  distinct()

## ------------------------------------------------------------------------
old.salzone <- readxl::read_excel(file.path(project.dir, "data/jackie_data/JMJ_PIBI_Salzone_Data.xlsx"),
                         sheet = "JMJ Salzone+Scores") %>% 
  clean_up() %>% 
  select(station, sample_date, ibi_salzone) %>% 
  mutate(sample_date = as.Date(sample_date)) %>% 
  rename(old_salzone = ibi_salzone,
         sampledate = sample_date) %>% 
  distinct()

## ------------------------------------------------------------------------
bay.df <- left_join(bay.df, events.sub, by = c("source", "station",
                                               "sampledate", "layer")) %>% 
  mutate(unique_id = paste(unique_id, season, salzone, sep = "_"))


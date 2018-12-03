## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
project.dir <- rprojroot::find_rstudio_root_file()

dir.create(file.path(project.dir, "data/phytoplankton2/"),
           recursive = TRUE, showWarnings = TRUE)


## ------------------------------------------------------------------------
url.root <- "http://datahub.chesapeakebay.net/api.JSON"
todays.date <- format(Sys.Date(), "%m-%d-%Y")

## ------------------------------------------------------------------------
station.vec <- file.path(url.root,
                       "LivingResources",
                       "TidalPlankton",
                       "Reported",
                       "1-01-1970",
                       todays.date,
                       "17",
                       "Station") %>% 
  fromJSON() %>% 
  pull(unique(MonitoringLocationId))

## ------------------------------------------------------------------------
phyto.df <- readxl::read_excel(file.path(project.dir, "data/Va_phyto_count_and_event/data-dev_2013_2016_ODU_Phyto_Reported_Data_05mar18.xlsx"))

phyto.df <- clean_up(phyto.df)

## ------------------------------------------------------------------------
test_phyto.df <- phyto.df %>%
  filter(station %in% c("cb6.4"))
    #"tf5.#", "tf4.#", "tf3.#", "ret5.#", "ret4.#", "ret3.#"))


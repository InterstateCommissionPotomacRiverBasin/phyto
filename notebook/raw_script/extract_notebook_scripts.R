# Extract -----------------------------------------------------------------
sections.path <- "notebook/sections"
appendix.path <- "notebook/sections/appendix"
extracted.path <- c("notebook/raw_script/extracted")

extract_code <- function(rmd.path, extracted.path) {
r.files.vec <- list.files(rmd.path)
r.files.vec <- r.files.vec[grepl(".Rmd", r.files.vec)]

purrr::map(r.files.vec, function(file.i) {
file.name <- gsub(".Rmd", "", file.i)
extracted.file <- paste0(file.name, ".R")
knitr::purl(file.path(rmd.path, file.i),
file.path(extracted.path, extracted.file))
})

}
extract_code(sections.path, extracted.path)
extract_code(appendix.path, extracted.path)


# Run ---------------------------------------------------------------------
run.itis.acquisition <- FALSE
evaluate <- TRUE
cache.me <- FALSE
extracted.path <- c("notebook/raw_script/extracted")
source.vec <- c(
"intro.R",
"data_input.R",
"info_output.R",
"data_acquisition_itis.R",
"prep_hierarchy.R",
"prep_carbon.R",
# #"prep_events.R",
"prep_events_test.R",
"prep_station.R",
"prep_wq.R",
# # "metric_calc.R",
"metric_calc_test.R",
#  #"scores_ratings.R",
"scores_ratings_test.R",
#  #"info_output.R"
"map_ratings.R"#,
# "validation_old_values.R",
# "validation_scoring_disagreement.R",
# "validation_metric_disagreement.R"
)

purrr::map(source.vec, function(source.i) {
source(file.path(extracted.path, source.i))
})

print("DONE")
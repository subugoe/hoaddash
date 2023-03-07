## Generate reports based on config list
library(dplyr)
library(purrr)
library(hoaddata)
library(ymlthis)
library(readr)

# Load agreements
my_ta <- readr::read_csv("config.csv")
# Get ISSNs
get_ta_jns <- function(publisher = NULL, collection = c("jct", "oam")) {
  
  match.arg(collection)
  
  if(collection == "jct") {
    issn_l <- hoaddata::jct_hybrid_jns |>
      dplyr::filter(esac_publisher == publisher) |>
      distinct(issn_l) |>
      pull(issn_l)
  } else {
    issn_l <- hoaddata::oam_hybrid_jns |>
      dplyr::filter(vertrag == publisher) |>
      distinct(issn_l) |>
      pull(issn_l)
  }
  tibble::tibble(issn_l = issn_l, publisher = publisher, collection = collection) |>
    # Create directory paths
    mutate(dir_name = paste0(collection, 
                             "/", 
                             tolower(gsub(" |[()]", "", publisher))))
}

ta_jns <- purrr::pmap_df(list(publisher = my_ta$publisher, collection = my_ta$collection), get_ta_jns) 

# Create directories
ta_dir_create <- function(ta_dirs = NULL) {
  lapply(ta_dirs, fs::dir_create)
}

# Create templates

ta_template_create <- function(.publisher = NULL) {
 my_df <- ta_jns |>
    dplyr::filter(publisher %in% .publisher) 
 # Template
 ymlthis::yml() |>
   ymlthis::yml_title(paste0(unique(my_df$publisher),
   ": Hybrid Journals in Transformative Agreements")) |>
   ymlthis::yml_subtitle(paste0("How open are ", unique(my_df$publisher), " hybrid journals in transformative agreements? This open source dashboard highlights the Open Access uptake in hybrid journals included in transformative agreements as listed by the cOAlition S Journal Checker Tool. You can analyse progress made over time by open license, publisher and country. You can also monitor the availability of publisher-provided metadata in Crossref.")) |>
   ymlthis::yml_params(issn_l = my_df$issn_l, 
                       publisher = unique(my_df$publisher),
                       collection = unique(my_df$publisher)) |>
   ymlthis::yml_discard(c("author", "date")) |>
   ymlthis::use_rmarkdown(path = paste0(unique(my_df$dir_name), "/index.qmd"),
                          template = "_template_index.qmd",
                          include_yaml = TRUE,
                          open_doc = FALSE,
                          overwrite = TRUE) 
}

# Worklfow

# Load agreements
my_ta <- readr::read_csv("config.csv")

# Get journals specific to TA collection
ta_jns <- purrr::pmap_df(list(publisher = my_ta$publisher, collection = my_ta$collection), get_ta_jns) 

# Create directories
ta_dir_create(unique(ta_jns$dir_name))

# Create templates
lapply(unique(ta_jns$publisher), ta_template_create)

# Move JCT overview

fs::file_copy("_template_overview.qmd", "jct/index.qmd", overwrite = TRUE)

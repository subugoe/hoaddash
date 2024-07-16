## Generate reports based on config list
library(dplyr)
library(purrr)
library(hoaddata)
library(ymlthis)
library(readr)
library(fs)

# Load agreements
my_ta <- readr::read_csv("config.csv") 
# Get ISSNs
get_ta_jns <- function(publisher = NULL, collection = c("jct", "oam")) {
  match.arg(collection)

  if (collection == "jct") {
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
    mutate(dir_name = paste0(
      collection,
      "/",
      tolower(gsub(" |[()]", "", publisher))
    ))
}

ta_jns <- purrr::pmap_df(list(publisher = my_ta$publisher, collection = my_ta$collection), get_ta_jns)

# Create directories
ta_dir_create <- function(ta_dirs = NULL) {
  lapply(ta_dirs, fs::dir_create)
}

# Copy funder logo
funding_cp <- function(ta_dirs = NULL) {
  lapply(ta_dirs, function(x) {
    fs::file_copy(c("funding_note.md", "dfg_logo.jpg", "sub_logo.svg"), x, overwrite = TRUE)
  })
}

# Create templates

ta_template_create <- function(.publisher = NULL, ...) {
  my_df <- ta_jns |>
    dplyr::filter(publisher %in% .publisher)
  # Title
  if (unique(my_df$collection) == "jct") {
    my_title <- paste0(
      unique(my_df$publisher),
      ": Hybrid Journals in Transformative Agreements"
    )
    my_subtitle <- paste0("How open are ", unique(my_df$publisher), "'s hybrid journals included in transformative agreements, as listed by the [cOAlition S Journal Checker Tool](https://journalcheckertool.org/transformative-agreements/)? You can analyse progress made over time by open license, publisher and country. You can also monitor the availability of publisher-provided metadata in Crossref.")
  } else {
    my_title <- unique(my_df$publisher)
    my_subtitle <- paste0("How open are hybrid journals included in the German consortial transformative agreement ", unique(my_df$publisher), "? You can analyse progress made over time by open license, publisher and country. You can also monitor the availability of publisher-provided metadata in Crossref.")
  }
  # Template
  ymlthis::yml() |>
    ymlthis::yml_title(my_title) |>
    ymlthis::yml_subtitle(my_subtitle) |>
    ymlthis::yml_params(
      issn_l = my_df$issn_l,
      publisher = unique(my_df$publisher),
      collection = unique(my_df$collection),
      download_path =  unique(my_df$dir_name)) |>
    ymlthis::yml_discard(c("author", "date")) |>
    ymlthis::use_rmarkdown(
      path = paste0(unique(my_df$dir_name), "/index.qmd"),
      template = "_portfolio_template.qmd",
      include_yaml = TRUE,
      open_doc = FALSE,
      overwrite = TRUE
    )
}

# Worklfow

# Load agreements
my_ta <- readr::read_csv("config.csv")

# Get journals specific to TA collection
ta_jns <- purrr::pmap_df(list(publisher = my_ta$publisher, collection = my_ta$collection), get_ta_jns)

# Create directories
ta_dir_create(unique(ta_jns$dir_name))

# Copy funding note
funding_cp(unique(ta_jns$dir_name))

# Create templates
# purrr::walk(my_ta$publisher, ta_template_create)

# Move JCT overview

fs::file_copy("_jct_overview.qmd", "index.qmd", overwrite = TRUE)

# OAM overview
fs::file_copy("_oam_overview.qmd", "oam/index.qmd", overwrite = TRUE)
funding_cp("oam/")

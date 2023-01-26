## Generate reports based on config list

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





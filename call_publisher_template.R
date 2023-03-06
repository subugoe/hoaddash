
render_pubs <- function(publisher = NULL) {
  
  if(!is.null(publisher)) {
    my_df <- hoaddata::jct_hybrid_jns |>
      filter(esac_publisher == publisher) |>
      distinct(issn_l)
  } else {
  my_df <- hoaddata::jct_hybrid_jns |>
    distinct(issn_l) 
  }
  
  dir_name <- tolower(gsub(" |[()]", "", publisher))
  fs::dir_create(paste0("publisher/", dir_name))
  ymlthis::yml() |>
    ymlthis::yml_params(issn_l = my_df$issn_l, publisher = publisher) |>
    ymlthis::yml_discard(c("author", "date")) |>
 # ymlthis::yml(list(params = list(issn_l = my_df$issn_l))) |>
    ymlthis::use_rmarkdown(path = paste0("publisher/", dir_name , "/index.qmd"),
                           template = "_template_index.qmd",
                           include_yaml = TRUE,
                           open_doc = FALSE) 
}

all <- hoaddata::jct_hybrid_jns |>
  distinct(issn_l)
oam <- hoaddata::jct_hybrid_jns |>
  inner_join(hoaddata::oam_hybrid_jns, by = "issn_l") |>
  filter(vertrag == "Wiley Hybrid (DEAL)") |>
  distinct(issn_l) 

lapply(c("Springer Nature", "Elsevier", "Wiley"), render_pubs)

quarto::quarto_render("publisher/elsevier/index.qmd")

# data prep

# Patch missing esac_publisher for kar52025bibsam (Karger/BIBSAM) until fixed upstream in hoaddata
jct_hybrid_jns <- hoaddata::jct_hybrid_jns |>
  dplyr::mutate(esac_publisher = dplyr::if_else(
    esac_id == "kar52025bibsam" & is.na(esac_publisher),
    "Karger",
    esac_publisher
  ))

# jct_hybrid_jns holds one row per ISSN variant and agreement (~50 per
# journal). Only use journal-publisher pairs, so join on the
# deduplicated table to avoid a massive fan-out.
jct_pub_jns <- dplyr::distinct(jct_hybrid_jns, issn_l, esac_publisher)

summarize_pubs <- function(...) {
  pub_df_all <- summarise_oa_all(var_summary = ...)
  pub_df_de <- summarise_oa_de(var_summary = ...)

  my_df <- bind_rows(pub_df_all, pub_df_de)
  return(my_df)
}

summarise_oa_all <-
  function(.data = hoaddata::jn_ind,
           var_summary = NULL) {
    pub_df <- .data |>
      inner_join(jct_pub_jns, by = "issn_l", multiple = "all")
    pub_all <- pub_df |>
      distinct(across({{ var_summary }}), issn_l, cr_year, jn_all) |>
      group_by(across({{ var_summary }})) |>
      summarise(articles = sum(jn_all))

    pub_cc <- pub_df |>
      filter(!is.na(cc)) |>
      distinct(across({{ var_summary }}), issn_l, cr_year, cc, cc_total) |>
      group_by(across({{ var_summary }})) |>
      summarise(oa_articles = sum(cc_total))

    pub_df_all <- left_join(pub_all, pub_cc) |>
      ungroup() |>
      mutate_if(is.numeric, ~ replace(., is.na(.), 0)) |>
      mutate(collection = "global")
    return(pub_df_all)
  }

summarise_oa_de <-
  function(.data = hoaddata::jn_aff,
           var_summary = NULL) {
    pub_df_de <- .data |>
      filter(country_code == "DE") |>
      inner_join(jct_pub_jns, by = "issn_l", multiple = "all") |>
      mutate(cr_year = as.factor(cr_year))
    pub_all_de <- pub_df_de |>
      distinct(across({{ var_summary }}), issn_l, cr_year, articles_total) |>
      group_by(across({{ var_summary }})) |>
      summarise(articles = sum(articles_total))

    pub_cc_de <- pub_df_de |>
      filter(!is.na(cc)) |>
      distinct(
        across({{ var_summary }}),
        issn_l,
        cr_year,
        cc,
        articles_under_cc_variant
      ) |>
      group_by(across({{ var_summary }})) |>
      summarise(oa_articles = sum(articles_under_cc_variant))
    pub_de <- left_join(pub_all_de, pub_cc_de) |>
      mutate(collection = "de")
    return(pub_de)
  }

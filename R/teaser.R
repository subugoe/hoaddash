
#' ## Global Creative Commons prevalence
#'
#' Calculate CC license variants by year. Returns absulote and relative values.
cc_ind_global <- function(jn_ind_df = jn_ind_df) {
  cc_df <- jn_ind_df %>%
    group_by(cr_year, cc) %>%
    summarise(cc_total = sum(cc_total))
  all_df <- jn_ind_df %>%
    distinct(cr_year, issn_l, jn_all) %>%
    group_by(cr_year) %>%
    summarise(jn_all = sum(jn_all))
  inner_join(cc_df, all_df) %>%
    filter(!is.na(cc)) %>%
    mutate(prop = cc_total / jn_all) %>%
    # select(-jn_all) %>%
    ungroup() %>%
    # missing combinations
    tidyr::complete(cr_year, cc, fill = list(cc_total = 0, prop = 0)) %>%
    distinct() %>%
    ungroup()
}
#' Publications 2017 - 2024
all_publications <- cc_ind_global(jn_ind_df = jn_ind_df) |>
  distinct(cr_year, jn_all) |>
  pull(jn_all) |>
  sum(na.rm = TRUE)
#' Publications 2023
all_publications_2023 <-
  cc_ind_global(jn_ind_df = jn_ind_df) |>
  distinct(cr_year, jn_all) |>
  filter(cr_year == "2023") |>
  pull(jn_all) |>
  sum(na.rm = TRUE)
#' CC publications 2017 - 2022
cc_publications <- cc_ind_global(jn_ind_df = jn_ind_df) |>
  pull(cc_total) |>
  sum(na.rm = TRUE)
#' CC publications 2023
cc_publications_23 <- cc_ind_global(jn_ind_df = jn_ind_df) |>
  filter(cr_year == "2023") |>
  pull(cc_total) |>
  sum(na.rm = TRUE)

#' ## Germany Creative Commons prevalence
#'
#' Calculate CC license variants by year. Returns absulote and relative values.
cc_ind_de <- function(jn_aff_df = jn_aff_df) {
  de_df <- jn_aff_df |>
    filter(country_code == "DE") |>
    tidyr::complete(cr_year, cc) |>
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  de_total <- de_df |>
    distinct(cr_year, issn_l, articles_total) %>%
    group_by(cr_year) %>%
    summarize(jn_all = sum(articles_total))
  de_cc <- de_df |>
    filter(!is.na(cc)) |>
    distinct(cr_year, issn_l, articles_under_cc_variant, cc) |>
    group_by(cr_year, cc) |>
    summarize(cc_total = sum(articles_under_cc_variant))
  left_join(de_total, de_cc, by = c("cr_year"), multiple = "all") |>
    mutate(cr_year = factor(cr_year)) %>%
    mutate(prop = cc_total / jn_all) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
}

#' Publications 2017 - 2024
all_publications_de <- cc_ind_de(jn_aff_df = jn_aff_df) |>
  distinct(cr_year, jn_all) |>
  pull(jn_all) |>
  sum(na.rm = TRUE)
#' Publications 2023
all_publications_2023_de <-
  cc_ind_de(jn_aff_df = jn_aff_df) |>
  distinct(cr_year, jn_all) |>
  filter(cr_year == "2023") |>
  pull(jn_all) |>
  sum(na.rm = TRUE)
#' CC publications 2017 - 2023
cc_publications_de <- cc_ind_de(jn_aff_df = jn_aff_df) |>
  pull(cc_total) |>
  sum(na.rm = TRUE)
#' CC publications 2023
cc_publications_23_de <- cc_ind_de(jn_aff_df = jn_aff_df) |>
  filter(cr_year == "2023") |>
  pull(cc_total) |>
  sum(na.rm = TRUE)


## Indicator display
basic_stat <- function(...) {
  # Number format helper
  number_format <- tibble::tibble(
    cc_publications = cc_publications,
    cc_publications_de = cc_publications_de,
    all_publications_de = all_publications_de,
    all_publications = all_publications,
    cc_publications_23 = cc_publications_23,
    all_publications_2023 = all_publications_2023,
    cc_publications_23_de = cc_publications_23_de,
    all_publications_2023_de = all_publications_2023_de
  ) |>
    mutate(across(everything(), ~ case_when(
    . < 1000 ~ as.character(.),
    . < 1000000 ~ paste0(format(round(. / 1e3, 0), trim = TRUE), "K"),
    . >= 1000000 ~ paste0(format(round(. / 1e6, 2), trim = TRUE), "M"))))


  glue::glue('<div class="grid">
 <div class="g-col-lg-6 g-col-12">
 <p style="text-align: center;" class="text-muted">2017-2024</p>
  <table style="margin:auto;padding:0;width:80%;text-align: center;">
  <colgroup>
       <col span="1" style="width: 50%;">
       <col span="1" style="width: 50%;">
      </colgroup>
    <tbody>
      <tbody>
         <tr >
            <td colspan="1"  scope="row" style="border-top: 1px solid rgb(255, 255, 255); font-size: 100%;">Global</td>
            <td colspan="1"  style="border-top: 1px solid rgb(255, 255, 255); font-size: 100%; border-left: 1px solid rgb(26, 55, 113);">Germany*</td>
         </tr>
          <tr >
            <td colspan="1"  scope="row" style="font-size: 157%;">{round(cc_publications / all_publications * 100, 1)}%</td>
            <td colspan="1"  style="font-size: 157%; border-left: 1px solid rgb(26, 55, 113);">{round(cc_publications_de / all_publications_de * 100, 1)}%</td>
         </tr>
         <tr >
            <td colspan="1"  scope="row" style="border-top: 1px solid rgb(255, 255, 255); color: rgb(102, 102, 102); font-size: 91%;">{number_format$cc_publications} out of {number_format$all_publications}</td>
            <td colspan="1"  style="border-top: 1px solid rgb(255, 255, 255); color: rgb(102, 102, 102); font-size: 91%; border-left: 1px solid rgb(26, 55, 113);">{number_format$cc_publications_de} out of {number_format$all_publications_de}</td>
         </tr>
      </tbody>
   </table>
</div>
 <div class="g-col-lg-6 g-col-12">
 <p style="text-align: center;" class="text-muted">2023</p>
 <table style="margin:auto;padding:0;width:80%;text-align: center;">
  <colgroup>
       <col span="1" style="width: 50%;">
       <col span="1" style="width: 50%;">
      </colgroup>
    <tbody>
      <tbody>
         <tr >
            <td colspan="1"  scope="row" style="border-top: 1px solid rgb(255, 255, 255); font-size: 100%;">Global</td>
            <td colspan="1"  style="border-top: 1px solid rgb(255, 255, 255); font-size: 100%; border-left: 1px solid rgb(26, 55, 113);">Germany*</td>
         </tr>
         <tr >
            <td colspan="1"  scope="row" style="font-size: 157%;">{round(cc_publications_23 / all_publications_2023 * 100, 1)}%</td>
            <td colspan="1"  style="font-size: 157%; border-left: 1px solid rgb(26, 55, 113);">{round(cc_publications_23_de / all_publications_2023_de * 100, 1)}%</td>
         </tr>
         <tr >
            <td colspan="1"  scope="row" style="border-top: 1px solid rgb(255, 255, 255); color: rgb(102, 102, 102); font-size: 91%;">{number_format$cc_publications_23} out of {number_format$all_publications_2023}</td>
            <td colspan="1"  style="border-top: 1px solid rgb(255, 255, 255); color: rgb(102, 102, 102); font-size: 91%; border-left: 1px solid rgb(26, 55, 113);">{number_format$cc_publications_23_de} out of {number_format$all_publications_2023_de}</td>
         </tr>
      </tbody>
   </table>
</div>
</div>')
}
  

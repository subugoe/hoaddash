cr_upw_data <- function(cr_upw = hoaddata::cr_upw, my_cat = "Global", ...) {
  cr_upw |>
    dplyr::filter(issn_l %in% params$issn_l) |>
    dplyr::filter(cat == my_cat) |>
    group_by(cr_year, cat) |>
    summarise(article_total = sum(article_total),
              `Crossref` = sum(cr_hybrid_total),
              `Unpaywall` = sum(upw_hybrid_total)) |>
    tidyr::pivot_longer(cols = c(`Crossref`, `Unpaywall`))
}

cr_upw_plot <- function(cr_upw_data = hoaddata::cr_upw, ...) {
  plot_df <- cr_upw_data(cr_upw_data, ...)
  my_plot <- ggplot(plot_df, aes(cr_year, value / article_total, fill = name, 
             tooltip = glue::glue('<small>{name}</small><br><b>{format(value, big.mark = ",")} / {round((value / article_total) * 100, 1)}%</b><br><small>articles with CC license in {cr_year}</small>'))) +
    geom_bar_interactive(position = position_dodge2(preserve = "single"), stat = "identity") +
    scale_fill_manual("Data source", values = c("Crossref" = "#fc5185",
                                                "Unpaywall" = "#4d4d4d")) +
    theme_minimal(base_family = "Source Sans Pro", base_size = 20) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    labs(y = NULL, x = NULL) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.ontop = FALSE,
          legend.position="top", legend.justification = "right")
  
  ggiraph::girafe(
    ggobj = my_plot,
    width_svg = 8,
    height_svg = 7 * 0.618,
    options = list(opts_tooltip(
      css="background-color:white;
;font-size:1.15em;padding:10px;border-radius:5px;box-shadow: 0 0 10px 0 rgba(0, 0, 0, 0.5)",
      opacity = .95)))
}

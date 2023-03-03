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

## React table is used when we want to highlight gaps across publishers
upw_cr_react <- function(.data = shared_upw_cr_diff_year) {
reactable::reactable(
  shared_upw_cr_diff_year,
  pagination = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(vAlign = "center", headerClass = "header"),
  defaultSortOrder = "desc",
  compact = TRUE,
  columns = list(
    # Hide
    cr_year = colDef(show = FALSE),
    # Publisher
    esac_publisher = colDef(
      "Publisher",
      minWidth = 180,
      align = "left",
      sticky = "left",
      class = "label"
    ),
    perc_cr = colDef(
      "Crossref % OA",
      cell = function(value) {
        value <- paste0(format(round(value * 100, 1), nsmall = 1), "%")
        value <- format(value, width = 5, justify = "right")
        react_bar_chart(value,
                        width = value,
                        fill = "#fc5185",
                        background = "transparent")
      },
      align = "center",
      style = list(whiteSpace = "pre"),
      class = "number",
      html = TRUE,
      minWidth = 100
    ),
    perc_upw = colDef(
      "Unpaywall % OA",
      cell = function(value) {
        value <- paste0(format(round(value * 100, 1), nsmall = 1), "%")
        value <- format(value, width = 5, justify = "right")
        react_bar_chart(value,
                        width = value,
                        fill = "#4d4d4d",
                        background = "#transparent")
      },
      align = "center",
      style = list(whiteSpace = "pre"),
      class = "number",
      html = TRUE,
      minWidth = 100
    ),
    perc_diff  = colDef(
      name = "% Difference",
      format = colFormat(percent = TRUE, digits = 1),
      class = "number",
      align = "right",
      width = 120,
      style = function(value) {
        if (value > 0) {
          color <- "#fc5185"
        } else if (value < 0) {
          color <- "#4d4d4d"
        } else {
          color <- "#777"
        }
        list(color = color, fontWeight = "bold")
      }
    ),
    cat = colDef(show = FALSE)
  ),
  searchable = FALSE,
  defaultPageSize = 8,
  language = reactableLang(
    searchPlaceholder = "SEARCH",
    noData = "No publisher found",
    pageInfo = "{rowStart}\u2013{rowEnd} of {rows} publisher portfolios",
    pagePrevious = "\u276e",
    pageNext = "\u276f"
  )
)
}
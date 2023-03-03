# Journal table

# Venue info
venues <- hoaddata::jct_oalex_venues
venues_df <- venues |>
  mutate(journal_name = case_when(
    !is.na(homepage_url) ~ glue::glue('{display_name}<a href="{homepage_url}" target="_blank"><sup><small><i class="bi bi-box-arrow-up-right"></i></small></sup></a>'),
    is.na(homepage_url) ~ display_name)
  ) |>
  mutate(issn_link = glue::glue('ISSN-L: <a href="https://zdb-katalog.de/list.xhtml?t={issn_l}&key=iss&frm=O&asc=false" target="_blank">{issn_l}</a> | OpenAlex: <a href="{id}" target="_blank">{gsub("https://openalex.org/", "", id)}</a>')) |>
  mutate(journal_display = glue::glue('<div><div style="font-weight:bold">{journal_name}</div><div style="font-size: 0.7rem">{issn_link}</div></div>'))

# Sparkline
trend_df <- my_df |>
  mutate(prop = oa_articles / articles) |>
  mutate(prop = replace(prop, is.nan(prop), 0)) |>
  group_by(issn_l, collection) |>
  mutate(across(contains("prop"), ~ round(., 3) * 100)) |>
  mutate(cc_trend = list(prop)) |> 
  distinct(issn_l, collection, cc_trend)

# Add to table
jn_table_df_react_ <- my_df |>
  group_by(issn_l, collection) |>
  summarise(total = sum(articles),
            cc_total = sum(oa_articles)) |>
  mutate(prop = cc_total / total) |>
  mutate(prop = replace(prop, is.nan(prop), 0)) |>
  left_join(venues_df, by = "issn_l") |>
  left_join(trend_df, by = c("issn_l", "collection")) |>
  ungroup() |> 
  pivot_wider(names_from = collection, values_from = c(total, cc_total, prop, cc_trend)) |>
  select(journal_display, total_global, prop_global, cc_trend_global, total_de, prop_de, cc_trend_de) |>
  mutate(across(contains("prop"), ~ round(., 3) * 100))

  
# table donuts
# donut
# https://glin.github.io/reactable/articles/popular-movies/popular-movies.html
get_global_color <- function(score) {
  global_pal <- function(x) rgb(colorRamp(c("#DAF7F4", "#00C7B3"))(x), maxColorValue = 255)
  normalized <- (score - min(score)) / (max(score) - min(score))
  global_pal(normalized)
}
get_germany_color <- function(score) {
  germany_pal <- function(x) rgb(colorRamp(c("#DAEFF7", "#0093C7"))(x), maxColorValue = 255)
  normalized <- (score - min(score)) / (max(score) - min(score))
  germany_pal(normalized)
}
jn_table_df_react <- jn_table_df_react_ |>
  #head() |>
  mutate(global_color = get_global_color(prop_global),
         germany_color = get_germany_color(prop_de)) |>
  mutate(all_trend_spark = NA,
         de_trend_spark = NA) |>
  relocate(all_trend_spark, .before = total_de)

## Table
journal_listing <- reactable::reactable(
  jn_table_df_react,
  pagination = TRUE,
  searchable = TRUE,
  compact = TRUE,
  defaultColDef = colDef(vAlign = "center", headerClass = "header"),
  defaultSorted = "journal_display",
  defaultSortOrder = "desc",
  columns = list(
    journal_display = colDef(
      "Journal",
      defaultSortOrder = "asc",
      html = TRUE,
      minWidth = 250
    ),
    total_global = colDef(
      "Articles",
      format = colFormat(separators = TRUE, locales = "en-GB"),
      width = 90,
      style = list(whiteSpace = "pre"),
      class = "number border-left",
    ),
    prop_global = colDef(
      "% OA",
      cell = JS(
        "function(cellInfo) {
        const sliceColor = cellInfo.row['global_color']
        const sliceLength = 2 * Math.PI * 24
        const sliceOffset = sliceLength * (1 - cellInfo.value / 100)
        const donutChart = (
          '<svg width=60 height=60 style=\"transform: rotate(-90deg)\" focusable=false>' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=4 stroke=rgba(0,0,0,0.1)></circle>' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=4 stroke=' + sliceColor +
            ' stroke-dasharray=' + sliceLength + ' stroke-dashoffset=' + sliceOffset + '></circle>' +
          '</svg>'
        )
        const label = '<div style=\"position: absolute; top: 50%; left: 50%; ' +
          'transform: translate(-50%, -50%)\">' + cellInfo.value + '%' + '</div>'
        return '<div style=\"display: inline-flex; position: relative\">' + donutChart + label + '</div>'
      }"
      ),
      html = TRUE,
      align = "center",
      width = 130
    ),
    total_de = colDef(
      "Articles",
      format = colFormat(separators = TRUE, locales = "en-GB"),
      width = 90,
      style = list(whiteSpace = "pre"),
      class = "number border-left",
    ),
    prop_de = colDef(
      "% OA",
      cell = JS(
        "function(cellInfo) {
        const sliceColor = cellInfo.row['germany_color']
        const sliceLength = 2 * Math.PI * 24
        const sliceOffset = sliceLength * (1 - cellInfo.value / 100)
        const donutChart = (
          '<svg width=60 height=60 style=\"transform: rotate(-90deg)\" focusable=false>' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=4 stroke=rgba(0,0,0,0.1)></circle>' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=4 stroke=' + sliceColor +
            ' stroke-dasharray=' + sliceLength + ' stroke-dashoffset=' + sliceOffset + '></circle>' +
          '</svg>'
        )
        const label = '<div style=\"position: absolute; top: 50%; left: 50%; ' +
          'transform: translate(-50%, -50%)\">' + cellInfo.value + '%' + '</div>'
        return '<div style=\"display: inline-flex; position: relative\">' + donutChart + label + '</div>'
      }"
      ),
      html = TRUE,
      align = "center",
      width = 130
    ),
    all_trend_spark = colDef(
      "Trend",
      cell = function(value, index) {
        sparkline(
          jn_table_df_react$cc_trend_global[[index]],
          lineColor = "#00C7B3",
          fillColor = "transparent",
          highlightSpotColor = "#00C7B3",
          minSpotColor = FALSE,
          maxSpotColor = FALSE,
          spotColor = "#00C7B3",
          lineWidth = 2.5,
          spotRadius = 4,
          width = 90
        )
      },
      minWidth = 100,
      align = "center"
    ),
    de_trend_spark = colDef(
      "Trend",
      cell = function(value, index) {
        sparkline(
          jn_table_df_react$cc_trend_de[[index]],
          lineColor = "#0093c7",
          fillColor = "transparent",
          highlightSpotColor = "#0093c7",
          minSpotColor = FALSE,
          maxSpotColor = FALSE,
          spotColor = "#0093c7",
          lineWidth = 2.5,
          spotRadius = 4,
          width = 90,
          chartRangeMin = 0,
          chartRangeMax = 10
        )
      },
      minWidth = 100,
      align = "center"
    ),
#    issn_link = colDef(show = FALSE),
    cc_trend_global = colDef(show = FALSE),
    cc_trend_de = colDef(show = FALSE),
    global_color = colDef(show = FALSE),
    germany_color = colDef(show = FALSE)
  ),
  # Create column group
  columnGroups = list(
    colGroup(
      name = "Global Publication Volume",
      columns = c("total_global", "prop_global", "all_trend_spark"),
      headerClass = "group-header"
    ),
    colGroup(
      name = "German Lead Authors",
      columns = c("total_de", "prop_de", "de_trend_spark"),
      headerClass = "group-header"
    )
  ),
  defaultPageSize = 8,
  language = reactableLang(
    pageInfo = "{rowStart}\u2013{rowEnd} of {rows} hybrid journals",
    pagePrevious = "\u276e",
    pageNext = "\u276f"
  )
)

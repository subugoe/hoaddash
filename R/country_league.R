# Country market share table
## Publisher-specific tables

# Article set
country_df

# Max values to define width of bar charts
bar_max_width <- country_df %>%
  group_by(cr_year)  %>%
  summarize(max_group = max(cc_total),
            max_all_group = max(articles)) |>
  ungroup() |>
  # Deal with periods where no CC licenses where found
  mutate(max_group = ifelse(max_group == 0, 1, max_group))

country_pub_df <-
  country_df %>%
  left_join(bar_max_width, by = "cr_year") |>
  # Remove missing country information
  filter(!is.na(country_name)) |>
  select(country_name,
         cc_total,
         articles,
         prop,
         max_group,
         max_all_group)

shared_oa_country_df <- SharedData$new(country_pub_df)

react_table_oa_country <- reactable::reactable(
  shared_oa_country_df,
  style = list(),
  defaultColDef = colDef(headerClass = "header"),
  defaultSorted = "cc_total",
  defaultSortOrder = "desc",
  # Highlight Germany
  rowStyle = JS(
    "function(rowInfo) {
    if (rowInfo.row['country_name'] == 'Germany') {
      return { background: 'rgba(234, 226, 216)' }
    }
  }"
  ),
  # Define columns
  columns = list(
    country_name = colDef(html = TRUE,
                          name = "Country",
                          width = 200),
    cc_total = colDef(
      #  html = TRUE,
      name = "OA with CC",
      align = "left",
      style = list(whiteSpace = "pre"),
      cell = function(value, index) {
        ### define the bar width according to the specified value
        width <-
          paste0(value * 100 / shared_oa_country_df$data()$max_group[index], "%")
        ### add a comma to the label
        value <- format(value, big.mark = ",")
        ### justify and provide padding with width
        value <- format(value, width = 10, justify = "right")
        ### create the barchart div
        bar <- div(
          ### with a class of 'bar-chart'
          class = "bar-chart",
          ### give the bar a margin
          style = list(marginRight = "6px"),
          div(
            class = "bar",
            style = list(width = width, backgroundColor = "#fc5185")
          )
        )
        ### bring it all together, with the 'value' (number) preceding the bar itself
        div(class = "bar-cell", span(class = "number", value), bar)
      }
    ),
    articles = colDef(
      #  html = TRUE,
      name = "Total Article Volume",
      align = "left",
      style = list(whiteSpace = "pre"),
      cell = function(value, index) {
        ### define the bar width according to the specified value
        width <-
          paste0(value * 100 / shared_oa_country_df$data()$max_all_group[index], "%")
        ### add a comma to the label
        value <- format(value, big.mark = ",")
        value <- format(value, width = 10, justify = "right")
        bar <- div(
          class = "bar-chart",
          style = list(marginRight = "6px"),
          div(
            class = "bar",
            style = list(width = width, backgroundColor = "#c3c3c3")
          )
        )
        ### bring it all together, with the 'value' (number) preceding the bar itself
        div(class = "bar-cell", span(class = "number", value), bar)
      }
    ),
    prop = colDef(
      name = "% of total",
      format = colFormat(percent = TRUE, digits = 1),
      style = list(fontWeight = "bold"),
      class = "number",
      align = "right",
      width = 90
    ),
    # Don't show columns that are used for filtering or calculations
    cr_year = colDef(show = FALSE),
    max_all_group = colDef(show = FALSE),
    max_group = colDef(show = FALSE)
  ),
  compact = TRUE,
  language = reactableLang(
    searchPlaceholder = "SEARCH COUNTRIES",
    noData = "No country found",
    pageInfo = "{rowStart}\u2013{rowEnd} of {rows} countries",
    pagePrevious = "\u276e",
    pageNext = "\u276f",
  )
)

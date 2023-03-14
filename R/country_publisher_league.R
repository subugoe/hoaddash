if(exists("oam")) {
  esac_df <- oam
} else {
esac_df <- hoaddata::jct_hybrid_jns |>
  distinct(issn_l, esac_publisher)
}

country_total <- jn_aff_df |>
  distinct(issn_l, cr_year, country_code, articles_total) |>
  inner_join(esac_df, by = "issn_l") |>
  group_by(cr_year, country_code, esac_publisher) |>
  summarise(articles = sum(articles_total)) 

country_cc <- jn_aff_df  |>
  filter(!is.na(cc)) |>
  distinct(issn_l, cr_year, articles_under_cc_variant, country_code, cc) |>
  inner_join(esac_df, by = "issn_l") |>
  group_by(cr_year, country_code, esac_publisher) |>
  summarize(cc_total = sum(articles_under_cc_variant))

country_df_by_publisher <- left_join(country_total, country_cc, by = c("cr_year", "country_code", "esac_publisher")) |>
  mutate(cr_year = factor(cr_year)) |>
  mutate(prop = cc_total / articles) |>
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |>
  filter(!is.na(country_code)) |>
  mutate(
    country_name = 
    countrycode::countrycode(
      country_code, origin = "iso2c", destination = "country.name"
      )
    ) |>
    mutate(country_name = gsub("&", "and", country_name)) |>
    mutate(esac_publisher = forcats::fct_relevel(
      forcats::as_factor(esac_publisher), publ_league)
      )


country_pub_league <- country_df |>
  mutate(esac_publisher = "All") |>
  bind_rows(country_df_by_publisher) |>
  mutate(esac_publisher = forcats::fct_relevel(forcats::as_factor(esac_publisher), c("All", publ_league)))

# Max values to define width of bar charts
bar_max_width <- country_pub_league |>
  group_by(cr_year, esac_publisher)  |>
  summarize(max_group = max(cc_total),
            max_all_group = max(articles)) |>
  ungroup() |>
  # Deal with periods where no CC licenses where found
  mutate(max_group = ifelse(max_group == 0, 1, max_group))

country_pub_df <-
  country_pub_league |>
  left_join(bar_max_width, by = c("cr_year", "esac_publisher")) |>
  # Remove missing country information
  filter(!is.na(country_name)) |>
  select(esac_publisher,
         cr_year,
         country_name,
         cc_total,
         articles,
         prop,
         max_group,
         max_all_group) |>
  ungroup()

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
      name = "% OA",
      format = colFormat(percent = TRUE, digits = 1),
      style = list(fontWeight = "bold"),
      class = "number",
      align = "right",
      width = 90
    ),
    # Don't show columns that are used for filtering or calculations
    cr_year = colDef(show = FALSE),
    max_all_group = colDef(show = FALSE),
    max_group = colDef(show = FALSE),
    esac_publisher = colDef(show = FALSE)
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

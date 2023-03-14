# Data prep
publisher_league_data_prep <- function() {
  jn_league_raw <- summarize_pubs(var_summary = c(cr_year, esac_publisher, issn_l)) |>
  tidyr::complete(cr_year, esac_publisher, collection) |>
  mutate_if(is.numeric, ~ replace(., is.na(.), 0))

journals_per_publisher <- jn_league_raw |>
  group_by(cr_year, esac_publisher, collection) |> 
  summarise(all_journals = n_distinct(issn_l, na.rm = TRUE))

journals_with_oa_per_publisher <- jn_league_raw |>
  filter(oa_articles != 0) |>
  group_by(cr_year, esac_publisher, collection) |> 
  summarise(oa_journals = n_distinct(issn_l, na.rm = TRUE))

jn_league_df <- left_join(journals_per_publisher, journals_with_oa_per_publisher, by = c("cr_year", "esac_publisher", "collection")) |>
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) |> 
  mutate(journal_prop = oa_journals / all_journals) |>
  ungroup() 

pub_league <- summarize_pubs(var_summary = c(cr_year, esac_publisher)) |>
  right_join(jn_league_df, by = c("cr_year", "esac_publisher", "collection")) |>
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) |> 
  mutate(oa_prop = oa_articles / articles) |>
  mutate(esac_publisher = forcats::fct_relevel(forcats::as_factor(esac_publisher), publ_league))


# Prepare bar width relative to Crosstalk selection
bar_max_width <- pub_league |>
  group_by(cr_year, collection)  %>%
  summarize(
    max_all_group = max(articles),
    max_cc_group = max(oa_articles, na.rm = TRUE),
    max_jn_group = max(all_journals)
  ) |>
  ungroup() 
table_df <- pub_league |>
  inner_join(bar_max_width, by = c("cr_year", "collection")) |>
  select(cr_year, 
         esac_publisher,
         collection,
         # Article-level info
         articles,
         oa_articles,
         oa_prop,
         # Journal-level info
         all_journals,
         oa_journals,
         journal_prop,
         # Bar chart config
         max_all_group,
         max_cc_group,
         max_jn_group) |>
  mutate(collection = case_when(
    collection == "global" ~ "Global",
    collection == "de" ~ "Germany"
  ))
return(table_df)
}
## Table


# Crosstalk interactivity
# Initialize shared Crosstalk data

publisher_league_reactable <- function(shared_publisher_df, ...) {

  # Columns

  reactable::reactable(
  shared_publisher_df,
  pagination = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(vAlign = "center", headerClass = "header"),
  defaultSorted = "articles",
  defaultSortOrder = "desc",
  compact = TRUE,
  columns = publisher_col(...),
  # Create column groups
  columnGroups = list(
    colGroup(
      name = "Article Volume",
      columns = c("articles", "oa_articles", "oa_prop"),
      headerClass = "group-header"
    ),
    colGroup(
      name = "Hybrid Journals",
      columns = c("oa_journals", "all_journals", "journal_prop"),
      headerClass = "group-header"
    )
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
  )}


my_cols <- function() {
  list(
    # Hide
    cr_year = colDef(show = FALSE),
    # Total Article Volume
    articles = colDef(
      "Total",
      format = colFormat(separators = TRUE, locales = "en-GB"),
      width = 150,
      style = list(whiteSpace = "pre"),
      class = "number border-left",
      cell = function(value, index) {
        width <-
          paste0(value * 100 / shared_publisher_df$data()$max_all_group[index], "%")
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
      },
      align = "center"
    ),
    # CC Article Volume
    oa_articles = colDef(
      "OA with CC",
      format = colFormat(separators = TRUE, locales = "en-GB"),
      width = 150,
      style = list(whiteSpace = "pre"),
      class = "number",
      cell = function(value, index) {
        width <-
          paste0(value * 100 / shared_publisher_df$data()$max_cc_group[index], "%")
        value <- format(value, big.mark = ",")
        value <- format(value, width = 10, justify = "right")
        bar <- div(
          class = "bar-chart",
          style = list(marginRight = "6px"),
          div(
            class = "bar",
            style = list(width = width, backgroundColor = "#fc5185")
          )
        )
        ### bring it all together, with the 'value' (number) preceding the bar itself
        div(class = "bar-cell", span(class = "number", value), bar)
      },
      align = "center"
    ),
    oa_prop = colDef(
      name = "% OA with CC",
      cell = function(value) {
        value <- paste0(format(round(value * 100, 1), nsmall = 1), "%")
        value <- format(value, width = 5, justify = "right")
        react_bar_chart(value,
                        width = value,
                        fill = "#fc5185",
                        background = "#c3c3c3")
      },
      align = "center",
      style = list(whiteSpace = "pre"),
      class = "number",
      html = TRUE,
      minWidth = 100
    ),
    # All Journals
    all_journals = colDef(
      "Total ",
      format = colFormat(separators = TRUE, locales = "en-GB"),
      width = 150,
      style = list(whiteSpace = "pre"),
      class = "number border-left",
      cell = function(value, index) {
        width <-
          paste0(value * 100 / shared_publisher_df$data()$max_jn_group[index], "%")
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
      },
      align = "center"
    ),
    # OA Journals
    oa_journals = colDef(
      show = FALSE
    ),
    # Percentage with OA
    journal_prop = colDef(
      "% ≥ 1 CC article",
      # Render the bar charts using a custom cell render function
      cell = function(value) {
        value <- paste0(format(round(value * 100, 0), nsmall = 0), "%")
        value <- format(value, width = 5, justify = "right")
        react_bar_chart(value,
                        width = value,
                        fill = "#3fc1c9",
                        background = "#c3c3c3")
      },
      align = "center",
      style = list(whiteSpace = "pre"),
      class = "number",
      html = TRUE,
      minWidth = 100
    ),
    max_cc_group = colDef(show = FALSE),
    max_all_group = colDef(show = FALSE),
    max_jn_group = colDef(show = FALSE),
    collection = colDef(show = FALSE)
  ) }

publisher_col <- function(.collection = NULL, ...) {
  my_cols <- my_cols()
  if(.collection == "jct") {
    esac_publisher = colDef(
      "Publisher",
      minWidth = 180,
      align = "left",
      sticky = "left",
      class = "label"
    )
    my_cols$esac_publisher <- esac_publisher
    } else {
       # Publisher
      agreement = colDef(
      "Agreement",
      cell = function(value, index) {
        lead <- shared_publisher_df$data()$lead[index]
        htmltools::tagList(htmltools::div(style = list(
          fontWeight = 600, color = "#333"
        ), value),
        htmltools::div(style = list(fontSize = 10), lead))
      },
      width = 150,
      align = "left",
      sticky = "left"
    )
    my_cols$agreement <- agreement
    my_cols$lead <- colDef(show = FALSE)
    my_cols$esac_publisher <- colDef(show = FALSE)
    }
    return(my_cols)
    }

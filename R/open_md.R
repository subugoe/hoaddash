# Data prep
open_md <- function(.data = NULL, issn = NULL, publisher = NULL, ...) {
 .data |>
    filter(issn_l %in% issn) |>
    mutate(esac_publisher = publisher) |>
    group_by(esac_publisher, cat, cr_year) |>
    rename(articles = article_total) |>
    summarise_at(vars(articles:ref_total), sum, na.rm = TRUE) |>
    mutate(across(contains("total"), function(x) x / articles)) |>
    select(-articles) |>
    pivot_longer(contains("total")) |>
    pivot_wider(names_from = cr_year, values_from = value) |>
    select(cat, name, contains("20")) |>
  # Human-readable labels
    mutate(name = case_when(
    name == "tdm_total" ~ "TDM Support",
    name == "orcid_total" ~ "ORCID",
    name == "funder_total" ~ "Funder info",
    name == "abstract_total" ~ "Open Abstracts",
    name == "ref_total" ~ "Open Citations"))
}

# Make table
# Initiate Crosstalk

shared_open_md_df <- function(.data = NULL, issn_l = NULL, publisher = NULL, ...) {
  open_md_df <- open_md(.data, issn = issn_l, publisher = publisher) |>
    relocate(esac_publisher)
  
  SharedData$new(open_md_df)
}

open_md_react <- function(...) {
  reactable(...,
            pagination = TRUE,
            highlight = TRUE,
            compact = TRUE,
            showPageSizeOptions = TRUE,
            sortable = FALSE,
            columns = list(
              esac_publisher = colDef("Publisher",
                                      # https://glin.github.io/reactable/articles/cookbook/cookbook.html#merge-cells
                                      style = JS("function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        if (!firstSorted || firstSorted.id === 'esac_publisher') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['esac_publisher'] === prevRow['esac_publisher']) {
            return { visibility: 'hidden' }
          }
        }
      }"),
                                      minWidth = 180),
              name = colDef("Metadata",
                            minWidth = 120),
              `2017` = indicator_column(name = "2017"),
              `2018` = indicator_column(name = "2018"),
              `2019` = indicator_column(name = "2019"),
              `2020` = indicator_column(name = "2020"),
              `2021` = indicator_column(name = "2021"),
              `2022` = indicator_column(name = "2022"),
              `2023` = indicator_column(name = "2023"),
              `2024` = indicator_column(name = "2024"),
              `2025` = indicator_column(name = "2025"),
              cat = colDef(show = FALSE)
            ))
  
}



# Table helper 
format_pct <- function(value) {
  if (is.na(value)) "  \u2013 "    # en dash for 0%
  # else if (value < 0.01) " <1%"
  # else if (value > 0.99) ">99%"
  else formatC(paste0(round(value * 100), "%"), width = 4)
}
# Color cells
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}
# http://vis4.net/palettes/#/9|s|00429d,96ffea,ffffe0|ffffe0,ff005e,93003a|1|1
my_pal <- c("#ffffe0","#ffdec7","#ffbcaf","#ff9895","#f4777f","#e4576b","#cf3759","#b41648","#93003a")
#my_pal <- c('#ffd4fa', '#e8cbf4', '#d1c3ee', '#babbe7', '#a2b3e1', '#88aadb', '#6ca2d5', '#4999ce', '#0091c8')

pct_color <- make_color_pal(my_pal, bias =2)
indicator_column <- function(maxWidth = 70, class = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = 70,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (is.na(value)) {
        list(color = "#333333")
      } else if (value < 0.2) {
        list(color = "#333333", background = pct_color(value))
      } else {
        list(color = "#fff", background = pct_color(value))
      }
    },
    ...
  )
}

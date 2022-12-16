# Publisher scatterplot

# Tooltip prep
## helper fxn
create_scatter_tooltip <-
  function(publisher = NULL,
           plot_df = my_df) {
    

    tooltip_tmp <- plot_df |>
      filter(esac_publisher == publisher) |>
      janitor::adorn_totals() |>
      mutate(oa_prop = round(oa_articles / articles * 100, 1)) |>
      mutate(oa_prop = paste0(oa_prop, "%")) |>
      select(
        `Year` = cr_year,
        `Total` = articles,
        `OA` = oa_articles,
        `% OA` = oa_prop
      )
    
      kableExtra::kbl(tooltip_tmp,
        align = "lrrr",
        format.args = list(big.mark
                           = ','),
        caption = glue::glue("<h4>{publisher}</h4>")
      ) |>
      kableExtra::kable_styling(html_font = "Source Sans Pro") |>
      kableExtra::row_spec(7, bold = TRUE) 
  }
## Call fxn
scatter_pub <- function(my_df = my_df) {
  tooltip_df <- my_df |>
    mutate(my_tooltip = map(esac_publisher, function(x)
      create_scatter_tooltip(x, my_df))) |>
    distinct(esac_publisher, my_tooltip)
  
  plot_df_all <- my_df |>
    group_by(esac_publisher, pub_col) |>
    summarise(articles = sum(articles),
              oa_articles = sum(oa_articles)) |>
    inner_join(tooltip_df, by = "esac_publisher")
  
  plot_all <-
    ggplot(plot_df_all,
           aes(articles, oa_articles / articles, color = pub_col, label = esac_publisher)) +
    geom_point_interactive(aes(size = oa_articles, tooltip = my_tooltip), alpha = .7) +
    scale_x_log10(
      labels = function(x)
        format(x, scientific = FALSE, big.mark = ","),
      
    ) +
    coord_cartesian(xlim = c(100, max(plot_df_all$articles))) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L))  +
    scale_size(
      "OA articles",
      labels = function(x)
        format(x, big.mark = ",", scientific = FALSE)
    ) +
    scale_color_identity() +
    geom_hline(
      aes(yintercept = median(oa_articles / articles)),
      colour = "#d55e00",
      linetype = "dashed",
      size = 1
    ) +
    geom_vline(
      aes(xintercept = median(articles)),
      colour = "#E69F00",
      linetype = "dashed",
      size = 1
    ) +
    theme_minimal(base_family = "Source Sans Pro") +
    labs(x = "Articles (log scale)", y = "OA") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.ontop = FALSE,
          legend.position="top", legend.justification = "right") 
}

plot_scatterplot <-
  function(my_df = my_df, collection = NULL) {
    my_col <- collection
    plot_all <- my_df |>
      filter(collection == my_col) |>
      scatter_pub()
    ggiraph::girafe(
      ggobj = plot_all,
      width_svg = 6,
      height_svg = 5 * 0.618,
      options = list(
        opts_tooltip(
          css = "background-color:white;
;font-size:1.15em;padding:10px;border-radius:5px;box-shadow: 0 0 10px 0 rgba(0, 0, 0, 0.5)",
          opacity = .95
        )
      )
    )
  }

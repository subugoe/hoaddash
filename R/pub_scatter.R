# Publisher scatterplot
scatter_pub <- function(my_df = my_df) {

  ### Benchmark lines (median)
  pub_df <- my_df |>
    group_by(cr_year) |>
    mutate(oa_prop = oa_articles / articles) |>
    summarise(median_articles = mean(articles, na.rm = TRUE),
              median_oa = mean(oa_prop, na.rm = TRUE)) |>
    inner_join(my_df, by = "cr_year")
  
  
  plot_all <-
    ggplot(pub_df,
           aes(articles, oa_articles / articles, frame = cr_year, color = pub_col,
               text = glue::glue(
             "<b>{esac_publisher}</b> in {cr_year}

       Articles: {format(articles, big.mark = ',')}
       OA Articles: {format(oa_articles, big.mark = ',')} ({round(oa_articles / articles * 100, 1)}%)
                  "))) +
    geom_point(aes(size = log(oa_articles)), alpha = .7) +
    scale_x_log10(
      labels = function(x)
        format(x, scientific = FALSE, big.mark = ","),
      
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L))  +
    scale_size(
      "OA articles",
      labels = function(x)
        format(x, big.mark = ",", scientific = FALSE)
    ) +
    coord_cartesian(xlim = c(100, max(pub_df$articles))) + 
    scale_color_identity(guide="none") +
    geom_hline(
      aes(yintercept = median_oa, frame = cr_year),
      colour = "#d55e00",
      linetype = "dashed",
      size = 1
    ) +
    geom_vline(
      aes(xintercept = median_articles, frame = cr_year),
      colour = "#E69F00",
      linetype = "dashed",
      size = 1
    ) +
    theme_minimal(base_family = "Atkinson Hyperlegible", base_size = 15) +
    labs(x = "Articles (log scale)", y = "OA") +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none")
  return(plot_all)
}

plot_scatterplot <-
  function(my_df = my_df, collection = NULL) {
    my_col <- collection
    plot_all <- my_df |>
      filter(collection == my_col) |>
      scatter_pub()
    plotly::ggplotly(plot_all, tooltip = c("text")) |>
      style(hoverlabel = list(bgcolor = "white", font = list(
        family = "Atkinson Hyperlegible",
        size = 18))) |>
      animation_slider(
        currentvalue = list(prefix = "Publication year: "),
        redraw = FALSE,
        pad = list(t = 50, r = 50)
      ) |>
      animation_button(visible = TRUE) |>
      config(displayModeBar = FALSE)
  }

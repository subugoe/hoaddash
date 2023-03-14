# Market shares

pub_shares <- my_df |>
  mutate(
    type = case_when(
      esac_publisher == "Springer Hybrid (DEAL)" ~ "Springer Hybrid (DEAL)",
      esac_publisher == "Wiley Hybrid (DEAL)" ~ "Wiley Hybrid (DEAL)",
      esac_publisher == "Wiley" ~ "Wiley",
      esac_publisher %in% top_13[3:13] ~ "Top 3-13",
      is.character(esac_publisher) ~ "Other"
    )
  ) |>
  mutate(type =
           forcats::fct_relevel(
             type,
             c("Springer Hybrid (DEAL)", "Wiley Hybrid (DEAL)", "Top 3-13", "Other")
           )) |>
  mutate(cat = forcats::fct_rev(as.factor(collection))) |>
  group_by(cat, type, esac_publisher) |>
  summarise(
    articles = sum(articles),
    oa_articles = sum(oa_articles),
    .groups = "drop"
  ) |>
  pivot_longer(cols = c(articles, oa_articles)) |>
  group_by(name, cat) |>
  mutate(prop = value / sum(value))

publ_league <- pub_shares |>
  filter(name == "articles", cat == "global") |>
  arrange(desc(value)) |>
  pull(esac_publisher)

pub_shares_plot <- pub_shares |>
  ungroup() |>
  mutate(esac_publisher = forcats::fct_relevel(esac_publisher, publ_league))

plot_pub_shares <-
  function(.data = pub_shares_plot, .cat = "global") {
    # filter by collection
    plot_df <- .data |>
      filter(cat == .cat) |>
      # tooltip
      mutate(
        my_tooltip = glue::glue(
          '<p>{esac_publisher}</p>
                               <p><big>{format(value, big.mark = ",")}</big> <span class="text-muted">Articles</span></p>
                               <p><big>{round(prop * 100, 1)}%</big><span class="text-muted"> <small>Market Share</small></span></p>'
        )
      )
    plot_df |>
      arrange(esac_publisher) |>
      mutate(
        name = if_else(
          name == "articles",
          "Total Articles",
          "Open Access Articles with Creative Commons License"
        )
      ) |>
      mutate(name = forcats::fct_inorder(name)) |>
      ggplot(aes(
        x = "",
        y = prop,
        group = type,
        fill = type
      )) +
      geom_bar_interactive(
        width = 1,
        stat = "identity",
        position = position_stack(reverse = TRUE),
        color = "white",
        aes(tooltip = my_tooltip)
      ) +
      coord_flip() +
      scale_fill_manual(
        "",
        values = c(
          "Springer Hybrid (DEAL)" = "#486a7e",
          "Wiley Hybrid (DEAL)" = "#068853",
          "Top 3-13" = "grey60",
          "Other" = "grey90"
        ),
        guide = guide_legend(reverse = FALSE,  nrow = 1)
      ) +
      facet_wrap(~ name, ncol = 1) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                         labels = scales::percent_format(accuracy = 5L)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_family = "Atkinson Hyperlegible", base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(panel.grid.major.y = element_blank()) +
      theme(panel.border = element_blank()) +
      theme(strip.text.x = element_text(hjust = -0.01)) +
      theme(
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text(size = 10)
      )
  }

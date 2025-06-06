country_total <- jn_aff_df |>
  distinct(issn_l, cr_year, country_code, articles_total) |>
  group_by(cr_year, country_code) |>
  summarise(articles = sum(articles_total))

country_cc <- jn_aff_df |>
  filter(!is.na(cc)) |>
  distinct(issn_l, cr_year, articles_under_cc_variant, country_code, cc) |>
  group_by(cr_year, country_code) |>
  summarize(cc_total = sum(articles_under_cc_variant))

country_df <- left_join(country_total, country_cc, by = c("cr_year", "country_code")) |>
  mutate(cr_year = factor(cr_year)) |>
  mutate(prop = cc_total / articles) |>
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) |>
  filter(!is.na(country_code)) |>
  mutate(
    country_name = countrycode::countrycode(country_code, origin = "iso2c", destination = "country.name")
  )


# top 20 countries in terms of total publication volume
top_20 <- country_df |>
  group_by(country_name) |>
  summarise(n = sum(articles)) |>
  arrange(desc(n)) |>
  head(20)

## Order of multiples, sort by the most productive (Top 20)

country_multiple_df <- country_df |>
  mutate(country_name_fct = forcats::fct_other(country_name, keep = top_20$country_name)) |>
  mutate(country_name_fct = forcats::fct_relevel(country_name_fct, top_20$country_name, "Other")) |>
  group_by(country_name_fct, cr_year) |>
  summarise(
    articles = sum(articles),
    cc_total = sum(cc_total)
  ) |>
  mutate(prop = cc_total / articles) |>
  # remove cases where we could not retrieve a country name |>
  filter(!is.na(country_name_fct))

# Prepare facet heading
facet_labels <- country_multiple_df |>
  group_by(country_name_fct) |>
  summarise(total = sum(articles)) |>
  mutate(prop = total / sum(total)) |>
  ungroup() |>
  # Highlight Germany
  mutate(country_name_tmp = ifelse(
    country_name_fct == "Germany",
    glue::glue('<p style="text-align:center;color:#0093C7"><b>{country_name_fct}</b>'),
    glue::glue('<p style="text-align:center"><b>{country_name_fct}</b>')
  )) |>
  mutate(total_string = case_when(
    total < 1000 ~ as.character(total),
    total < 1000000 ~ paste0(format(round(total / 1e3, 1), trim = TRUE), "K"),
    total >= 1000000 ~ paste0(format(round(total / 1e6, 2), trim = TRUE), "M")
  )) |>
  mutate(
    facet_labels = glue::glue(
      '{country_name_tmp}<br><br>
      {total_string} | {paste(round(prop * 100, 1), "%")}</p>'
    )
  ) |>
  distinct(country_name_fct, facet_labels)

country_facet_labels <- as.character(facet_labels$facet_labels)
names(country_facet_labels) <- facet_labels$country_name_fct

## Tooltip
country_multiple_tooltip <- country_multiple_df |>
  ungroup() |>
  mutate(tooltip_text = glue::glue('<small>{country_name_fct}</small><br><b>{format(cc_total, big.mark = ",")} / {round(prop * 100, 1)}%</b><br><small>articles with CC license in {cr_year}</small>')) |>
  mutate(max_ = max(prop)) |>
  select(country_name_fct, cr_year, tooltip_text, max_)



country_multiple_df_plot <- country_multiple_df |>
  # Highlight Germany
  mutate(my_cols = ifelse(country_name_fct == "Germany", "#0093C7", "#B0B0B0")) |>
  # Add tooltips
  inner_join(country_multiple_tooltip, by = c("country_name_fct", "cr_year")) |>
  # Improve y axis
  mutate(cr_year = gsub("^20", "'", cr_year))


country_multiple_plot <- ggplot(country_multiple_df_plot, aes(cr_year, prop, fill = my_cols, group = my_cols)) +
  geom_area(stat = "identity") +
  # Tooltip
  geom_bar_interactive(
    inherit.aes = FALSE,
    aes(cr_year, max_, tooltip = tooltip_text),
    stat = "identity",
    color = "transparent",
    fill = "transparent",
    alpha = 0.01,
    width = 1,
    position = position_dodge2()
  ) +
  facet_wrap(~country_name_fct, nrow = 3, labeller = as_labeller(country_facet_labels)) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1L),
    expand = expansion(mult = c(0, 0.05)),
    breaks = scales::breaks_extended(4)
  ) +
  scale_fill_identity() +
  theme_minimal(base_family = "Atkinson Hyperlegible") +
  labs(
    y = "OA Share", x = "2017 - 2025",
    subtitle = "**Country**<br><br>Total Lead Author Articles | Country Share"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(hjust = 0.5),
    panel.spacing = unit(1, "lines")
  )

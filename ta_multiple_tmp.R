# TA Top 20 countries


# top 20 countries in terms of total publication volume
top_20 <- hoaddata::jn_aff |>
  filter(!is.na(country_code)) |>
  distinct(issn_l, cr_year, country_code, articles_total) |>
  group_by(country_code) |>
  summarise(n = sum(articles_total)) |>
  arrange(desc(n)) |>
  head(20)

## All by year

country_by_year <- hoaddata::jn_aff |>
  distinct(issn_l, cr_year, country_code, articles_total) |>
  group_by(country_code, cr_year) |>
  summarise(n = sum(articles_total)) |>
  arrange(desc(n))

## OA by TA
oa_by_ta <- hoaddata::ta_country_output  |> 
  group_by(country_code, cr_year, has_ta) |>
  summarise(cc_total = sum(oa_n)) 

country_multiple_df <- left_join(country_by_year, oa_by_ta, by = c("country_code", "cr_year")) |>
  filter(!is.na(country_code)) |>
  mutate(country_name_fct = forcats::fct_other(country_code, keep = top_20$country_code)) |>
  mutate(country_name_fct = forcats::fct_relevel(country_name_fct, top_20$country_code, "Other")) |>
  group_by(country_name_fct, cr_year, has_ta) |>
  mutate(n = sum(n),
         cc_total = sum(cc_total)) |>
  select(-country_code) |>
  distinct()

country_multiple_df |>
  ggplot(aes(cr_year, cc_total / n, fill = has_ta)) +
  geom_area(stat = "identity") +
  # Tooltip
  facet_wrap(~country_name_fct, nrow = 3)

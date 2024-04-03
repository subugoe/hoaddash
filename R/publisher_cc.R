### Helper fxn

top_13_plot <- function(.collection = NULL, ...) {
  my_df <- tooltips_df |>
    filter(collection == .collection) |>
    mutate(publisher_fct = forcats::fct_relevel(esac_publisher, top_13))
  
  pp <-  my_df |>
    # to do : all cc articles
    group_by() |>
    mutate(max_ = max(prop)) 
  ## Plot Top 3
  p_top_3 <- my_df |> 
    filter(publisher_fct %in% top_13[1:3]) |> 
    ggplot(aes(cr_year, prop,  fill = forcats::fct_rev(cc), group = forcats::fct_rev(cc))) +
    geom_area(
      #aes(fill = cc, group = cc),
      stat = "identity",
      color = "white",
      size = 0.3
    ) +
    geom_bar_interactive(data = filter(pp, publisher_fct %in% top_13[1:3]), 
                         inherit.aes = FALSE,
                         aes(cr_year, max_, tooltip = tooltip_group),
                         stat = "identity",
                         color = "transparent",
                         fill = "transparent",
                         alpha = 0.01,
                         width = 1,
                         position = position_dodge2()
    ) + 
    facet_wrap(~publisher_fct, scales = "fixed", nrow = 1) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                       expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(4)) +
    scale_fill_manual("",
                      values =  c(
                        `CC BY` = "#65BADA",
                        `CC BY-SA` = "#068E8C",
                        `CC BY-NC` = "#00A757",
                        `CC BY-NC-SA` = "#E5BA52",
                        `CC BY-ND` = "#D86F27",
                        `CC BY-NC-ND` = "#C82E6B"
                      ),
                      guide = guide_legend(nrow = 1)) +
    theme_minimal(base_family = "Atkinson Hyperlegible", base_size = 20) +
    labs(y = NULL, x = NULL) +
    #       title = "Open Access in Hybrid Journals",
    #       subtitle = "By Creative Commons license variant as provided by Crossref\n") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x=element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.ontop = FALSE,
          legend.position="top", legend.justification = "right",
          legend.title=element_text(size = 14), 
          legend.text=element_text(size = 12)) 
  p_other <-  my_df |> 
    filter(publisher_fct %in% top_13[4:13]) |> 
    ggplot(aes(cr_year, prop,  fill = forcats::fct_rev(cc), group = forcats::fct_rev(cc))) +
    geom_area(
      #aes(fill = cc, group = cc),
      stat = "identity",
      color = "white",
      size = 0.3
    ) +
    geom_bar_interactive(data = filter(pp, publisher_fct %in% top_13[4:13]), 
                         inherit.aes = FALSE,
                         aes(cr_year, max_, tooltip = tooltip_group),
                         stat = "identity",
                         color = "transparent",
                         fill = "transparent",
                         alpha = 0.01,
                         width = 1,
                         position = position_dodge2()
    ) + 
    facet_wrap(~publisher_fct, scales = "fixed", ncol = 5,
               labeller = label_wrap_gen(multi_line = TRUE)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                       expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(4)) +
    scale_fill_manual("License spectrum",
                      values =  c(
                        `CC BY` = "#65BADA",
                        `CC BY-SA` = "#068E8C",
                        `CC BY-NC` = "#00A757",
                        `CC BY-NC-SA` = "#E5BA52",
                        `CC BY-ND` = "#D86F27",
                        `CC BY-NC-ND` = "#C82E6B"
                      )) +
    theme_minimal(base_family = "Atkinson Hyperlegible", base_size = 13) +
    labs(y = NULL, x = "2017-2024") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x=element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.ontop = FALSE,
          legend.position="none")
  # Merge into one plot
  publisher_p <-  p_top_3 / p_other +
    plot_layout(guides = 'keep')
  publisher_p
}

cc_tooltips_publisher <- function(cc_df = NULL) {
  
  total_by_year <- cc_df |>
    group_by(cr_year)  |>
    summarise(cc_total_all = sum(cc_articles),
              cc_total_prop = sum(prop))

    my_tooltips <-
    cc_df |>
    mutate(colors = case_when(
      cc == "CC BY" ~ "#65BADA",
      cc == "CC BY-SA" ~ "#068E8C",
      cc == "CC BY-NC" ~ "#00A757",
      cc == "CC BY-NC-SA" ~ "#E5BA52",
      cc == "CC BY-ND" ~ "#D86F27",
      cc == "CC BY-NC-ND" ~ "#C82E6B")) |>
    mutate(tooltip = glue::glue('
<tr>
    <td style="text-align:left;padding-bottom:0"><span style="color:{colors}">■</span> {cc}<br></td>
    <td style="text-align:right;padding-bottom:0">{format(cc_articles, big.mark = ",")}</td>
    <td style="text-align:right;">{round(prop * 100, 1)}%</td>
</tr>')) |>
    group_by(cr_year) |>
    summarise(tooltip_group = paste(tooltip, collapse = "")) |>
    inner_join(total_by_year, by = "cr_year") |>
    mutate(tooltip_group = glue::glue('<h3><span class="text-muted">Articles with Creative Commons license<br></span>in <b>{cr_year}</h3><hr><table style="margin:8px 0 0;padding:0;width:100%">
  <colgroup>
       <col span="1" style="width: 50%;">
       <col span="1" style="width: 30%;">
       <col span="1" style="width: 20%;">
      </colgroup>
    <tbody>
    {tooltip_group}
    <tr style="padding-top:5px">
    <td style="text-align:left;padding-bottom:0;font-weight:bold;"><span style="color:transparent">■</span> <b>Total</b></td>
    <td style="text-align:right;font-weight:bold;padding-bottom:0;">{format(cc_total_all, big.mark = ",")}</td>
    <td style="text-align:right;font-weight:bold;">{round(cc_total_prop * 100, 1)}%</td>
</tr></tbody></table>'))
  
  # Bringing it all together for ggplot
  # Ranges for retangles plot, which we want to use for data hover by year
  return(my_tooltips)
}


## Journal per year
publisher_per_year <- summarize_pubs(var_summary = c(cr_year, esac_publisher)) |>
  tidyr::complete(cr_year, esac_publisher, collection) |>
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) |>
  select(-oa_articles)
## CC variants

jn_cc_variants <- function(.country_code = NULL) {
  if(is.null(.country_code))
    tmp_df <- hoaddata::cc_articles
  else
    tmp_df <- hoaddata::cc_articles |>
      filter(country_code %in% .country_code)
  tmp_df |> 
    group_by(issn_l, cr_year, cc) |>
    summarise(cc_articles = n_distinct(doi)) |>
    mutate(cr_year = as.character(cr_year))
}

cc_de <- jn_cc_variants("DE") |>
  mutate(collection = "de") 
cc_all <- jn_cc_variants() |>
  mutate(collection = "global")
cc_df <- bind_rows(cc_all, cc_de)

cc_plot_top_df <- hoaddata::jct_hybrid_jns |>
  distinct(issn_l, esac_publisher) |>
  inner_join(cc_df) |> 
  group_by(esac_publisher, cr_year, collection, cc) |>
  summarise(cc_articles = sum(cc_articles)) |>
  inner_join(publisher_per_year) |>
  mutate(prop = cc_articles / articles)  |>
  ungroup() |> 
  tidyr::complete(cr_year, esac_publisher, collection, cc) |>
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) |>
  dplyr::mutate(cc = factor(
    cc,
    # Order by permissiveness
    levels = c(
      "CC BY",
      "CC BY-SA",
      "CC BY-NC",
      "CC BY-NC-SA",
      "CC BY-ND",
      "CC BY-NC-ND"
    ))) |>
  arrange(cc)

# top publishers
top_13 <- cc_plot_top_df |>
  filter(collection == "global", !esac_publisher %in% "National Academy of Sciences") |>
  group_by(esac_publisher) |>
  summarise(oa = sum(cc_articles)) |>
  arrange(desc(oa)) |>
  head(13) |>
  pull(esac_publisher)

publisher_subsets <- tibble::tibble(esac_publisher = top_13) |>
  mutate(collection = list(c("de", "global"))) |>
  unnest(collection)

### Make tooltips for ech publisher and collection
tlt_df <- purrr::map2_df(.x = publisher_subsets$esac_publisher, .y = publisher_subsets$collection, function(.x, .y, ...) {
  cc_plot_top_df |>
    filter(esac_publisher == .x, collection == .y) |>
    cc_tooltips_publisher()  |>
    mutate(esac_publisher = .x, collection = .y) |>
    select(-contains("cc")) 
})

tooltips_df <- cc_plot_top_df |>
  inner_join(tlt_df, by = c("esac_publisher", "cr_year", "collection"))

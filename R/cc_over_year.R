# Define tooltip
cc_tooltips <- function(cc_df = NULL) {
  
  total_by_year <- cc_df |>
    group_by(cr_year)  |>
    summarise(cc_total_all = sum(cc_total),
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
    <td style="text-align:right;padding-bottom:0">{format(cc_total, big.mark = ",")}</td>
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


#' Plot Creative Commons license over time
plot_cc_variants_by_year <- function(cc_df = NULL) {
  
  pp <-  cc_tooltips(cc_df) |>
    ungroup() |>
    mutate(max_ = max(cc_total_prop)) 
  
  ggplot(cc_df, aes(cr_year, prop, fill = forcats::fct_rev(cc), group = forcats::fct_rev(cc))) +
    geom_area(
      stat = "identity",
      color = "white",
      linewidth = 0.3
    ) +
    # Tooltip
    geom_bar_interactive(data = pp, 
                         inherit.aes = FALSE,
                         aes(cr_year, max_, tooltip = tooltip_group),
                         stat = "identity",
                         color = "transparent",
                         fill = "transparent",
                         alpha = 0.01,
                         width = 1,
                         position = position_dodge2()
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                       expand = expansion(mult = c(0, 0.05))
                       #    breaks = c(0, .25, .5),
                       #    limits = c(0, .7)
    )  +
    scale_fill_manual("",
                      values =  c(
                        `CC BY` = "#65BADA",
                        `CC BY-SA` = "#068E8C",
                        `CC BY-NC` = "#00A757",
                        `CC BY-NC-SA` = "#E5BA52",
                        `CC BY-ND` = "#D86F27",
                        `CC BY-NC-ND` = "#C82E6B"
                      ), guide = guide_legend(nrow = 1)) +
    theme_minimal(base_family = "Atkinson Hyperlegible", base_size = 20) +
    labs(y = NULL, x = NULL) +
    #       title = "Open Access in Hybrid Journals",
    #       subtitle = "By Creative Commons license variant as provided by Crossref\n") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.ontop = FALSE,
          legend.position="top", legend.justification = "right",
          legend.title=element_text(size = 14), 
          legend.text=element_text(size = 12)) 
}
# helper for non-detailed RADexplorer plot

library(tidyverse)
library(ggtext)

build_nondetailed_plot <- function(unique, groups_info, RADq, selected_regions_clean, selected_vr, vr_levels_all, vregionIDs = FALSE) {

  # plotting inputs
  groups_plot <- unique %>%
    select(taxa, any_of(selected_vr)) %>%
    pivot_longer(
      cols = -taxa,
      names_to = "vregion",
      values_to = "group"
    ) %>%
    mutate(
      taxa = as.character(taxa),
      vregion = factor(vregion, levels = vr_levels_all),
      group = as.character(group)
    ) %>%
    group_by(vregion) %>%
    mutate(
      group_id = match(group, unique(group)),
      vx = match(vregion, vr_levels_all)
    ) %>%
    ungroup()

  # plotting constants
  gap <- 1.5
  n_vr <- length(vr_levels_all)
  tile_w <- 0.7
  bracket_x <- 0.28
  bracket_arm <- 0.10
  check_x <- bracket_x + 0.03

  # y positions
  taxa_levels <- c(
    groups_info %>%
      filter(taxa %in% groups_plot$taxa) %>%
      arrange(group_num, taxa_order) %>%
      pull(taxa),
    setdiff(unique(groups_plot$taxa), groups_info$taxa) %>% sort()
  ) %>% unique()

  y_map <- tibble(
    taxa = taxa_levels,
    y = seq(1, by = 1 + gap, length.out = length(taxa_levels))
  )

  groups_plot <- groups_plot %>%
    left_join(y_map, by = "taxa")

  # repeated header labels
  header_rows <- tibble(
    y_header = y_map$y[seq(1, nrow(y_map), by = 1)] - 1.4
  ) %>%
    tidyr::crossing(
      tibble(
        vregion = vr_levels_all,
        vx = seq_along(vr_levels_all)
      )
    )

  # axis label data
  y_breaks <- y_map %>%
    left_join(
      RADq %>%
        transmute(
          taxa = species,
          variable_region,
          copy_num = as.numeric(copy_num)
        ) %>%
        filter(variable_region %in% selected_regions_clean, !is.na(copy_num)) %>%
        distinct(taxa, copy_num) %>%
        count(taxa, name = "n_copies"),
      by = "taxa"
    ) %>%
    mutate(n_copies = tidyr::replace_na(n_copies, 0))

  # grouped taxon brackets
  group_summary <- y_map %>%
    left_join(groups_info %>% select(taxa, group, group_label), by = "taxa") %>%
    add_count(group, name = "n_taxa")

  group_bracket_df <- group_summary %>%
    filter(n_taxa > 1) %>%
    group_by(group, group_label) %>%
    summarise(
      y_start = min(y) - 0.72,
      y_end = max(y) + 0.72,
      .groups = "drop"
    )

  # unique taxon checks
  unique_taxa_df <- group_summary %>%
    filter(n_taxa == 1) %>%
    distinct(taxa, y)

  # base plot
  p_msa <- ggplot() +
    geom_text(
      data = header_rows,
      aes(x = vx, y = y_header + 0.25, label = vregion),
      inherit.aes = FALSE,
      color = "black",
      size = 2.8
    ) +
    geom_segment(
      data = y_map,
      aes(x = 0.6, xend = n_vr + 0.4, y = y, yend = y),
      inherit.aes = FALSE,
      color = "grey80",
      linewidth = 1.2,
      lineend = "round"
    ) +
    geom_tile(
      data = groups_plot,
      aes(x = vx, y = y, fill = factor(group_id)),
      width = tile_w,
      height = 1.5,
      color = "black",
      linewidth = 0.35
    )

  # grouped taxon brackets
  if (nrow(group_bracket_df) > 0) {
    p_msa <- p_msa +
      geom_segment(
        data = group_bracket_df,
        aes(y = y_start, yend = y_end),
        x = bracket_x,
        xend = bracket_x,
        inherit.aes = FALSE,
        color = "red3",
        linewidth = 0.75,
        lineend = "round"
      ) +
      geom_segment(
        data = group_bracket_df,
        aes(y = y_start, yend = y_start),
        x = bracket_x,
        xend = bracket_x + bracket_arm,
        inherit.aes = FALSE,
        color = "red3",
        linewidth = 0.75,
        lineend = "round"
      ) +
      geom_segment(
        data = group_bracket_df,
        aes(y = y_end, yend = y_end),
        x = bracket_x,
        xend = bracket_x + bracket_arm,
        inherit.aes = FALSE,
        color = "red3",
        linewidth = 0.75,
        lineend = "round"
      )
  }

  # unique taxon checks
  if (nrow(unique_taxa_df) > 0) {
    p_msa <- p_msa +
      geom_text(
        data = unique_taxa_df,
        aes(x = check_x, y = y),
        label = "✔",
        inherit.aes = FALSE,
        color = "green3",
        size = 4
      )
  }

  # tile IDs
  if (isTRUE(vregionIDs)) {
    p_msa <- p_msa +
      geom_text(
        data = groups_plot,
        aes(x = vx, y = y + 0.08, label = group_id),
        inherit.aes = FALSE,
        color = "white",
        size = 2.8,
        fontface = "bold"
      )
  }

  # axes and theme
  p_msa <- p_msa +
    scale_x_continuous(
      breaks = seq_len(n_vr),
      labels = NULL,
      position = "top",
      limits = c(0.05, n_vr + 0.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = y_breaks$y,
      labels = make_species_axis_labels(y_breaks$taxa, y_breaks$n_copies),
      limits = c(max(groups_plot$y) + 0.8, min(header_rows$y_header) - 0.2),
      expand = c(0, 0),
      trans = "reverse"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = ggtext::element_markdown(margin = ggplot2::margin(r = 18)),
      axis.text.x.top = element_blank(),
      axis.ticks.x.top = element_blank()
    ) +
    labs(x = NULL, y = NULL)

  # plot height
  plot_height <- max(200, 80 + max(y_map$y) * 25)

  # return plot and height
  list(
    plot = p_msa,
    plot_height = plot_height
  )
}

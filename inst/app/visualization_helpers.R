# helper functions for RADexplorer plotting

library(tidyverse)
library(ggtext)

standardize_plot_inputs <- function(RADq, unique, groups, uniqueVregions, selected_regions_clean) {

  # standardize column names
  if ("species" %in% names(unique) && !"taxa" %in% names(unique)) {
    unique <- unique %>% dplyr::rename(taxa = species)
  }

  if ("species" %in% names(groups) && !"taxa" %in% names(groups)) {
    groups <- groups %>% dplyr::rename(taxa = species)
  }

  if ("groups" %in% names(groups) && !"group" %in% names(groups)) {
    groups <- groups %>% dplyr::rename(group = groups)
  }

  if ("species" %in% names(uniqueVregions) && !"taxa" %in% names(uniqueVregions)) {
    uniqueVregions <- uniqueVregions %>% dplyr::rename(taxa = species)
  }

  # keep only the RADq columns used for plotting
  RADqtiles <- RADq %>%
    dplyr::filter(variable_region %in% selected_regions_clean) %>%
    dplyr::transmute(
      species,
      variable_region_clean = variable_region,
      copy_num = as.numeric(copy_num),
      seq_id
    ) %>%
    dplyr::filter(!is.na(copy_num), !is.na(seq_id)) %>%
    dplyr::distinct(species, variable_region_clean, copy_num, .keep_all = TRUE)

  # preserve input group order
  groups_info <- groups %>%
    dplyr::select(taxa, group) %>%
    dplyr::mutate(
      taxa_order = dplyr::row_number(),
      group_num = match(group, unique(group)),
      group_label = paste0("Group ", group_num)
    )

  list(
    RADq = RADq,
    unique = unique,
    groups = groups,
    uniqueVregions = uniqueVregions,
    RADqtiles = RADqtiles,
    groups_info = groups_info
  )
}

build_species_layout <- function(RADqtiles, groups_info, gap = 2) {

  # order taxa by grouping first
  species_levels <- c(
    groups_info %>%
      dplyr::filter(taxa %in% RADqtiles$species) %>%
      dplyr::arrange(group_num, taxa_order) %>%
      dplyr::pull(taxa),
    sort(setdiff(unique(RADqtiles$species), groups_info$taxa))
  ) %>% unique()

  species_counts <- RADqtiles %>%
    dplyr::distinct(species, copy_num) %>%
    dplyr::count(species, name = "n_copies") %>%
    dplyr::mutate(species = factor(species, levels = species_levels)) %>%
    dplyr::arrange(species)

  species_layout <- species_counts %>%
    dplyr::mutate(
      start = dplyr::lag(cumsum(n_copies + gap), default = 0) + 1,
      end = start + n_copies - 1,
      y_lab = (start + end) / 2
    ) %>%
    dplyr::left_join(
      groups_info %>%
        dplyr::select(species = taxa, group, group_num, group_label, taxa_order),
      by = "species"
    )

  copy_layout <- RADqtiles %>%
    dplyr::distinct(species, copy_num) %>%
    dplyr::mutate(species = factor(species, levels = species_levels)) %>%
    dplyr::arrange(species, copy_num) %>%
    dplyr::group_by(species) %>%
    dplyr::mutate(copy_row = dplyr::row_number()) %>%
    dplyr::ungroup()

  RADqtiles_plot <- RADqtiles %>%
    dplyr::mutate(
      species = factor(species, levels = species_levels),
      seq_id_local = factor(substring(seq_id, 3))
    ) %>%
    dplyr::left_join(
      species_layout %>% dplyr::select(species, start),
      by = "species"
    ) %>%
    dplyr::left_join(copy_layout, by = c("species", "copy_num")) %>%
    dplyr::mutate(
      y = start + copy_row - 1,
      hover_text = paste0(species, " copy number ", copy_num)
    )

  y_breaks <- species_layout %>%
    dplyr::arrange(species) %>%
    dplyr::select(species, y_lab, n_copies)

  group_sizes <- species_layout %>%
    dplyr::count(group_label, name = "n_species")

  group_bracket_df <- species_layout %>%
    dplyr::group_by(group_label) %>%
    dplyr::summarise(
      y_start = min(start) - 0.38,
      y_end = max(end) + 0.38,
      .groups = "drop"
    ) %>%
    dplyr::left_join(group_sizes, by = "group_label") %>%
    dplyr::filter(n_species > 1) %>%
    dplyr::select(-n_species)

  unique_taxa_df <- species_layout %>%
    dplyr::left_join(group_sizes, by = "group_label") %>%
    dplyr::filter(n_species == 1) %>%
    dplyr::distinct(species, y_lab)

  list(
    species_layout = species_layout,
    copy_layout = copy_layout,
    RADqtiles = RADqtiles_plot,
    y_breaks = y_breaks,
    group_bracket_df = group_bracket_df,
    unique_taxa_df = unique_taxa_df
  )
}

make_species_axis_labels <- function(species, n_copies) {

  # species name plus copy count label
  paste0(
    "<span style='font-size:10pt; line-height:1.1; font-weight:650;'><i>", species, "</i></span>",
    "<br><span style='font-size:8pt; line-height:1.1;'>",
    n_copies,
    " 16S rRNA gene cop",
    ifelse(n_copies == 1, "y", "ies"),
    "</span>"
  )
}

make_plotly_layout <- function(p_msa, plot_height) {

  # final plotly layout
  ggplotly(p_msa, tooltip = c("text", "seq_id")) %>%
    layout(
      margin = list(l = 220, r = 30, t = 140, b = 40),
      height = plot_height,
      annotations = list(
        list(
          x = -0.1,
          y = 1,
          yshift = 18,
          xref = "paper",
          yref = "paper",
          text = "<b>Taxa</b>",
          showarrow = FALSE,
          xanchor = "center",
          yanchor = "bottom",
          font = list(size = 15, color = "black")
        ),
        list(
          x = 0.5,
          y = 1,
          yshift = 18,
          xref = "paper",
          yref = "paper",
          text = "<b>16S rRNA Gene Copies</b>",
          showarrow = FALSE,
          xanchor = "center",
          yanchor = "bottom",
          font = list(size = 15, color = "black")
        )
      ),
      xaxis = list(
        side = "top",
        showline = FALSE,
        zeroline = FALSE,
        showgrid = FALSE
      ),
      yaxis = list(
        showline = FALSE,
        zeroline = FALSE,
        showgrid = FALSE
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
}

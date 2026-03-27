#' helper for building genus + species dropdown choices
#'
#' makes one boldable genus row plus its species rows
#'
#' @param species_vector vector of species names
#'
#' @return data frame of selectize choices
#' @export
make_taxa_choices <- function(species_vector) {
  species_vector <- sort(unique(species_vector))
  genus_vec <- sub(" .*", "", species_vector)
  genus_levels <- unique(genus_vec)
  
  out <- do.call(
    rbind,
    lapply(genus_levels, function(g) {
      genus_row <- data.frame(
        value = paste0(g, " - All Species"),
        label = paste0(g, " - All Species"),
        group = g,
        search_text = g,
        is_genus = TRUE,
        stringsAsFactors = FALSE
      )
      
      species_rows <- data.frame(
        value = species_vector[genus_vec == g],
        label = species_vector[genus_vec == g],
        group = g,
        search_text = paste(species_vector[genus_vec == g], g),
        is_genus = FALSE,
        stringsAsFactors = FALSE
      )
      
      rbind(genus_row, species_rows)
    })
  )
  
  rownames(out) <- NULL
  out
}


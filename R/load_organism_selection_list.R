#' Load list used in RADexplorer dropdown
#'
#' @param accessions_table an table with columns "organism" and "genus"
#'
#' @returns
#' @export
load_organism_selection_list <- function(accessions_table){
  genera <- accessions_table$genus |>
    unique() |>
    sort()
  list_size = length(genera) + length(accessions_table)
  selection_list <- vector("character", list_size)
  curr_index <- 1
  for (curr_genus in genera){
    selection_list[curr_index] <- paste0(curr_genus, " - All Species")
    curr_index <- curr_index + 1 
    unordered_species <- accessions_table |>
      (function(x) x[accessions_table$genus == curr_genus, ])() |>
      (function(x) x$organism)() |>
      unique()
    ordered_species <- unordered_species[order(!grepl("^[a-zA-Z]", unordered_species), unordered_species)]
    for(species in ordered_species){
      selection_list[curr_index] <- species
      curr_index <- curr_index + 1 
    }
  }
  return(selection_list)
}
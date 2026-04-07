#' Build the RADexplorer loading screen UI
#'
#' @return A bslib page_fillable UI object containing a centered loading
#'   spinner and a randomly selected loading message.
loading_screen_ui <- function() {
  bslib::page_fillable(
    div(
      style = "
        height:100vh;
        display:flex;
        align-items:center;
        justify-content:center;
        flex-direction:column;
        gap:16px;
      ",
      h3("Loading RADx..."),
      div(class = "spinner-border", role = "status"),
      p(sample(get_loading_messages(), 1))
    )
  )
}

#' Get loading screen messages
#'
#' @return A character vector of loading messages.
get_loading_messages <- function() {
  c(
    "Parsing FASTA files...",
    "Clustering sequences...",
    "Mapping regions V1-V9...",
    "Grouping identical reads...",
    "Collapsing redundant taxa...",
    "Filtering low-quality reads...",
    "Resolving ambiguous bases...",
    "Checking alignment consistency...",
    "Reconciling duplicate IDs...",
    "Amplifying target regions...",
    "Trusting the pipeline...",
    "Feeding the cultures...",
    "Identifying taxa...",
    "Streaking agar plates...",
    "Looking for the one working pipette...",
    "Centrifuging samples...",
    "Extracting DNA...",
    "Frantically coding..."
  )
}

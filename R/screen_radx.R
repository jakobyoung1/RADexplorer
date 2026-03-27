radx_screen_ui <- function() {
  # rad explorer menu
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      shiny::h5("Options"),
      # variable region select
      shiny::checkboxGroupInput(
        "varRegions",
        "Select all 16S gene variable regions to include:",
        choices = paste0("V", 1:9),
        selected = c("V4")
      ),
      shiny::div(
        style = "margin-top:-16px; margin-bottom:8px; font-size:12px;",
        shiny::actionLink("deselectVarRegions", "Deselect all v-regions")
      ),
      bslib::input_switch(
        "detailedView",
        label = "Detailed View",
        value = TRUE
      ),
      bslib::input_switch(
        "vregionIDs",
        label = "Display V-Region Labels",
        value = FALSE
      ),
      shiny::div(
        style = "display:flex; gap:10px; width:100%;",
        shiny::actionButton("backToMenu", "Back", style = "flex:1;")
      )
    ),
    bslib::card(
      shiny::conditionalPanel(
        "input.continueWithTaxa > 0",
        plotly::plotlyOutput("visual", height = "650px")
      )
    )
  )
}

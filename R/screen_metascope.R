metascope_screen_ui <- function(genus, species) {
  # Metascope download menu
  page_fillable(
    title = "RADport to MetaScope",
    fillable = TRUE,
    useShinyjs(),
    div(
      style = "display:flex; align-items:flex-start; justify-content:center; padding-top:50px; padding-bottom:50px; overflow:visible;",
      div(
        style = "display:flex; gap:24px; width:min(900px, 95vw); overflow:visible; align-items:stretch;",
        card(
          style = "flex:1 1 0; overflow:visible;",
          card_body(
            style = "overflow:visible;",
            div(
              style = "display:flex; flex-direction:column; gap:16px; overflow:visible;",
              div(
                style = "display:flex; gap:12px; width:100%;",
                h4("RADport", style = "margin:0;"),
                actionButton("backToMenu", "Back", style = "margin-left:auto;")
              ),
              div(
                style = "overflow:visible;",
                checkboxInput(
                  "useMetascopeFilters",
                  "Use MetaScope filters",
                  value = FALSE
                ),
                card(
                  id = "metascopeFilterCard",
                  style = "opacity:0.5; background-color:#f8f9fa; overflow:visible;",
                  p("To create MetaScope filters, select taxa below."),
                  shinyjs::disabled(
                    tags$div(
                      id = "metascopeFilterControls",
                      style = "overflow:visible;",

                      tags$div(
                        style = "overflow:visible;",
                        tags$label(
                          `for` = "selectGenusFilter",
                          style = "display:block; margin-bottom:0px;",
                          tags$div("Select genus or genera to filter:"),
                          tags$div(
                            style = "font-size:13px; margin-top:0px;",
                            class = "checkbox",
                            tags$label(
                              checkboxInput(
                                "entireGenusFilter",
                                label = HTML("<i>Filter all members of the selected genus</i>"),
                                value = FALSE
                              )
                            )
                          )
                        ),
                        selectizeInput(
                          "selectGenusFilter",
                          label = NULL,
                          choices = genus,
                          multiple = TRUE,
                          options = list(
                            placeholder = "Type to search",
                            maxOptions = 10000,
                            openOnFocus = FALSE,
                            dropdownParent = "body"
                          ),
                          width = "100%"
                        )
                      ),

                      conditionalPanel(
                        condition = "input.useMetascopeFilters && input.selectGenusFilter && input.selectGenusFilter.length > 0",
                        div(
                          style = "overflow:visible;",
                          selectizeInput(
                            "selectTaxaFilter",
                            "Select species to filter:",
                            choices = character(0),
                            multiple = TRUE,
                            options = list(
                              placeholder = "Type to search",
                              maxOptions = 10000,
                              closeAfterSelect = FALSE,
                              openOnFocus = FALSE,
                              dropdownParent = "body"
                            ),
                            width = "100%"
                          )
                        )
                      )
                    )
                  )
                )
              ),
              div(
                style = "display:flex; flex-direction:column; gap:12px; width:100%;",
                actionButton("port", "Download RADport files", style = "width:100%;"),
                actionButton(
                  "metascopeInstructionsButton",
                  "MetaScope RADport Instructions",
                  style = "width:100%;"
                )
              )
            )
          )
        )
      )
    )
  )
}

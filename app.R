library(shiny)
library(sortable)
library(htmlwidgets)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css", integrity = "sha384-T3c6CoIi6uLrA9TneNEoa7RxnatzjcDSCmG1MXxSR1GAsXEV/Dwwykc2MPK8M2HN",  crossorigin = "anonymous"),
    #tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/sortablejs@latest/Sortable.min.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@latest/Sortable.min.js")
  ),

  tags$div(
    id='allcards',
    class="container d-flex justify-content-center align-items-center vh-100",
    tags$div(
      class='card',
      style="width: 18rem;",
      tags$div(
        class='card-body',
        tags$ul(
          id="example_1",
          tags$li(class="btn btn-primary p-2", "Item 1"),
          tags$li(class="btn btn-primary p-2", "Item 2")
        )
      ),
    ),

    tags$div(
      class='card',
      style='width: 18rem;',
      tags$div(
        class='card-body',
        tags$ul(
          id="example_2",
          tags$li(class="btn btn-secondary p-2", "Item 1"),
          tags$li(class="btn btn-secondary p-2", "Item 2")
        ),
      )
    )
  ),

  sortable_js(
    css_id = "example_1",
    sortable_options(group = "shared")
  ),

  sortable_js(
    css_id = "example_2",
    sortable_options(group = "shared")
  )

  # sortable_js(
  #   css_id = "dropzone",
  #   sortable_options(group = "shared")
  # )
)

server <- function(input, output, session) {
  observeEvent(input$example_1, {
    itemID <- input$example_1
    print(itemID)
    # message <- sprintf("Item with ID '%s' was dropped.", itemID)
    # cat(message, '\n')
  })
}

shinyApp(ui, server)

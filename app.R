library(shiny)
library(sortable)
library(htmlwidgets)

bucket_list_args_init <- list(
  orientation="vertical",
  header=c("Move items between buckets")
)

# bucket_list_args_lists <- list(
#   add_rank_list("drag from here",
#                 input_id = 'list1',
#                 c("one", "two", "three")),
#   add_rank_list("to here",
#                 input_id = 'list2')
# )

bucket_list_args <- c(bucket_list_args_init,bucket_list_args_lists)

ui <- fluidPage(
  fluidRow(
    column(
      width=4,
      tags$div(
        id='allcards',
        do.call(bucket_list, bucket_list_args)
      )
    ),
    column(
      width=8,
      tags$div(
        tags$p('First list'),
        verbatimTextOutput('results_1')
      ),
      tags$div(
        tags$p('Second list'),
        verbatimTextOutput('results_2')
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    update_
  })
}

shinyApp(ui, server)

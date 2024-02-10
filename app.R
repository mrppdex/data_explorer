library(shiny)
library(shinyjs)
library(sortable)
library(htmlwidgets)
library(bslib)
library(DT)

apply_filters <- function(data, criteria) {
  for(i in 1:nrow(criteria)) {
    tmpcatvar <- as.character(criteria$name[i])
    tmpparamcd <- as.character(criteria$value[i])
    data <- data %>% filter(.[[tmpcatvar]]==tmpparamcd)
  }
  return(data)
}

rank_list_args <- c(
  rank_list_args_lists
)

ui <- fluidPage(
  useShinyjs(),
  fluidRow(
    column(
      width=4,
      do.call(tags$div, rank_list_args)
    ),
    column(
      width=8,
      actionButton('reset','Reset'),
      tags$div(
        id='sinkcontainer',
        rank_list(
          text='drop here:',
          input_id='sink',
          orientation='horizontal',
          labels=NULL,
          options = sortable_options(
            group = list(
              name='shared',
              put=TRUE
            ),
            onSort=sortable_js_capture_input('sink_sorted')
          )
        )
      ),
      verbatimTextOutput('sinktxt'),
      DT::dataTableOutput("ards_tbl")
    )
  )
)

server <- function(input, output, session) {
  output$sinktxt <- renderPrint({
    input$sink
    print(input$sink)
  })

  df <- reactive({
    filter_txt <- input$sink_sorted

    if (length(filter_txt)>0) {
      grouping_info <-
        data.frame(val=filter_txt, stringsAsFactors = FALSE) %>%
        tidyr::separate(col=val, into=c('paramcd', 'param'), sep = "_{5,}",
                        extra="merge") %>%
        mutate(param=gsub('_',' ',param))

      grouping_info_col <- grouping_info %>%
        pivot_longer(cols=everything())

      filter_criteria <-
        cat_map_tbl %>% filter(value %in% (grouping_info_col %>% pull(value))) %>%
        distinct()

      filtered_ards <- apply_filters(ards, filter_criteria)

      filtered_output <-
      filtered_ards %>%
        pivot_wider(id_cols=starts_with('cat'),
                    names_from=stat,
                    values_from = val)
    }

    if(length(filter_txt)==0) {
      return(NULL)
    }
    return(filtered_output)
  })

  observeEvent(input$sink_sorted, {
    x <- input$sink_sorted
  })

  output$ards_tbl <- renderDataTable({
    datatable(df())
  })

  observeEvent(input$reset, {
    cat(paste('pressed', input$reset, '\n'))
    runjs('document.getElementById("sink").innerHTML="";')
    update_bucket_list(
      'sink', session=session
    )
  })
}

shinyApp(ui, server)

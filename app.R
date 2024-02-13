library(shiny)
library(shinyjs)
library(sortable)
library(htmlwidgets)
library(bslib)
library(DT)

library(dplyr)
library(tidyr)

cat1var <- "PARAMCD"
cat1 <- paste('Parameter', 1:3)

cat2var <- "CAT2CD"
cat2 <- paste('Category 2', LETTERS[1:3])

cat3var <- "CAT3CD"
cat3 <- paste('Category 3', LETTERS[1:3])

ards_shell <- data.frame(expand.grid(cat1var, cat1, cat2var, cat2, cat3var, cat3),
                         stringsAsFactors = FALSE)
colnames(ards_shell) <- c('cat1var', 'cat1', 'cat2var', 'cat2', 'cat3var', 'cat3')

stat <- c('N', 'RR', '95% Lower CI', '95% Upper CI')

groups <- length(cat1)*length(cat2)*length(cat3)
N_vec <- sample(10:100, groups)
RR_vec <- runif(groups, min=0.8, max=5)
lci_vec <- qnorm(0.025, RR_vec)
uci_vec <- qnorm(0.975, RR_vec)

ards_wide <- cbind(ards_shell, N_vec, RR_vec, lci_vec, uci_vec)

ards <- ards_wide %>%
  pivot_longer(
    names_to="stat",values_to = "val",
    cols = ends_with('_vec')
  ) %>%
  mutate_if(is.numeric, ~ round(.,2)) %>%
  mutate_all(~ as.character(.)) %>%
  mutate(
    stat = case_when(
      stat=='N_vec' ~ 'N',
      stat=='RR_vec' ~ 'RR',
      stat=='lci_vec' ~ '95% Lower CI',
      stat=='uci_vec' ~ '95% Upper CI'
    )
  )

cat_labels <-
  ards %>% select(starts_with('cat')) %>% distinct() %>%
  pivot_longer(
    cols=everything(),
    names_to = c(".value", "type"),
    names_pattern = "(cat\\d)(var|)"
  ) %>%
  mutate(
    type=case_when(
      type=="" ~ "cat",
      type=="var" ~ "catvar"
    )
  ) %>%
  pivot_longer(
    cols=starts_with('cat'),
    names_to='cat_number'
  ) %>% distinct() %>%
  split(., .$type)

cat_full <- cat_labels[[2]] %>% select(-type) %>%
  rename(code=value) %>%
  left_join(cat_labels[[1]] %>% select(-type) %>% rename(param=value), by='cat_number') %>%
  select(-cat_number)


create_rank_lists <- function(data, group) {
  grp <- as.character(group)
  labels <- unlist(data)
  names(labels) <- paste0(grp,strrep('_',5),gsub('\\s','_',labels))
  print(labels)

  rank_list(
    text = grp,
    input_id = tolower(grp),
    labels = labels,
    options = sortable_options(
      group = list(
        name='shared',
        put=FALSE,
        pull='clone'
      )
    )
  )
}

rank_list_args_lists <-
  cat_full %>%
  group_by(code) %>%
  summarize(paramall=list(param)) %>%
  group_by(code) %>%
  group_map(create_rank_lists) %>% rev


cat_map_tbl <-
  ards %>% select(starts_with('cat')) %>% distinct() %>%
  pivot_longer(cols=everything())


apply_filters <- function(data, criteria) {
  for(i in seq_len(nrow(criteria))) {
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
  tags$head(
    tags$script(HTML("
      function simulateDragEnter(elementId) {
        var event = new Event('dragend', {
          bubbles: true,
          cancelable: true
        });

        var element = document.getElementById(elementId);
        if (element) {
          element.dispatchEvent(event);
        }
      }
    "))
  ),
  fluidRow(
    column(
      width=4,
      do.call(tags$div, rank_list_args),
      rank_list('all columns',
        input_id='allcols',
        labels=colnames(ards),
        options = sortable_options(
          group=list(
            name='shared1'
          )
        )
      )
    ),
    column(
      width=8,
      actionButton('reset','Reset'),
      tags$div(
        id='sinkcontainer',
        rank_list(
          text='filtering:',
          input_id='sink',
          orientation='horizontal',
          labels=NULL,
          options = sortable_options(
            group = list(
              name='shared',
              put='shared'
            ),
            onSort=sortable_js_capture_input('sink_sorted')
          )
        )
      ),
      fluidRow(
        column(
          width=6,
          rank_list(
            text='stats column name',
            input_id='statcolname',
            labels=NULL,
            options = sortable_options(
              group = list(
                name='shared1',
                put=htmlwidgets::JS("function (to, from) { return from.el.id=='allcols' && to.el.children.length < 1; }")
              )
            )
          )
        ),
        column(
          width=6,
          rank_list(
            text='stats column values',
            input_id='statcolval',
            labels=NULL,
            options = sortable_options(
              group = list(
                name='shared1',
                put=htmlwidgets::JS("function (to, from) { return from.el.id=='allcols' && to.el.children.length < 1; }")
              )
            )
          )
        ),
        column(
          width=12,
          rank_list(
            text='select category columns',
            input_id='selcols',
            labels=NULL,
            options = sortable_options(
              group = list(
                name='shared1'
              )
            )
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
    stats_name <- input$statcolname
    stats_val  <- input$statcolval
    select_cols <- input$selcols

    if (length(filter_txt)>0 & length(stats_name)==1 & length(stats_val)==1) {
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

      prior_cols <- colnames(filtered_ards)
      filtered_output <-
      filtered_ards %>%
        pivot_wider(id_cols=starts_with('cat'),
                    names_from=stats_name,
                    values_from = stats_val)
      posterior_cols <- c(colnames(filtered_output), stats_name, stats_val)
      new_cols <- setdiff(posterior_cols, prior_cols)

      if(length(input$selcols)>0) {
        filtered_output <- filtered_output %>%
          select_at(c(select_cols, new_cols))
      }
    }

    if(length(filter_txt)==0 | length(stats_name)==0 | length(stats_val)==0) {
      return(ards)
    }
    return(filtered_output)
  })

  output$ards_tbl <- renderDataTable({
    datatable(df())
  })

  observeEvent(input$reset, {
    cat(paste('pressed', input$reset, '\n'))
    runjs('document.getElementById("sink").innerHTML="";')
    runjs('simulateDragEnter("sink")')
  })
}

shinyApp(ui, server)

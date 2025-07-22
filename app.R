library(shiny)
library(plotly)
library(bslib)
library(tidyverse)
library(DT)

# library(data.table)

# windowsFonts is available only on Windows; skip if unavailable
if (exists("windowsFonts")) {
  windowsFonts(NotoSansTC = "NotoSansTC-Regular", NotoSansCJKtcBlack = "NotoSansCJKtc-Black")
}

# function -- 讀取最近期的資料
latestPath <- function(path){
  latestOrder <- path %>% file.info() %>% .$ctime %>% which.max()
  return(path[latestOrder])
}

# function -- ceiling by digits as `round(digits = ...)`
ceiling_digits <- function(x, digits = 10){ceiling((x + 1)/digits)*digits}



# taiCOL taxa list ----
csv_path <- list.files(
  "www",
  pattern = "^TaiCOL_taxon_plantae_.*\\.csv$",
  full.names = TRUE
) %>% latestPath()
name_plantae_taicol <- read_csv(csv_path)

name_plantae_taicol <- name_plantae_taicol %>%
  mutate(taxon_id = paste0("<a href='", "https://taicol.tw/taxon/", taxon_id,"' target='_blank'>", taxon_id,"</a>")) %>%
  select(-name_id) %>%
  relocate(ends_with("name_c"), .after = taxon_id)


ui <- fluidPage(
  titlePanel("TaiCOL Name List"),
  sidebarLayout(
    sidebarPanel(
      em("(version TaiCOL_taxon_20250429)"),
      checkboxInput(
        inputId = "isEndemic",
        label = "Endemic",
        value = FALSE
      ),
      selectInput(
        inputId = "filter_column",
        label = "Filter Column",
        choices = names(name_plantae_taicol),
        selected = "rank"
      ),
      textInput(
        inputId = "filter_value",
        label = "Filter Value"
      ),
      selectInput(
        inputId = "plot_column",
        label = "Plot Column",
        choices = names(name_plantae_taicol),
        selected = "rank"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DTOutput("dataTable", width = "100%", height = "100%")),
        tabPanel("Plot", plotlyOutput("barPlot"))
      )
    )
  )
)


server <- function(input, output) {

  filtered_data <- reactive({
    df <- name_plantae_taicol
    if (input$isEndemic) {
      df <- df %>% filter(is_endemic)
    }
    if (nzchar(input$filter_value) && input$filter_column %in% names(df)) {
      df <- df %>% filter(str_detect(as.character(.data[[input$filter_column]]),
                                    fixed(input$filter_value, ignore_case = TRUE)))
    }
    df
  })

  output$dataTable <- renderDT({
    datatable(
      filtered_data(),
      filter = list(
        position = "top"
      ),
      extensions = c("Buttons", "FixedColumns"),
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        search = list(regex = TRUE, caseInsensitive = TRUE),
        fixedColumns = list(leftColumns = 3)
      ),
      escape = FALSE
    )
  })

  output$barPlot <- renderPlotly({
    req(input$plot_column)
    plot_data <- filtered_data() %>%
      count(.data[[input$plot_column]]) %>%
      arrange(desc(n)) %>%
      slice_head(n = 20)

    p <- ggplot(plot_data,
                aes(x = reorder(.data[[input$plot_column]], n), y = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = input$plot_column, y = "Count")

    ggplotly(p)
  })

}

shinyApp(ui, server)
  


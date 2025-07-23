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




# taiCOL taxa list ----
csv_path <- list.files(
  "www",
  pattern = "^TaiCOL_taxon_plantae_.*\\.csv$",
  full.names = TRUE
) %>% latestPath()
data_version <- stringr::str_extract(basename(csv_path), "\\d{8}")
name_plantae_taicol <- read_csv(csv_path)

name_plantae_taicol <- name_plantae_taicol %>%
  mutate(taxon_id = paste0("<a href='", "https://taicol.tw/taxon/", taxon_id,"' target='_blank'>", taxon_id,"</a>")) %>%
  select(-name_id) %>%
  relocate(ends_with("name_c"), .after = taxon_id)


ui <- fluidPage(
  titlePanel("TaiCOL Name List"),
  sidebarLayout(
    sidebarPanel(
      em(paste0("(version ", data_version, ")")),
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
        inputId = "x_col",
        label = "X Axis",
        choices = names(name_plantae_taicol),
        selected = "rank"
      ),
      selectInput(
        inputId = "y_col",
        label = "Y Axis",
        choices = c("None" = "", names(name_plantae_taicol)),
        selected = ""
      ),
      selectInput(
        inputId = "color_col",
        label = "Color",
        choices = c("None" = "", names(name_plantae_taicol)),
        selected = ""
      ),
      selectInput(
        inputId = "chart_type",
        label = "Chart Type",
        choices = c(
          "Bar" = "bar",
          "Line" = "line",
          "Scatter" = "scatter",
          "Boxplot" = "boxplot",
          "Histogram" = "hist",
          "Density" = "density",
          "Violin" = "violin",
          "Area" = "area",
          "Waffle" = "waffle",
          "Calendar" = "calendar"
        ),
        selected = "bar"
      )
    ),
    mainPanel(
      DTOutput("dataTable", width = "100%", height = "100%"),
      br(),
      plotlyOutput("barPlot")
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
    req(input$x_col)
    df <- filtered_data()
    x <- rlang::sym(input$x_col)
    y <- if (nzchar(input$y_col)) rlang::sym(input$y_col) else NULL
    color <- if (nzchar(input$color_col)) rlang::sym(input$color_col) else NULL

    p <- ggplot(df)

    if (input$chart_type == "waffle") {
      counts <- df %>% count(!!x)
      n_cols <- 10
      waffle_df <- counts %>%
        tidyr::uncount(n) %>%
        mutate(idx = row_number() - 1,
               row = idx %/% n_cols,
               col = idx %% n_cols)
      p <- ggplot(waffle_df, aes(col, -row, fill = !!x)) +
        geom_tile(color = "white") +
        scale_y_continuous(NULL, breaks = NULL) +
        scale_x_continuous(NULL, breaks = NULL) +
        guides(fill = guide_legend(title = rlang::as_label(x)))
    } else if (input$chart_type == "calendar") {
      df <- df %>% mutate(date = as.Date(as.character(!!x)))
      df <- df %>% count(date) %>%
        mutate(week = lubridate::isoweek(date),
               wday = lubridate::wday(date, week_start = 1))
      p <- ggplot(df, aes(week, wday, fill = n, text = date)) +
        geom_tile(color = "white") +
        scale_y_reverse(breaks = 1:7,
                        labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")) +
        labs(x = "Week", y = NULL, fill = "Count")
    } else {
      if (input$chart_type == "bar") {
        if (is.null(y)) {
          p <- ggplot(df, aes(x = !!x))
          if (!is.null(color)) p <- p + aes(fill = !!color)
          p <- p + geom_bar()
        } else {
          group_vars <- c(as.character(x), as.character(y))
          if (!is.null(color)) group_vars <- c(group_vars, as.character(color))
          df_sum <- df %>% count(!!!syms(group_vars))
          aes_args <- list(x = !!x, y = n)
          if (!is.null(color)) aes_args$fill <- !!color
          p <- ggplot(df_sum) + do.call(aes, aes_args) + geom_col(position = "dodge")
        }
      } else {
        aes_args <- list(x = !!x)
        if (!is.null(y) && input$chart_type %in% c("line", "scatter", "boxplot",
                                                   "violin", "area")) {
          aes_args$y <- !!y
        }
        if (!is.null(color)) aes_args$colour <- !!color
        p <- ggplot(df) + do.call(aes, aes_args)

        if (input$chart_type == "line") {
          p <- p + geom_line() + geom_point()
        } else if (input$chart_type == "scatter") {
          p <- p + geom_point()
        } else if (input$chart_type == "boxplot") {
          p <- p + geom_boxplot()
        } else if (input$chart_type == "hist") {
          p <- p + geom_histogram(bins = 30)
        } else if (input$chart_type == "density") {
          p <- p + geom_density()
        } else if (input$chart_type == "violin") {
          p <- p + geom_violin()
        } else if (input$chart_type == "area") {
          p <- p + geom_area()
        }
      }
    }

    ggplotly(p)
  })

}

shinyApp(ui, server)
  


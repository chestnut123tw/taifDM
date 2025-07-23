library(shiny)
library(plotly)
library(bslib)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(DT)
library(colourpicker)
library(viridis)

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
  mutate(
    taxon_id = paste0(
      "<a href='",
      "https://taicol.tw/taxon/",
      taxon_id,
      "' target='_blank'>",
      taxon_id,
      "</a>"
    )
  ) %>%
  select(-name_id) %>%
  relocate(ends_with("name_c"), .after = taxon_id)

# boolean columns for checkbox filters
is_cols <- grep("^is_", names(name_plantae_taicol), value = TRUE)


ui <- fluidPage(
  titlePanel("TaiCOL Name List"),
  sidebarLayout(
    sidebarPanel(
      em(paste0("(version ", data_version, ")")),
      selectInput(
        inputId = "tax_rank",
        label = "Taxonomic Rank",
        choices = c("All" = "", sort(unique(name_plantae_taicol$rank))),
        selected = ""
      ),
      ## checkbox filters for all `is_` columns
      tagList(
        lapply(is_cols, function(x) {
          checkboxInput(inputId = x, label = x, value = FALSE)
        })
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
      selectizeInput(
        inputId = "filter_values",
        label = "Filter Values (group)",
        choices = NULL,
        multiple = TRUE
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
        inputId = "fill_col",
        label = "Fill",
        choices = c("None" = "", names(name_plantae_taicol)),
        selected = ""
      ),
      selectInput(
        inputId = "palette_name",
        label = "Color Palette",
        choices = c(
          "Viridis", "Plasma", "Inferno", "Magma", "Cividis",
          "Set1", "Set2", "Dark2", "Custom"
        ),
        selected = "Viridis"
      ),
      colourInput(
        inputId = "custom_color",
        label = "Custom Color",
        value = "#1f77b4"
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
          "Calendar" = "calendar",
          "Stacked Bar" = "stackbar",
          "Pie" = "pie",
          "Heatmap" = "heatmap",
          "Lollipop" = "lollipop"
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

  observeEvent(input$filter_column, {
    choices <- unique(name_plantae_taicol[[input$filter_column]])
    updateSelectizeInput(session, "filter_values", choices = choices, server = TRUE)
  })

  filtered_data <- reactive({
    df <- name_plantae_taicol

    # taxonomic rank filter
    if (nzchar(input$tax_rank)) {
      df <- df %>% filter(rank == input$tax_rank)
    }

    # boolean checkbox filters
    for (col in is_cols) {
      val <- input[[col]]
      if (isTRUE(val)) {
        df <- df %>% filter(.data[[col]])
      }
    }

    if (length(input$filter_values) > 0 && input$filter_column %in% names(df)) {
      df <- df %>% filter(.data[[input$filter_column]] %in% input$filter_values)
    } else if (nzchar(input$filter_value) && input$filter_column %in% names(df)) {
      df <- df %>% filter(
        str_detect(as.character(.data[[input$filter_column]]),
                   fixed(input$filter_value, ignore_case = TRUE))
      )
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
    fill <- if (nzchar(input$fill_col)) rlang::sym(input$fill_col) else NULL

    chart <- input$chart_type

    if (chart == "waffle") {
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
    } else if (chart == "calendar") {
      df <- df %>% mutate(date = as.Date(as.character(!!x)))
      df <- df %>% count(date) %>%
        mutate(week = lubridate::isoweek(date),
               wday = lubridate::wday(date, week_start = 1))
      p <- ggplot(df, aes(week, wday, fill = n, text = date)) +
        geom_tile(color = "white") +
        scale_y_reverse(breaks = 1:7,
                        labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
        labs(x = "Week", y = NULL, fill = "Count")
    } else if (chart == "heatmap") {
      req(!is.null(y))
      df_sum <- df %>% count(!!x, !!y)
      p <- ggplot(df_sum, aes(!!x, !!y, fill = n)) + geom_tile()
    } else if (chart == "stackbar") {
      req(!is.null(y))
      df_sum <- df %>% count(!!x, !!y)
      p <- ggplot(df_sum, aes(!!x, n, fill = !!y)) + geom_col(position = "stack")
    } else if (chart == "pie") {
      counts <- df %>% count(!!x)
      p <- ggplot(counts, aes(x = "", y = n, fill = !!x)) +
        geom_col(width = 1) +
        coord_polar("y") +
        labs(x = NULL, y = NULL)
    } else if (chart == "lollipop") {
      req(!is.null(y))
      p <- ggplot(df, aes(!!x, !!y)) +
        geom_segment(aes(xend = !!x, yend = 0), colour = "grey") +
        geom_point(aes(color = !!color, fill = !!fill), size = 3)
    } else {
      aes_args <- list(x = !!x)
      if (!is.null(y)) aes_args$y <- !!y
      if (!is.null(color)) aes_args$colour <- !!color
      if (!is.null(fill)) aes_args$fill <- !!fill
      p <- ggplot(df, do.call(aes, aes_args))

      if (chart == "bar") {
        p <- p + geom_bar(stat = "count", position = "dodge")
      } else if (chart == "line") {
        p <- p + geom_line() + geom_point()
      } else if (chart == "scatter") {
        p <- p + geom_point()
      } else if (chart == "boxplot") {
        p <- p + geom_boxplot()
      } else if (chart == "hist") {
        p <- p + geom_histogram(bins = 30)
      } else if (chart == "density") {
        p <- p + geom_density()
      } else if (chart == "violin") {
        p <- p + geom_violin()
      } else if (chart == "area") {
        p <- p + geom_area()
      }
    }

    pal <- input$palette_name
    if (pal == "Custom") {
      p <- p + scale_color_manual(values = input$custom_color) +
        scale_fill_manual(values = input$custom_color)
    } else if (pal %in% c("Viridis", "Plasma", "Inferno", "Magma", "Cividis")) {
      opt <- tolower(pal)
      p <- p + viridis::scale_color_viridis_d(option = opt) +
        viridis::scale_fill_viridis_d(option = opt)
    } else {
      p <- p + scale_color_brewer(palette = pal) +
        scale_fill_brewer(palette = pal)
    }

    ggplotly(p)
  })
}

shinyApp(ui, server)
  


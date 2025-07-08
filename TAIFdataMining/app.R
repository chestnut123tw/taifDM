library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(shiny)
library(tidyverse)
library(DT)

# library(data.table)

windowsFonts(NotoSansTC = "NotoSansTC-Regular", NotoSansCJKtcBlack = "NotoSansCJKtc-Black")

# function -- 讀取最近期的資料
latestPath <- function(path){
  latestOrder <- path %>% file.info() %>% .$ctime %>% which.max()
  return(path[latestOrder])
}

# function -- ceiling by digits as `round(digits = ...)`
ceiling_digits <- function(x, digits = 10){ceiling((x + 1)/digits)*digits}



# taiCOL taxa list ----
name_plantae_taicol <- read_csv(grep("^.+www/TaiCOL_taxon_plantae_.+\\.csv$", x = list.files(recursive = TRUE),value = TRUE) %>% latestPath())

name_plantae_taicol <- name_plantae_taicol %>% 
  mutate(taxon_id = paste0("<a href='", "https://taicol.tw/taxon/", taxon_id,"' target='_blank'>", taxon_id,"</a>")) %>% 
  select(-name_id) %>%
  relocate(ends_with("name_c"), .after = taxon_id)


ui <- grid_page(
  layout = c(
    "header  header",
    "sidebar area2 ",
    "area3   area2 "
  ),
  row_sizes = c(
    "50px",
    "0.6fr",
    "1fr"
  ),
  col_sizes = c(
    "285px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header("Settings"),
    card_body(
      em("(version TaiCOL_taxon_20250429)"),
      checkboxInput(
        inputId = "isEndemic",
        label = "Endemic",
        value = FALSE
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "TaiCOL Name List",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "area2",
    full_screen = TRUE,
    card_body(
      DTOutput(
        outputId = "dataTable",
        width = "100%",
        height = "100%"
      )
    )
  )
)


server <- function(input, output) {
  
  output$dataTable <- renderDT({
    name_plantae_taicol %>%
      filter(is_endemic %in% input$isEndemic) %>% 
      # filter(redlist %in% input$redList) %>% 
      datatable(
        filter = list(
          position = "top"
        ),
        extensions = c("Buttons","FixedColumns"),
        options = list(
          pageLength = 5,
          autoWidth = TRUE,
          search = list(regex = TRUE, caseInsensitive = TRUE),
          fixedColumns = list(leftColumns = 3)
        ),
        escape = FALSE
      )
  })
  
}

shinyApp(ui, server)
  


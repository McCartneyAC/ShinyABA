# Packages ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinydashboardPlus)
library(shinyWidgets)
library(DT)
library(psych)
library(dplyr)
library(tidyr)
library(sjPlot)
library(MASS)
library(fontawesome)
library(ggplot2)
library(knitr)
library(rmarkdown)
library(tibble)
library(gt)

# Functions ------------------------------------------------

is_extant <-function(x) any(!is.na(x))
is_numeric<-function(x) any(is.numeric(x))

# Data import
use <- function(name) {
   # consider future support for .json? 
   if (grepl(".csv", name)) {
      readr::read_csv(name)
   } else if (grepl(".xlsx", name)) {
      readxl::read_xlsx(name)
   } else if (grepl(".dta", name)) {
      haven::read_dta(name)
   } else if (grepl(".sav", name)) {
      haven::read_spss(name)
   } else if (grepl(".rda", name)) {
      load(name)
   } else {
      stop("unknown data type.")
   }
}
description<-function(data, group = NULL, fast = TRUE, ...) {
   grp<-paste0(deparse(substitute(group)))
   #print(grp)
   if(is.null(group)) {
      data %>%
         psych::describe(fast = fast, ...) %>%
         tibble::rownames_to_column() %>%
         dplyr::select(-c(vars)) %>%
         dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
         gt::gt() %>%
         gt::tab_options(
            column_labels.font.size = "small",
            table.font.size = "small",
            row_group.font.size = "small",
            data_row.padding = px(3)
         ) %>% 
         tab_header(
            title = paste0("Data Description") 
         )
   } else {
      data %>%
         select_if(is.numeric) %>%
         psych::describeBy(group = group, fast = fast, mat= TRUE, ...) %>%
         tibble::rownames_to_column() %>%
         select(-c(item, vars)) %>%
         dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
         arrange(group1) %>%
         group_by(group1) %>%
         gt() %>%
         gt::tab_options(
            column_labels.font.size = "small",
            table.font.size = "small",
            row_group.font.size = "small",
            data_row.padding = px(3))
   } %>% 
      tab_header(
         title = paste0("Data Description") ,
         subtitle = paste0("Grouped by: ",  grp )
      )
}

# ui ---------------------------------------------------

ui <- dashboardPagePlus(skin = "black",
  dashboardHeaderPlus(title = "ShinyABA"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
     menuItem()
    ),
  dashboardBody(
    tags$head(tags$title("ShinyABA"))
    )
)

# server --------------------------------------------------

server <- function(input, output, session) { 

   datasetInput <- reactive({
      infile <- input$FileInput
      if (is.null(infile))
         return(NULL)
      dat<-use(infile$datapath)
      names(dat) <-  gsub(" ", "_", names(dat), fixed = TRUE) 
      return(dat)
   })
   
   output$data_table = DT::renderDataTable(datasetInput())
   
}

shinyApp(ui, server)


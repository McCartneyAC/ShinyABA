# Packages ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinydashboardPlus)
library(shinyWidgets)
library(DT)
library(psych)
library(tidyr)
library(sjPlot)
library(MASS)
library(fontawesome)
library(ggplot2)
library(knitr)
library(rmarkdown)
library(tibble)
library(gt)
library(xml2)
library(rvest)
library(dplyr)
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





news <- function(term) {
  # https://www.listendata.com/2020/12/web-scrape-google-news-with-r.html

  
  html_dat <- read_html(paste0("https://news.google.com/search?q=",term,"&hl=en-IN&gl=IN&ceid=US%3Aen"))
  
  dat <- data.frame(Link = html_dat %>%
                      html_nodes('.VDXfz') %>% 
                      html_attr('href')) %>% 
    mutate(Link = gsub("./articles/","https://news.google.com/articles/",Link))
  
  news_dat <- data.frame(
    Title = html_dat %>%
      html_nodes('.DY5T1d') %>% 
      html_text(),
    Link = dat$Link,
    Description =  html_dat %>%
      html_nodes('.Rai5ob') %>% 
      html_text()
  ) 
  
  # Extract Source and Time (To avoid missing content)
  prod <- html_nodes(html_dat, ".SVJrMe")
  Source <- lapply(prod, function(x) {
    norm <- tryCatch(html_node(x, "a") %>% html_text() ,
                     error=function(err) {NA})
  })
  
  time <- lapply(prod, function(x) {
    norm <- tryCatch(html_node(x, "time") %>% html_text(),
                     error=function(err) {NA})
  })
  
  mydf <- data.frame(Source = do.call(rbind, Source), Time = do.call(rbind, time), stringsAsFactors = F)
  dff <- cbind(news_dat, mydf) %>% distinct(Time, .keep_all = TRUE)
  
  return(dff)
}



# ui ---------------------------------------------------


ui <- shinydashboardPlus::dashboardPage(
  skin = "purple-light",
  dashboardHeader(title = "ShinyABA",
                  leftUi = tagList(
                    dropdownBlock(
                      fileInput("FileInput", "Input Your Data Set"),
                      id = "inputBlock",
                      icon = icon("upload"),
                      title = "Student Data",
                      badgeStatus = NULL
                    )
                  ) #Taglist
                  ),
                  #header
                  
                  
                  dashboardSidebar(
                    sidebarMenu(id = "sidebar",
                                menuItem("About", tabName = "abouttab", icon = icon("readme")),
                                menuItem("News", tabName = "newstab", icon = icon("newspaper")),
                                menuItem("View Data", tabName = "dataset", icon = icon("table"))
                                ) #menu
                    ), #sidebar

                  dashboardBody(tags$head(tags$title("ShinyABA")),
                                tabItems(
                                  tabItem(tabName = "abouttab",
                                    box( title = "About",
                                      tags$p("Info to go here:", tags$br(),
                                             "How to use. ", tags$br(),
                                             "How to structure data.", tags$br(),
                                             "Acknowledgements. ", tags$br(),
                                             "Where to send bug reports.", tags$br(),
                                             "Additional ABA information. "),
                                      textOutput("article1_headline")
                                    )
                                  ),#tabItem (ABout)
                                  tabItem(tabName = "newstab",
                                              fluidRow(
                                                column(width = 12,
                                                  box(title = "ABA in the News", solidHeader = TRUE, width = 12, height = 300,
                                              # column(width = 6,
                                              # flipBox(
                                              #   id = "myflipbox1",
                                              #   width = 12,
                                              #   front = div(
                                              #     height = "300px",
                                              #     width = "100%",
                                              #   # tags$h1(textOutput("article1_headline")),
                                              #    # textOutput("article1_headline"),
                                              #     tags$br(),
                                              #     textOutput("article1_source"),
                                              #     tags$br(),
                                              #     textOutput("article1_time")
                                              #   ),
                                              #   back = div(
                                              #     height = "300px",
                                              #     width = "100%",
                                              #     tags$h1("Header Text"),
                                              #  #  h1(textOutput("article1_headline")),
                                              #     textOutput("article1_desc")
                                              #   )
                                              # )
                                              # ), #column

                                                     DT::dataTableOutput("news_table")
                                                     ) #box
                                              ) #column

                                               ) #fluidrow
                                          ),# tabitem
                                  tabItem(
                                    tabName = "dataset",
                                      box(
                                        title = "Your Data",
                                        width = 12,
                                        DT::dataTableOutput("data_table")
                                        ) # Tabbox (Dataset view)
                                ) #tabidtem
                                ) #tabItems
                                )# dashbaord body
                                ) # UI

# server --------------------------------------------------

server <-function(input, output, session) {
    
  datasetInput <- reactive({
      infile <- input$FileInput
      if (is.null(infile))
        return(NULL)
      dat <- use(infile$datapath)
      names(dat) <-
        gsub(" ", "_", names(dat), fixed = TRUE)
      return(dat)
    })
    
    output$data_table = DT::renderDataTable(datasetInput())
    
    
    

# News Module -------------------------------------------------------------

        
    newstable <- news('ABA"%20therapy') %>% 
      as_tibble() %>% 
      mutate(headline = Title) %>% 
      mutate(Link = paste0("<a href='",Link,"'>link</a>")) %>% 
      dplyr::select(headline, Time, Source, Description, Link)
    
    ago<- news_dat %>% 
      as_tibble() %>% 
      arrange(Time) %>% 
      dplyr::filter(grepl("ago", Time))  %>% 
      tidyr::separate(Time, c("num", "unts", "ago"), " ", remove = FALSE) %>%  
      mutate(num = as.numeric(num)) %>% 
      mutate(newtime = case_when(
        unts == "minutes" ~ Sys.Date(),
        unts == "minute" ~ Sys.Date(),
        unts == "hours" ~ Sys.Date(),
        unts == "hour" ~ Sys.Date(),
        unts == "days" ~ Sys.Date()- num
      )) %>% 
      dplyr::select(headline, Source, Time, newtime,  Description, Link)
    
    yesterday <- news_dat %>% 
      as_tibble() %>% 
      arrange(Time) %>% 
      dplyr::filter(grepl('Yesterday', Time)) %>% 
      mutate(newtime = Sys.Date() -1 ) %>% 
      dplyr::select(headline, Source, Time, newtime, Description, Link)
    
    newstable<-rbind(ago, yesterday) %>% 
      dplyr::arrange(desc(newtime), Time) %>% 
      head(10) %>% 
      dplyr::select(headline, Source, Time, Description, Link)
    
   output$news_table <- DT::renderDataTable(newstable, escape = FALSE)
    
   
   # article 1 
    output$active_side_1<- renderUI({
      side <- if (input$myflipbox1) "front" else "back"
      dashboardBadge(side, color = "blue")
    })
    
    article1_headline<-renderText(
      newstable %>% 
        dplyr::filter(row_number()==1) %>% 
        dplyr::select(headline)%>% 
        as.character()
    )
    
    output$article1_link<-render_html()
    
    output$article1_source <-renderText(
      newstable %>% 
        dplyr::filter(row_number()==1) %>% 
        dplyr::select(Source)%>% 
        as.character()
    )
    
    output$article1_time <-renderText(
      newstable %>% 
        dplyr::filter(row_number()==1) %>% 
        dplyr::select(Time)%>% 
        as.character()
    )
    
    output$article1_desc <-renderText(
      newstable %>% 
        dplyr::filter(row_number()==1) %>% 
        dplyr::select(Description)%>% 
        as.character()
    )
}
    
  

shinyApp(ui, server)
                        
                        

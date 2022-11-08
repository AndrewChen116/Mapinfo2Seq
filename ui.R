library(shiny)
library(tidyverse)
library(parallel)
library(bslib)
library(DT)

css <- "mark{
          padding: 0;
          background-color:#FFFF93;
          color:#930000;
        }"

ui <- navbarPage(
  # Title 
  titlePanel(
    h3("Mapinfo2Seq"),
    windowTitle = "Mapinfo2Seq"
  ),
  theme = bs_theme(
    bootswatch = "darkly"
  ),
  
  sidebarLayout(
    # side panel
    sidebarPanel(
      ## file input
      fileInput(
        "mapinfo_file", h6(strong("Mapinfo"), style = "color:#97CBFF"),
        accept = ".tsv", multiple = F ,width = "100%",
        placeholder = "Please input tsv data "
      ),
      ## Parallel setting
      fluidRow(
        column(
          selectInput(
            "doParallel",h6(strong("Parallel calculating"),style = "color:#97CBFF"),
            choices = c("True", "False"),selected = "False"
          ),
          width = 6
        ),
        column(
          sliderInput(
            "ncore",h6(strong("Cores"),style = "color:#97CBFF"),
            value = 4, min = 2, max = floor(detectCores()*0.8)
          ),
          width = 6
        )
      ),
      br(),
      ## output setting
      fluidRow(
        column(
          actionButton(inputId="doSearch", label=" Search",
                       icon=icon(name = "compass")),
          width = 3
        ),
        column(
          actionButton(inputId="doExample", label=" Example",
                       icon=icon(name = "table")),
          width = 3
        ),
        column(
          downloadButton("downloadData", "Download"),
          width = 3
        ),
        column(
          
          width = 3
        )
      ),
      ## print execution time
      htmlOutput("result_check") %>% h6(.,align="center"),
      htmlOutput("print_time") %>% h6(.,align="center",style = "color:#B3D9D9"),
      br(),
      br(),
      br(),
      br(),
      ## version info
      h6("20221110_KLC_v0.1.1",align="right",style = "color:#6C6C6C"),
      ## width of sidebarPanel
      width = 3
      
    ),
    # main panel
    mainPanel(
      ## tag setting
      tags$head(tags$style(HTML(css))),
      
      ## output table
      tabsetPanel(
        tabPanel(
          h5("Mapinfo table",style = "color:#97CBFF"),
          dataTableOutput("mapinfo_table"),
          width = 1
        ),
        tabPanel(
          h5("Sequence result",style = "color:#97CBFF"),
          dataTableOutput("seq_table"),
          width = 1
        ),
      ),
      ## width of mainPanel
      width = 9
    )
  )
)
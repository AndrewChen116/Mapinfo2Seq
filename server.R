rm(list = ls())
source("FUN_seqCrawler.R")
options(shiny.maxRequestSize=30*1024^2) 

server <- function(input, output, session) {
  
  # load the example Rdata
  observeEvent(c(input$doSearch, input$doExample),{
    if(length(ls(envir = globalenv()))==1){
      load("example.RData")
      assign("mapinfo.df",mapinfo.df,globalenv())
      cat("Import example\n")
    }
  })
  
  # check if apply example
  observeEvent(input$doExample,{
    assign("doExample",TRUE,globalenv())
    cat("Example setting checked\n")
  })
  
  # check the doExample status
  observeEvent(c(input$doSearch, input$doExample),{
    if(sum(ls(envir = globalenv()) %in% "doExample")){
      cat("Import example:", doExample,"\n")
    }else{
      assign("doExample",FALSE,globalenv())
      cat("import example:", doExample,"\n")
    }
  })
  
  # Mapinfo table input 
  mapinfo_table <- eventReactive(c(input$doSearch, input$doExample),{
    ## record the start time
    t1 <- proc.time()
    
    ## import data
    tryCatch(
      {
        if(!is.null(input$mapinfo_file$datapath) & !doExample){
          mapinfo.df <- input$mapinfo_file$datapath %>% read.csv(sep = "\t") 
          colnames(mapinfo.df) <- c("id","chr","start","end","strand")
          cat("Imported mapinfo file checked\n")
        }else if(doExample){
          cat("Example mapinfo file checked\n")
        }else{
          mapinfo.df <- NULL
          cat("No mapinfo file imported\n")
        }
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        mapinfo.df <- NULL
      }
    )
    list(
      "mapinfo"=mapinfo.df,
      "t1"=t1
    )
  })
  
  # Crawler  
  seq_table <- eventReactive(c(input$doSearch),{
    mapinfo.lt <- mapinfo_table()
    mapinfo.df <- mapinfo.lt$mapinfo
    t1 <- mapinfo.lt$t1
    
    seq.df <- NULL
    tryCatch(
      {
        if(!is.null(mapinfo.df)){
          cat("Comparing ...\n")
          seq.lt <- genome_seq_crawler(mapinfo.df, db="hg38",
                                       applyParallel=input$doParallel, core=input$ncore)
          seq.df <- data.frame(
            Seq = seq.lt %>% unlist
          )
          cat("Comparation finished \n")
        }else{
          cat("No imported data for compare, process stop\n")
          seq.df <- NULL
        }
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        seq.df <- NULL
        cat("err")
      }
    )
    
    ## record the execution time
    t2 <- proc.time()
    t <- t2-t1
    print_time <- paste0("Execution time: ",round(t[3][[1]],2), " s")
    
    ## reset doExample
    cat("Reset example setting\n")
    assign("doExample",FALSE,globalenv())
    ## return
    cat("******************** \n")
    list(
      "output" = seq.df,
      "time" = print_time
    )
  })
  
  # check result
  result_check <- eventReactive(c(input$doSearch),{
    tryCatch(
      {
        if(!is.null(seq_table()$output)){
          txt <- paste0('<h6><p style="color:#97CBFF">Crawler finished!</p></h6>')
        }else{
          txt <- '<h6><p style="color:#CE0000">Error! please check format! </p></h6>'        }
      },
      warning = function(war){
        print(war)
        txt <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        txt <- NULL
      }
    )
  })
  
  # export to ui
  
  ## mapinfo table
  output$mapinfo_table <- renderDataTable(
    mapinfo_table()[[1]],
    options = list(
      pageLength = 25,
      searching = T,
      row.names = F
    ),
    escape = FALSE
  )
  
  ## seq result
  output$seq_table <- renderDataTable(
    seq_table()[[1]],
    options = list(
      pageLength = 25,
      searching = T,
      row.names = F
    ),
    escape = FALSE
  )
  
  ## check result
  output$result_check <- renderText(result_check())
  
  ## show execution time
  output$print_time <- renderUI(
    seq_table()[[2]]
  )
  
  ## download output df
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_output", ".txt")
    },
    content = function(file) {
      write.table(seq_table()[[1]], file, sep = "\t",
                quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
  )
}
source("FUN_seqCrawler.R")
options(shiny.maxRequestSize=30*1024^2) 

server <- function(input, output, session) {
  
  # load the example Rdata
  observeEvent(c(input$doExample),{
    ## renew Rdata
    load("example.RData")
    assign("mapinfo.df",mapinfo.df,globalenv())
    cat("Rdata imported!\n")
    
    ## detect coldStart status
    if(sum(ls(envir = globalenv()) %in% "cold_start")){
      assign("cold_start", FALSE, globalenv())
    }else{
      assign("cold_start", TRUE, globalenv())
    }
    
    ## update doExample status
    if(cold_start){
      assign("doExample",FALSE,globalenv())
      cat("coldStart detected!\n")
      cat("doExample switch OFF\n")
    }else{
      assign("doExample",TRUE,globalenv())
      cat("doExample switch ON\n")
    }
    
    ## log
    cat("----------------------\n")
    cat("Cold Start:", cold_start,"\n")
    cat("Import example:", doExample,"\n")
    cat("----------------------\n")
  })
  
  # check the input file status
  observeEvent(c(input$mapinfo_file),{
    assign("doExample",FALSE,globalenv())
    cat("doExample switch OFF\n")
  })
  
  # Mapinfo table input 
  mapinfo_table <- eventReactive(c(input$doExample, input$mapinfo_file, input$doSearch),{
    ## record the start time
    t1 <- proc.time()
    
    ## import data
    tryCatch(
      {
        if(!is.null(input$mapinfo_file$datapath) & !doExample){
          mapinfo.df <- input$mapinfo_file$datapath %>% read.csv(sep = "\t") 
          colnames(mapinfo.df) <- c("id","species","chr","start","end","strand")
          mapinfo.df$start <- mapinfo.df$start %>% as.numeric()
          mapinfo.df$end <- mapinfo.df$end %>% as.numeric()
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
        mapinfo.df <- conditionMessage(err)
      }
    )
    
    ## check status
    cat("*****\n")

    ## return
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
          seq.lt <- genome_seq_crawler(mapinfo.df, applyParallel=input$doParallel, core=input$ncore)
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
    print_time <- paste0("< Execution time: ",round(t[3][[1]],2), " s >")
    
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
  input_check <- eventReactive(c(input$doExample, input$mapinfo_file),{
    tryCatch(
      {
        if(is.null(mapinfo_table()$mapinfo)){
          txt <- '<h6><p style="color:#FFD306">== Please input Mapinfo ==</p></h6>'        
          return(txt)
        }
        if(!is.data.frame(mapinfo_table()$mapinfo)){
          txt <- '<h6><p style="color:#CE0000">== Format error! ==</p></h6>'        
          return(txt)
        }
        if(ncol(mapinfo_table()$mapinfo) != 6){
          txt <- '<h6><p style="color:#CE0000">== Wrong col number! ==</p></h6>'        
          return(txt)
        }
        if(sum(is.na(mapinfo_table()$mapinfo))){
          txt <- '<h6><p style="color:#CE0000">== NA detected! (may caused by wrong number format) ==</p></h6>'        
          return(txt)
        }
        if(sum(mapinfo_table()$mapinfo == "")){
          txt <- '<h6><p style="color:#CE0000">== Blank detected! ==</p></h6>'        
          return(txt)
        }
        if(sum(!mapinfo_table()$mapinfo$strand %in% c("+","-"))){
          txt <- '<h6><p style="color:#CE0000">== Wrong format in strand column! ==</p></h6>'        
          return(txt)
        }
        if(doExample){
          txt <- '<h6><p style="color:#00DB00">== Example prepared! ==</p></h6>'
          return(txt)
        }
      
        txt <- '<h6><p style="color:#00DB00">== Data prepared! ==</p></h6>'
        return(txt)
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
  result_check <- eventReactive(c(input$doSearch),{
    tryCatch(
      {
        if(cold_start){
          txt <- '<h6><p style="color:#CE0000"></p></h6>'  
          return(txt)
        }
        if(is.null(seq_table()$output)){
          txt <- '<h6><p style="color:#CE0000">== Error! please check format! ==</p></h6>'  
          return(txt)
        }
      
        txt <- paste0('<h6><p style="color:#00DB00">== Crawler finished! ==</p></h6>')
        return(txt)
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
  output$input_check <- renderText(input_check())
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
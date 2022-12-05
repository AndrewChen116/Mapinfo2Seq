rm(list = ls(), envir = globalenv())
genome_seq_crawler <- function(df, applyParallel=F, core=4){
  require(parallel)
  
  id.set <- df$id %>% as.character()
  db.set <- df$species %>% as.character()
  chr.set <- df$chr %>% as.character() %>% toupper() %>% str_replace_all("CHR","")
  start.set <- df$start %>% as.numeric()
  end.set <- df$end %>% as.numeric()
  strand.set <- df$strand %>% as.character()
  
  Apply_FUN <- function(n){
    require(tidyverse)
    require(XML)
    
    id <- id.set[n]
    db <- db.set[n]
    chr <- chr.set[n]
    start <- start.set[n]
    end <- end.set[n]
    
    if(as.character(strand.set[n]) == "-"){
      strand <- "hgSeq.revComp=on&"
    }else{
      strand <- ""
    }
    
    request_url <-  paste0(
      "http://genome.ucsc.edu/cgi-bin/hgc?",
      "hgsid=1482964187_qN4AP7ePlVnsgTBTnU0shqmdFWq7&g=htcGetDna2&",
      "table=&i=mixed&o=",(start-1),"&l=",(start-1),"&r=",end,
      "&getDnaPos=chr",chr,":",start,"-",end,
      "&db=",db, "&hgSeq.cdsExon=1&&hgSeq.padding5=0",
      "&hgSeq.padding3=0&hgSeq.casing=upper&",
      "boolshad.hgSeq.maskRepeats=0&hgSeq.repMasking=lower&",
      strand,"boolshad.hgSeq.revComp=0&submit=get+DNA"
    ) %>% str_remove_all("\n") %>% str_remove_all(" ")
    
    
    html_content <- htmlParse(request_url) %>% 
      xpathSApply('//pre') %>% `[[`(1) %>% `[[`(1) %>% xmlValue %>% 
      str_replace_all(" ","_") %>% str_replace("(?<=>).*(?=\n)",id)
    
    return(html_content)
  }
  
  if(applyParallel){
    
    applied.env <- new.env()
    applied.env[["db.set"]] = db.set
    applied.env[["id.set"]] = id.set
    applied.env[["chr.set"]] = chr.set
    applied.env[["start.set"]] = start.set
    applied.env[["end.set"]] = end.set
    applied.env[["strand.set"]] = strand.set
    
    cl <- makeCluster(core)
    clusterExport(cl, "db.set", envir = applied.env)
    clusterExport(cl, "id.set", envir = applied.env)
    clusterExport(cl, "chr.set", envir = applied.env)
    clusterExport(cl, "start.set", envir = applied.env)
    clusterExport(cl, "end.set", envir = applied.env)
    clusterExport(cl, "strand.set", envir = applied.env)
    
    content.lt <- parLapply(
      cl, c(1:nrow(df)), Apply_FUN
    )
    stopCluster(cl)
    
    names(content.lt) <- id.set
    
  }else{
    content.lt <- c(1:nrow(df)) %>% lapply(Apply_FUN)
    names(content.lt) <- id.set
  }
  
  return(content.lt) 
}


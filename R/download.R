download_cover <- function() {
  
  list_files <- list.files("data", recursive = TRUE, pattern = "line.xls", full.names = TRUE)

  data_cover <- parallel::mclapply(list_files, function(i) {
    
    #i = "data/1-MBOUZI/16-0610/2016_mbouzi_line.xlsx"
    
    file_name   <- gsub(".*/([[:digit:]]+_.*\\.xls)", "\\1", i)
    annee       <- gsub("[^0-9]", "", file_name)
    site        <- gsub("[[:digit:]_]+(.*)_.*", "\\1", file_name)
    file_format <- gsub(".+\\.(\\w+)$", "\\1", file_name)
    
    message(paste(site, annee))
    
    if(file_format == "xls") {
    segments <- as.data.frame(readxl::read_xls(i, range = "R42C1:R51C8", col_names = FALSE))
    } else if(file_format == "xlsx") {
      segments <- as.data.frame(readxl::read_xlsx(i, range = "R42C1:R51C8", col_names = FALSE))
    }
      
    segments <- segments[, c(1,2,4,6,8)]
    colnames(segments) <- c("substrat", "t1", "t2", "t3", "t4")
    
    if(all(is.na(apply(segments[, 2:5], 2, sum)))) {
      
      if(file_format == "xls") {
        segments <- as.data.frame(readxl::read_xls(i, range = "R52C1:R61C8", col_names = FALSE))
      } else if(file_format == "xlsx") {
        segments <- as.data.frame(readxl::read_xlsx(i, range = "R52C1:R61C8", col_names = FALSE))
      }
      
      segments <- segments[, c(1,2,4,6,8)]
      colnames(segments) <- c("substrat", "t1", "t2", "t3", "t4")
      
    }
    
    if(all(apply(segments[, 2:5], 2, sum) != 40)) {
      stop(message(paste0("sum of PIT in one of transect during ", annee, "'s ", site, " survey is not 40")))
      return(NA)
    }
  
    seg_gather <- tidyr::gather(segments, transect, value, -substrat)
    seg_spread <- tidyr::spread(seg_gather, substrat, value)
    
    n_annee    <- rep(annee, nrow(seg_spread))
    n_site     <- rep(site, nrow(seg_spread))
    
    df <- data.frame(cbind(site = n_site, annee = n_annee, seg_spread))
    
    })
  
  data_cover <- do.call(rbind, data_cover)

}
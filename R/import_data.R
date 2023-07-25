# This function import data cover of year_site_line.xls/xlsx files -------------

import_line_function <- function(list_line_files) {
  
  data_line <- data.frame(do.call(rbind, lapply(list_line_files, function(i) {
    
    #i = "data/8-NGOUJA/20-1104/2020_ngouja_line.xlsx"
    
    file_name   <- basename(i)
    annee       <- gsub("[^0-9]", "", file_name)
    split       <- strsplit(file_name, "_")
    site        <- sapply(split, "[", 2)
    file_format <- sapply(strsplit(sapply(split, "[", 3), "\\."), "[", 2)
    
    message(paste(site, annee))
    
    if (file_format == "xls") {
      segments <- as.data.frame(readxl::read_xls(i, range = "R42C1:R51C8", col_names = FALSE))
    } else if (file_format == "xlsx") {
      segments <- as.data.frame(readxl::read_xlsx(i, range = "R42C1:R51C8", col_names = FALSE))
    }  
    
    segments <- segments[, c(1,2,4,6,8)]
    colnames(segments) <- c("substrat", "t1", "t2", "t3", "t4")
    
  
    if (any(is.na(apply(segments[, names(segments) %in% c("t1", "t2", "t3", "t4")], 2, sum)))) {
        
      if (file_format == "xls") {
        segments <- as.data.frame(readxl::read_xls(i, range = "R52C1:R61C8", col_names = FALSE))
      } else if (file_format == "xlsx") {
        segments <- as.data.frame(readxl::read_xlsx(i, range = "R52C1:R61C8", col_names = FALSE))
      }
        
      segments <- segments[, c(1,2,4,6,8)]
      colnames(segments) <- c("substrat", "t1", "t2", "t3", "t4")
        
      }
      
      if(any(apply(segments[, names(segments) %in% c("t1", "t2", "t3", "t4")], 2, sum) != 40)) {
        stop(message(paste0("sum of PIT in one of transect during ", annee, "'s ", site, " survey is not 40")))
        return(NULL)
      }
      
    seg_gather <- tidyr::gather(segments, transect, value, -substrat)
    seg_spread <- tidyr::spread(seg_gather, substrat, value)
    
    n_annee    <- rep(annee, nrow(seg_spread))
    n_site     <- rep(site, nrow(seg_spread))
    
    df_line <- data.frame(cbind(site = n_site, annee = n_annee, seg_spread))
    df_line$annee <- as.integer(df_line$annee)
    
    return(df_line)
    
    })))
  
  return(data_line)

}

# This function import data count of fish and benthic organisms 
# of year_site_line.xls/xlsx files ---------------------------------------------

import_belt_function <- function(list_belt_files){
  
  data_belt <- data.frame(do.call(rbind, lapply(list_belt_files, function(i) {
   
    #i = "data/mayotte/14-LONGONI/16-0613/2016b_longoni_belt.xls"
    
    fish_name   <- c("Butterflyfish", "Haemulidae", "Snapper", "Barramundi cod", "Humphead wrasse", "Bumphead parrot", "Parrotfish", "Moray eel", "Grouper total")
    invert_name <- c("Banded coral shrimp", "Diadema urchin", "Pencil urchin", "Collector urchin", "Sea cucumber", "Crown-of-thorns", "Triton", "Lobster", "Giant clam total")
    
    file_name   <- basename(i)
    annee       <- gsub("[^0-9]", "", file_name)
    split       <- strsplit(file_name, "_")
    site        <- sapply(split, "[", 2)
    file_format <- sapply(strsplit(sapply(split, "[", 3), "\\."), "[", 2)
    
    message(paste(site, annee))
    
    # import from old format of data sheet
    if (file_format == "xls") {
      fish   <- as.data.frame(readxl::read_xls(i, range = "R10C1:R25C5" , col_names = FALSE))
      invert <- as.data.frame(readxl::read_xls(i, range = "R29C1:R45C5" , col_names = FALSE))
    
    } else if (file_format == "xlsx") {
      fish     <- as.data.frame(readxl::read_xlsx(i, range = "R10C1:R25C5", col_names = FALSE))
      invert   <- as.data.frame(readxl::read_xlsx(i, range = "R29C1:R45C5" , col_names = FALSE))
      
    }
    
    colnames(fish) <- c("fish", "t1", "t2", "t3", "t4")
    colnames(invert) <- c("invert", "t1", "t2", "t3", "t4")
    invert$invert[invert$invert == "Diadema"] <- "Diadema urchin"
    
    # If there is no match in taxon names, try to import from other format of data sheet  
    if (length(fish$fish[fish$fish %in% fish_name]) != length(fish_name) |
      length(invert$invert[invert$invert %in% invert_name]) != length(invert_name)) {
      
      if (file_format == "xls") {
        fish   <- as.data.frame(readxl::read_xls(i, range = "R11C1:R25C5" , col_names = FALSE))
        invert <- as.data.frame(readxl::read_xls(i, range = "R32C1:R48C5" , col_names = FALSE))
        
      } else if (file_format == "xlsx") {
        fish     <- as.data.frame(readxl::read_xlsx(i, range = "R11C1:R25C5", col_names = FALSE))
        invert   <- as.data.frame(readxl::read_xlsx(i, range = "R32C1:R48C5" , col_names = FALSE))
        
      }
      
    colnames(fish)   <- c("fish", "t1", "t2", "t3", "t4")
    colnames(invert) <- c("invert", "t1", "t2", "t3", "t4")
    
    fish$fish[fish$fish == "Total # grouper observed"] <- "Grouper total"
    invert$invert[invert$invert == "Total # giant clams observed"] <- "Giant clam total"
  
    }
    
    # fish ---------------------------------------------------------------------
    if (any(fish$fish[fish$fish %in% fish_name] == fish_name) == FALSE) {
      stop(message(paste0("Data import problem. Please chech data sheet survey of ", site, " ", annee)))
    }
    fish <- fish[fish$fish %in% fish_name, ]
    
    fish$fish[fish$fish == "Grouper total"] <- "Grouper"
    fish$fish <- tolower(fish$fish)
    fish$fish <- gsub(" ", "_", fish$fish)
    fish_name <- fish$fish
      
    fish_gather <- tidyr::gather(fish, transect, value, -fish)
    fish_spread <- tidyr::spread(fish_gather, fish, value)
      
    n_annee    <- rep(annee, nrow(fish_spread))
    n_site     <- rep(site, nrow(fish_spread))
      
    fish <- data.frame(cbind(site = n_site, annee = n_annee, fish_spread))
    fish$annee <- as.integer(fish$annee)
    
    if (any(is.na(fish[, names(fish) %in% fish_name]))) {
      
      message("Fish survey of ", site, " in ", annee, " included NA values. Please check data.")
  
    }
      
    # invert -------------------------------------------------------------------
    if (any(invert$invert[invert$invert %in% invert_name] == invert_name) == FALSE) {
      stop(message(paste0("Data import problem. Please chech data sheet survey of ", site, " ", annee)))
    }
    invert <- invert[invert$invert %in% invert_name, ]
    
    invert$invert[invert$invert == "Giant clam total"] <- "Giant clam"
    invert$invert <- tolower(invert$invert)
    invert$invert <- gsub(" ", "_", invert$invert)
    invert$invert <- gsub("-", "_", invert$invert)
      
    invert_gather <- tidyr::gather(invert, transect, value, -invert)
    invert_spread <- tidyr::spread(invert_gather, invert, value)
      
    n_annee    <- rep(annee, nrow(invert_spread))
    n_site     <- rep(site, nrow(invert_spread))
      
    invert <- data.frame(cbind(site = n_site, annee = n_annee, invert_spread))
    invert$annee <- as.integer(invert$annee)
    
    if (any(is.na(invert[, names(invert) %in% invert_name]))) {
      
      message("Invert survey of ", site, " in ", annee, " included NA values. Please check data.")
      
    }
    
    return(list(fish = fish, invert = invert))
      
  })))
  
  data_fish   <- data.frame(do.call(rbind, data_belt$fish))
  data_fish   <- na.omit(data_fish)
  data_invert <- data.frame(do.call(rbind, data_belt$invert))
  data_invert <- na.omit(data_invert)
  
  return(list(data_fish = data_fish, data_invert = data_invert))
  
}

import_line_may <- function(list_line_may) {
  
  #targets::tar_load(list_line_may)
  
  import_line_function(list_line_files = list_line_may)
  
}

import_belt_may <- function(list_belt_may) {
  
  #targets::tar_load(list_belt_may)
  
  import_belt_function(list_belt_files = list_belt_may)
  
}

import_line_run <- function(list_line_run) {
  
  #targets::tar_load(list_line_files)
  
  import_line_function(list_line_files = list_line_run)
  
}

import_belt_run <- function(list_belt_run) {
  
  #targets::tar_load(list_belt_files)
  
  import_belt_function(list_belt_files = list_belt_run)
  
}
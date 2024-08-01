
#' Import pit survey data ------------------------------------------------------
#'
#' @description
#' This function import data substrat cover from monthday-year_site_pit.xls/xlsx files
#' 
#' @param list_pit_files 
#'
#' @return a pit survey data frame 
#' 
#' @export

import_pit_function <- function(list_pit_files) {
  
  data_pit <- data.frame(do.call(rbind, lapply(list_pit_files, function(i) {
    
    #i = "data/mayotte/pit/24-0622_tanaraki_pit.xls"
    
    file_name   <- basename(i)
    annee       <- as.numeric(paste0("20", sapply(strsplit(file_name, "-"), "[", 1)))
    split       <- strsplit(file_name, "_")
    site        <- sapply(split, "[", 2)
    file_format <- sapply(strsplit(sapply(split, "[", 3), "\\."), "[", 2)

    message(paste(site, annee))
  
    if (file_format == "xls") {
      segments <- as.data.frame(readxl::read_xls(i, range = "R52C1:R61C8", col_names = FALSE))
    } else if (file_format == "xlsx") {
      segments <- as.data.frame(readxl::read_xlsx(i, range = "R52C1:R61C8", col_names = FALSE))
    }
        
    
    segments <- segments[, c(1,2,4,6,8)]
    colnames(segments) <- c("substrat", "t1", "t2", "t3", "t4")
        
    if(any(apply(segments[, names(segments) %in% c("t1", "t2", "t3", "t4")], 2, sum) != 40)) {
        stop(paste0("sum of PIT in one of transect during ", annee, "'s ", site, " survey is not 40"))
    }
      
    seg_gather <- tidyr::gather(segments, transect, value, -substrat)
    seg_spread <- tidyr::spread(seg_gather, substrat, value)

    bleaching  <- as.data.frame(readxl::read_xls(i, range = "F37:M37", col_names = FALSE))
    seg_spread$bleaching <- bleaching[!is.na(bleaching)]
    
    n_annee    <- rep(annee, nrow(seg_spread))
    n_site     <- rep(site, nrow(seg_spread))
    
    df_pit <- data.frame(cbind(site = n_site, annee = n_annee, seg_spread))
    
    df_pit$annee[df_pit$annee %in% c("2021", "2022")] <- "2021-2022"
    df_pit$annee[df_pit$annee %in% c("2023", "2024")] <- "2023-2024"

    #df_pit$annee <- as.integer(df_pit$annee)
    
    return(df_pit)
    
    })))
  
  if (nrow(data_pit) != length(list_pit_files) * 4) {
    
    stop("Problem during importation, length of data is not proportionnal to length of list files")
    
  }
  
  return(data_pit)

}


#' Import belt survey data -----------------------------------------------------
#' 
#' @description
#' This function import data count of fish and benthic 
#' organisms from monthday-year_site_belt.xls/xlsx files
#' 
#' @param list_belt_files 
#'
#' @return a belt survey data frame
#' 
#' @export

import_belt_function <- function(list_belt_files){
  
  data_belt <- data.frame(do.call(rbind, lapply(list_belt_files, function(i) {
   
    #i = "data/mayotte/8-NGOUJA/20-1104/2020_ngouja_belt.xls"
    
    fish_name   <- c("Butterflyfish", "Haemulidae", "Snapper", "Barramundi cod", "Humphead wrasse", "Bumphead parrot", "Parrotfish", "Moray eel", "Grouper total")
    invert_name <- c("Banded coral shrimp", "Diadema urchin", "Pencil urchin", "Collector urchin", "Sea cucumber", "Crown-of-thorns", "Triton", "Lobster", "Giant clam total")
    
    file_name   <- basename(i)
    annee       <- as.integer(gsub("[^0-9]", "", file_name))
    split       <- strsplit(file_name, "_")
    site        <- sapply(split, "[", 2)
    file_format <- sapply(strsplit(sapply(split, "[", 3), "\\."), "[", 2)
  
    message(paste(site, annee))
    
    # import from old format of data sheet
    if (file_format == "xls") {
      fish   <- as.data.frame(readxl::read_xls(i, range = "R10C1:R24C5" , col_names = FALSE))
      invert <- as.data.frame(readxl::read_xls(i, range = "R29C1:R45C5" , col_names = FALSE))
    
    } else if (file_format == "xlsx") {
      fish     <- as.data.frame(readxl::read_xlsx(i, range = "R10C1:R24C5", col_names = FALSE))
      invert   <- as.data.frame(readxl::read_xlsx(i, range = "R29C1:R45C5" , col_names = FALSE))
      
    }
    
    colnames(fish) <- c("fish", "t1", "t2", "t3", "t4")
    colnames(invert) <- c("invert", "t1", "t2", "t3", "t4")
    invert$invert[invert$invert == "Diadema"] <- "Diadema urchin"

    # fish ---------------------------------------------------------------------
    if (any(fish$fish[fish$fish %in% fish_name] != fish_name)) {
      stop(paste0("Data import problem. Please chech data sheet survey of ", site, " ", annee))
    }
    
    if (any(is.na(fish[fish$fish %in% fish_name, ]))) {
      
      message(paste0("Fish survey of ", site, " in ", annee, " included NA values. Please check data."))
      Sys.sleep(5)
      
    }
    
    fish <- fish[fish$fish %in% fish_name, ]
    
    fish$fish[fish$fish == "Grouper total"] <- "Grouper"
    fish$fish   <- tolower(fish$fish)
    fish$fish   <- gsub(" ", "_", fish$fish)
    fish_gather <- tidyr::gather(fish, transect, value, -fish)
    fish_spread <- tidyr::spread(fish_gather, fish, value)
    n_annee     <- rep(annee, nrow(fish_spread))
    n_site      <- rep(site, nrow(fish_spread))
      
    fish <- data.frame(cbind(site = n_site, annee = n_annee, fish_spread))


    # invert -------------------------------------------------------------------
    if (any(invert$invert[invert$invert %in% invert_name] != invert_name)) {
      
      stop(paste0("Data import problem. Please check data sheet survey of ", site, " ", annee))
      
    }
    
    if (any(is.na(invert[invert$invert %in% invert_name, ]))) {
      
      message(paste0("Invert survey of ", site, " in ", annee, " included NA values. Please check data."))
      Sys.sleep(5)
      
    }
    
    invert <- invert[invert$invert %in% invert_name, ]
    
    invert$invert[invert$invert == "Giant clam total"] <- "Giant clam"
    invert$invert <- tolower(invert$invert)
    invert$invert <- gsub(" ", "_", invert$invert)
    invert$invert <- gsub("-", "_", invert$invert)
    invert_gather <- tidyr::gather(invert, transect, value, -invert)
    invert_spread <- tidyr::spread(invert_gather, invert, value)
    n_annee       <- rep(annee, nrow(invert_spread))
    n_site        <- rep(site, nrow(invert_spread))
      
    invert <- data.frame(cbind(site = n_site, annee = n_annee, invert_spread))

    return(list(fish = fish, invert = invert))
      
  })))
  
  data_fish   <- data.frame(do.call(rbind, data_belt$fish))
  data_fish   <- na.omit(data_fish)
  data_fish$annee[data_fish$annee %in% c("2021", "2022")] <- "2021-2022"
  data_fish$annee[data_fish$annee %in% c("2023", "2024")] <- "2023-2024"
  data_invert <- data.frame(do.call(rbind, data_belt$invert))
  data_invert <- na.omit(data_invert)
  data_invert$annee[data_invert$annee %in% c("2021", "2022")] <- "2021-2022"
  data_invert$annee[data_invert$annee %in% c("2023", "2024")] <- "2023-2024"
  
  return(list(data_fish = data_fish, data_invert = data_invert))
  
}

# --- MAYOTTE ------------------------------------------------------------------

# subset fish data from belt data

import_fish_may <- function(data_belt_may) {
  
  #targets::tar_load(data_belt_may)
  
  data_belt_may$data_fish
  
}

# subset invert data from belt data

import_invert_may <- function(data_belt_may) {
  
  #targets::tar_load(data_belt_may)
  
  data_belt_may$data_invert
  
}

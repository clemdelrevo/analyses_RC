download_bd_topo_mayotte <- function() {
  
  #download at : https://geoservices.ign.fr/ressource/158205
  
  mayotte_shp <- "data/BDTOPO_3-0_TOUSTHEMES_SHP_RGM04UTM38S_D976_2021-03-15/BDTOPO/1_DONNEES_LIVRAISON_2021-03-00272/BDT_3-0_SHP_RGM04UTM38S_D976-ED2021-03-15/ADMINISTRATIF/REGION.shp"
  mayotte_shp
  
}

# --- GLOBAL REEF LOCATION -----------------------------------------------------

# download Millennium Coral Reef Mapping Project

#This dataset shows the global distribution of coral reefs in 
#tropical and subtropical regions

download_millenium_reef <- function(overwrite = TRUE){
  
  # download to https://data.unep-wcmc.org/datasets/1
  millenium_reef_url      <- paste0("https://datadownload-production.s3.us-east-1.",
                                    "amazonaws.com/WCMC008_CoralReefs2021_v4_1.zip")
  millenium_reef_path     <- "data/"
  millenium_reef_zip      <- "data/WCMC008_CoralReefs2021_v4_1.zip"
  download.file(url = millenium_reef_url, destfile = millenium_reef_zip,
                method = "curl")
  unzip(millenium_reef_zip, exdir = millenium_reef_path)
  unlink(millenium_reef_zip)
  millenium_reef_old_name <- "data/14_001_WCMC008_CoralReefs2021_v4_1"
  millenium_reef_new_name <- "data/millenium_reef"
  file.rename(millenium_reef_old_name, millenium_reef_new_name)
  millenium_reef_shp      <- paste0("data/millenium_reef/",
                                    "01_Data/WCMC008_CoralReef2021_Py_v4_1.shp")
  
  return(millenium_reef_shp)
  
}

download_carmayotte <- function() {
  
  carmayotte_url  <-  "https://sextant.ifremer.fr/sextant_data/data_QGIS/OCEAN_INDIEN/HYPERSPECTRALE/Carmayotte_2020_CUFR_MAREX_OFB/Carmayotte_2020_CUFR_MAREX_OFB.zip"
  carmayotte_zip  <- "data/Carmayotte_2020_CUFR_MAREX_OFB.zip"
  carmayotte_file_name  <- "data/carmayotte"
  dir.create(carmayotte_file_name, showWarnings = FALSE)
  
  download.file(url = carmayotte_url, destfile = carmayotte_zip, method = "curl")
  unzip(carmayotte_zip, exdir = carmayotte_file_name)
  unlink(carmayotte_zip)
  carmayotte_shp <- "data/carmayotte/Carmayotte_2020_CUFR_MAREX_OFB.shp"
  
  return(carmayotte_shp)
  
}
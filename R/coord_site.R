get_coord_site_may <- function() {
  
  mbouzi       <- c(x = 45.237066, y = -12.817251)
  tzoundzou    <- c(x = 45.207440, y = -12.816770)
  pes.deux     <- c(x = 45.271563, y = -12.864071)
  pes.onze     <- c(x = 45.269563, y = -12.869418)
  sakouli      <- c(x = 45.21243, y = -12.89404)
  bandrele     <- c(x = 45.203500, y = -12.862630)
  saziley      <- c(x = 45.18378, y = -12.98530)
  ngouja       <- c(x = 45.083588, y = -12.963810)
  boueni       <- c(x = 44.966730, y = -12.937130)
  mtsangamouji <- c(x = 45.082837, y = -12.768401)
  tanaraki     <- c(x = 45.065181, y = -12.760835)
  boa          <- c(x = 45.038680, y = -12.685780)
  dzoumogne    <- c(x = 45.133490, y = -12.714240)
  longoni      <- c(x = 45.165660, y = -12.710100)
  majikavo     <- c(x = 45.233090, y = -12.740390)
  
  
  coord_site_may <- data.frame(t(cbind(mbouzi, tzoundzou, pes.deux, pes.onze, sakouli,
                                       bandrele, saziley, ngouja, boueni, mtsangamouji,
                                       tanaraki, boa, dzoumogne, longoni, majikavo)))
  coord_site_may$site <- rownames(coord_site_may)
  rownames(coord_site_may) <- 1:nrow(coord_site_may)
  
  return(coord_site_may)
  
}

get_coord_site_run <- function() {
  
  tessier.pe <- c(x = 55.247514, y = -21.110174)
  
  coord_site_run <- data.frame(t(cbind(tessier.pe)))
  coord_site_run$site <- rownames(coord_site_run)
  rownames(coord_site_run) <- 1:nrow(coord_site_run)
  
  return(coord_site_run)
  
}
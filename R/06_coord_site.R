# --- GPS COORDINATES SITES -----------------------------------------------------

# get the gps coordinates used in 07_coral_cover_evolution.R file 

get_coord_site_may <- function() {
  
  mbouzi       <- c(x = 45.237066, y = -12.817251)
  tzoundzou    <- c(x = 45.207440, y = -12.816770)
  pes.deux     <- c(x = 45.271563, y = -12.864071)
  pes.onze     <- c(x = 45.269563, y = -12.869418)
  sakouli      <- c(x = 45.212430, y = -12.89404)
  bandrele     <- c(x = 45.203500, y = -12.862630)
  saziley      <- c(x = 45.183780, y = -12.98530)
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
  
  bassin.pirogue.pl       <- c(x = 55.333010, y = -21.269200)
  brisants.pe             <- c(x = 55.327800, y = -21.264200)
  boucan.canot.pe         <- c(x = 55.226367, y = -21.025533)
  boucan.canot.pl         <- c(x = 55.227083, y = -21.026250)
  cap.la.houssaye.pe      <- c(x = 55.238200, y = -21.018200)
  ermitage.passe.pe       <- c(x = 55.220580, y = -21.085430)
  ermitage.passe.pl       <- c(x = 55.224800, y = -21.084283)
  la.corne.etimareco.pe   <- c(x = 55.281931, y = -21.165939)
  la.saline.pe            <- c(x = 55.217231, y = -21.069811)
  la.saline.pl            <- c(x = 55.249680, y = -21.073217)
  livingstone.pe          <- c(x = 55.231400, y = -21.099831)
  livingstone.pl          <- c(x = 55.236083, y = -21.096983)
  passe.ermitage.pe       <- c(x = 55.220580, y = -21.085430)
  passe.ermitage.pl       <- c(x = 55.224800, y = -21.084283)
  ravine.des.poux.etimareco.pe <- c(x = 55.284240, y = -21.175053)
  roches.noires.pe        <- c(x = 55.221119, y = -21.053069)
  roches.noires.pl        <- c(x = 55.222083, y = -21.051250)
  spot.pe                 <- c(x = 55.285000, y = -21.164100)
  tessier.pe              <- c(x = 55.247514, y = -21.110174)
  tessier.pl              <- c(x = 55.249680, y = -21.107580)
  trou.d.eau.pe           <- c(x = 55.239167, y = -21.105767)
  trou.d.eau.pl           <- c(x = 55.243233, y = -21.102917)
  trou.d.eau.etimareco.pe <- c(x = 55.239540, y = -21.106160)
  trou.d.eau.etimareco.pl <- c(x = 55.242294, y = -21.103311)
  
  
  
  coord_site_run <- data.frame(t(cbind(
                                       bassin.pirogue.pl,
                                       brisants.pe,
                                       boucan.canot.pe, 
                                       boucan.canot.pl, 
                                       cap.la.houssaye.pe,
                                       ermitage.passe.pe,
                                       ermitage.passe.pl,
                                       la.corne.etimareco.pe,
                                       la.saline.pe,
                                       la.saline.pl,
                                       livingstone.pe,
                                       livingstone.pl,
                                       passe.ermitage.pe,
                                       passe.ermitage.pl,
                                       ravine.des.poux.etimareco.pe,
                                       roches.noires.pe,
                                       roches.noires.pl,
                                       tessier.pe, 
                                       tessier.pl,
                                       trou.d.eau.pe,
                                       trou.d.eau.pl,
                                       trou.d.eau.etimareco.pe,
                                       trou.d.eau.etimareco.pl
                                       )))
  
  coord_site_run$site <- rownames(coord_site_run)
  rownames(coord_site_run) <- 1:nrow(coord_site_run)
  
  return(coord_site_run)
  
}
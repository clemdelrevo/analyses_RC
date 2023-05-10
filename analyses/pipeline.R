library(targets)

#targets options
tar_option_set(format = "qs")

# functions and options
tar_source()
options(mc.cores = 4)

# pipeline
list(
  
  ## download data
  tar_target(data_cover, download_cover())
  
)
library(targets)

#targets options
tar_option_set(format = "qs")

# functions and options
tar_source()
options(mc.cores = 5)

# pipeline
list(
  
  ## list files
   tar_target(list_line_files, get_list_line_files(), format = "file")
  ,tar_target(list_belt_files, get_list_belt_files(), format = "file")
  
  ## import data
  ,tar_target(data_line, import_line(list_line_files))
  ,tar_target(data_belt, import_belt(list_belt_files))
  
)
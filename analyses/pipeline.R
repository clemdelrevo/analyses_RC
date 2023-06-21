library(targets)

#targets options
tar_option_set(format = "qs")

# functions and options
tar_source()
options(mc.cores = 5)

# pipeline
list(
  
  ## list files ---
   tar_target(list_line_files, get_list_line_files(), format = "file")
  ,tar_target(list_belt_files, get_list_belt_files(), format = "file")
  
  ## import data ---
  ,tar_target(data_line, import_line(list_line_files))
  ,tar_target(data_belt, import_belt(list_belt_files))
  
  ## reef type ---
  ,tar_target(reef_type_may, get_reef_type_mayotte())
  
  ## coral_cover_timeline ---
  ,tar_target(may_coral_cover, get_mayotte_coral_cover_timeline(data_line, reef_type_may))
  
  ## cc_timeline_graph ---
  ,tar_target(may_graph_cc, get_may_graph_cc(may_coral_cover))
)
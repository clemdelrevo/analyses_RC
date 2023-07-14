library(targets)

#targets options
tar_option_set(format = "qs")

# sf options
sf::sf_use_s2(FALSE)

# functions and options
tar_source()
options(mc.cores = 5)

# pipeline
list(
  
  ## download ---
   tar_target(mayotte_shp, download_bd_topo_mayotte(), format = "file")
  ,tar_target(millenium_reef_shp, download_millenium_reef(), format = "file")
  
  ## wrangle data ---
  ,tar_target(mayotte_bd, wrangle_mayotte_shp(mayotte_shp))
  ,tar_target(may_reef, wrangle_may_reef(millenium_reef_shp))
  
  ## list files ---
  ,tar_target(list_line_files, get_list_line_files(), format = "file")
  ,tar_target(list_belt_files, get_list_belt_files(), format = "file")
  
  ## import data ---
  ,tar_target(data_line, import_line(list_line_files))
  ,tar_target(data_belt, import_belt(list_belt_files))
  
  ## coordonates site ---
  ,tar_target(coord_site_may, get_coord_site_may())
  
  ## reef type ---
  ,tar_target(reef_type_may, get_reef_type_mayotte())
  
  ## coral_cover_timeline ---
  ,tar_target(may_coral_cover, get_mayotte_coral_cover_timeline(data_line, reef_type_may))
  
  ## cc_timeline_graph ---
  ,tar_target(may_graph_cc, get_may_graph_cc(may_coral_cover))
  
  ## cc_diff_site ---
  ,tar_target(may_cc_diff_site, get_may_cc_diff_site(data_line, reef_type_may, coord_site_may))
  
  ## map_diff_cc ---
  ,tar_target(may_diff_cc_map, get_map_diff_cc(mayotte_bd, may_reef, may_cc_diff_site))
  
  ## figure ---
  ,tar_target(may_diff_cc_dot, get_may_diff_cc_dot(may_cc_diff_site))
)
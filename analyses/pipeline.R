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

  ## list files ---
  ,tar_target(list_line_may, get_list_line_may(), format = "file")
  ,tar_target(list_belt_may, get_list_belt_may(), format = "file")
  
  ## import data ---
  ,tar_target(data_line_may, import_line_may(list_line_may))
  ,tar_target(data_belt_may, import_belt_may(list_belt_may))
  
  ## wrangle data ---
  ,tar_target(mayotte_bd, wrangle_mayotte_shp(mayotte_shp))
  ,tar_target(may_reef, wrangle_may_reef(millenium_reef_shp))
  ,tar_target(data_fish, get_data_fish(data_belt_may, reef_type_may))
  ,tar_target(data_invert, get_data_invert(data_belt_may, reef_type_may))
  
  ## coordinates site ---
  ,tar_target(coord_site_may, get_coord_site_may())
  
  ## reef type ---
  ,tar_target(reef_type_may, get_reef_type_mayotte())
  
  ## coral cover timeline ---
  ,tar_target(may_pourc_cc, get_mayotte_cc_timeline(data_line_may, reef_type_may))
  ,tar_target(may_graph_cc, get_may_graph_cc(may_pourc_cc))
  ,tar_target(may_final_cc_timeline, get_may_final_cc_timeline(may_graph_cc, survey_may_cc))
  
  ## n survey timeline ---
  ,tar_target(survey_may_cc, get_n_survey_may_cc(may_pourc_cc))
  ,tar_target(survey_may_fish, get_n_survey_may_fish(fish_abondance))

  ## map_cc_diff_site ---
  ,tar_target(may_cc_diff_site, get_may_cc_diff_site(data_line_may, reef_type_may, coord_site_may))
  ,tar_target(may_diff_cc_map, get_map_diff_cc(mayotte_bd, may_reef, may_cc_diff_site))
  ,tar_target(may_diff_cc_dot, get_may_diff_cc_dot(may_cc_diff_site))
  
  ## fish abondance timeline ---
  ,tar_target(fish_abondance, get_fish_abondance(data_fish))
  ,tar_target(fish_trophic_abondance, get_fish_trophic_abondance(data_fish))
  
  ## fish_timeline_graph --
  ,tar_target(may_graph_fish, get_may_graph_fish(fish_abondance, fish_trophic_abondance))
  
  ## export figure ---
  ,tar_target(export_figure, get_export_figure(may_diff_cc_map, may_diff_cc_dot))
)
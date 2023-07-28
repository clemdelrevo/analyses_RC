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
    ### Mayotte ---
    tar_target(mayotte_shp, download_bd_topo_mayotte(), format = "file")
    ,tar_target(millenium_reef_shp, download_millenium_reef(), format = "file")
    ,tar_target(carmayotte_shp, download_carmayotte(), format = "file")

  ## list files ---
    ### Mayotte ---
    ,tar_target(list_line_may, get_list_line_may(), format = "file")
    ,tar_target(list_belt_may, get_list_belt_may(), format = "file")
  
  ## import data ---
    ### Mayotte ---
    ,tar_target(data_line_may, import_line_may(list_line_may))
    ,tar_target(data_belt_may, import_belt_may(list_belt_may))
    ,tar_target(data_fish_may, import_fish_may(data_belt_may))
    ,tar_target(data_invert_may, import_invert_may(data_belt_may))
  
  ## coordinates site ---
    ### Mayotte ---
    ,tar_target(coord_site_may, get_coord_site_may())
  
  ## reef type ---
    ### Mayotte ---
    ,tar_target(reef_type_may, get_reef_type_mayotte())
  
  ## wrangle data ---
    ### Mayotte ---
    ,tar_target(mayotte_bd, wrangle_mayotte_shp(mayotte_shp))
    ,tar_target(may_reef, wrangle_may_reef(millenium_reef_shp))
    ,tar_target(line_may, get_data_line(data_line_may, reef_type_may))
    ,tar_target(fish_may, get_data_fish(data_fish_may, reef_type_may))
    ,tar_target(invert_may, get_data_invert(data_invert_may, reef_type_may))

  
  ## coral cover timeline ---
    ### Mayotte ---
    ,tar_target(may_pourc_cc, get_mayotte_cc_timeline(line_may))
    ,tar_target(may_graph_cc, get_may_graph_cc(may_pourc_cc))
    ,tar_target(may_final_cc_timeline, get_may_final_cc_timeline(may_graph_cc, survey_may_cc))
  
  ## n survey timeline ---
    ### Mayotte ---
    ,tar_target(survey_may_cc, get_n_survey_may_cc(may_pourc_cc))
    ,tar_target(survey_may_fish, get_n_survey_may_fish(fish_abondance))

  ## map coral cover evolution ---
    ### Mayotte ---
    ,tar_target(may_cc_evol, get_may_cc_evol(line_may, reef_type_may, coord_site_may, mayotte_bd, may_reef))
  
  ## fish abondance timeline ---
    ### Mayotte ---
    ,tar_target(fish_abondance, get_fish_abondance(fish_may))
    ,tar_target(fish_trophic_abondance, get_fish_trophic_abondance(fish_may))
    ,tar_target(may_graph_fish, get_may_graph_fish(fish_abondance, fish_trophic_abondance))
    ,tar_target(may_final_fish_timeline, get_may_final_fish_timeline(may_graph_fish, survey_may_fish))
  
  ## station survey ---
    ### Mayotte --- 
    ,tar_target(may_substrat_survey, substrat_may_station(data_line_may))
    ,tar_target(may_fish_survey, get_may_fish_station(fish_may))
    ,tar_target(may_invert_survey, get_may_invert_station(invert_may))
  
  ## export figure ---
  ,tar_target(export_may, get_export_figure(data_line_may, data_fish_may, data_invert_may,
                                            may_cc_evol, 
                                            may_final_cc_timeline, may_final_fish_timeline,
                                            may_substrat_survey, may_fish_survey, may_invert_survey))
)
library(targets)
library(tarchetypes)

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
  tar_target(millenium_reef_shp, download_millenium_reef(), format = "file")
  ### Mayotte ---
  ,tar_target(mayotte_shp, download_bd_topo_mayotte(), format = "file")

  ## list files ---
  ### Mayotte ---
  ,tar_target(list_pit_may, get_list(path = "data/mayotte/pit"), format = "file")
  ,tar_target(list_belt_may, get_list(path = "data/mayotte/belt"), format = "file")
  
  ## import data ---
  ### Mayotte ---
  ,tar_target(data_pit_may, import_pit_function(list_pit_files = list_pit_may))
  ,tar_target(data_belt_may, import_belt_function(list_belt_files = list_belt_may))
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
  ,tar_target(
    pit_may, 
    add_reef_type_may(
      data     = data_pit_may,
      fringing = reef_type_may$fringing_may,
      barrier  = reef_type_may$barrier_may,
      intern   = reef_type_may$intern_may
      )
    )
  ,tar_target(
    fish_may,
    add_reef_type_may(
      data     = data_fish_may,
      fringing = reef_type_may$fringing_may,
      barrier  = reef_type_may$barrier_may,
      intern   = reef_type_may$intern_may
      )
    )
  ,tar_target(
    invert_may,
    add_reef_type_may(
      data     = data_invert_may,
      fringing = reef_type_may$fringing_may,
      barrier  = reef_type_may$barrier_may,
      intern   = reef_type_may$intern_may
      )
    )
  
  ,tar_target(mayotte_bd, wrangle_mayotte_shp(mayotte_shp))
  ,tar_target(may_reef, wrangle_may_reef(millenium_reef_shp))


  ## coral cover timeline ---
    ### Mayotte ---
    ,tar_target(pourc_cc_may, cc_timeline_function(data = pit_may))
    ,tar_target(graph_cc_may, get_graph_cc(pourc_cc_region = pourc_cc_may))
    ,tar_target(
      final_cc_timeline_may, 
      get_final_cc_timeline(
        graph_cc_region = graph_cc_may, 
        nb_survey_cc_region = nb_survey_cc_may
        )
      )

  ## n survey timeline ---
    ### Mayotte ---
  ,tar_target(nb_survey_cc_may, get_nb_survey_cc_may(pourc_cc_may))
  ,tar_target(nb_survey_fish_may, get_nb_survey_fish_may(fish_abondance_may))
  ,tar_target(nb_survey_invert_may, get_nb_survey_invert_may(invert_abondance_may)) 

  ## color ---
  ,tar_target(color_substrat, get_color_substrat())
  ,tar_target(french_fish_name, get_french_fish_name())
  ,tar_target(french_invert_name, get_french_invert_name())
  
  ## map coral cover evolution ---
  ### Mayotte ---
  ,tar_target(cc_evol_may, get_cc_evol_may(pit_may, reef_type_may, coord_site_may, mayotte_bd, may_reef))

  ## fish abondance timeline ---
  ### Mayotte ---
  ,tar_target(fish_abondance_may, get_fish_abondance(fish_region = fish_may))
  ,tar_target(graph_fish_may, get_graph_fish(fish_abondance_region = fish_abondance_may))
  ,tar_target(
    final_fish_timeline_may,
    get_final_fish_timeline(
      graph_fish_region = graph_fish_may,
      nb_survey_fish_region = nb_survey_fish_may
      )
    )

  ## invert abondance timeline ---
  ### Mayotte ---
  ,tar_target(invert_abondance_may, get_invert_abondance(invert_region = invert_may))
  ,tar_target(graph_invert_may, get_graph_invert(invert_abondance_region = invert_abondance_may))
  ,tar_target(
    final_invert_timeline_may,
    get_final_invert_timeline(
      graph_invert_region = graph_invert_may,
      nb_survey_invert_region = nb_survey_invert_may
    )
  )
  
  ## station survey ---
  ### Mayotte ---
  ,tar_target(substrat_survey_may, camenbert_function(data = data_pit_may, color = color_substrat))
  ,tar_target(fish_survey_may, stat_bar_function(data = data_fish_may, taxon_name = french_fish_name, taxon = "fish"))
  ,tar_target(invert_survey_may, stat_bar_function(data = data_invert_may, taxon_name = french_invert_name, taxon = "invert"))


 )
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
    ,tar_target(carmayotte_shp, download_carmayotte(), format = "file")
    ### Réunion ---
    ,tar_target(reunion_shp, download_bd_topo_reunion(), format = "file")
    ,tar_target(reunion_reef_gpkg, download_reunion_reef(), format = "file")
    ,tar_target(bati_run_shp, download_bati_run(), format = "file")
     
  ## list files ---
    ### Mayotte ---
    ,tar_target(list_pit_may, get_list_pit_may(), format = "file")
    ,tar_target(list_belt_may, get_list_belt_may(), format = "file")
    ### Réunion ---
    ,tar_target(list_pit_run, get_list_pit_run(), format = "file")
    ,tar_target(list_belt_run, get_list_belt_run(), format = "file")
  
  ## import data ---
    ### Mayotte ---
    ,tar_target(data_pit_may, import_pit_may(list_pit_may))
    ,tar_target(data_belt_may, import_belt_may(list_belt_may))
    ,tar_target(data_fish_may, import_fish_may(data_belt_may))
    ,tar_target(data_invert_may, import_invert_may(data_belt_may))
    ### Réunion ---
    ,tar_target(data_pit_run, import_pit_run(list_pit_run))
    #,tar_target(data_belt_run, import_belt_run(list_belt_run))
    #,tar_target(data_fish_run, import_fish_run(data_belt_run))
    #,tar_target(data_invert_run, import_invert_run(data_belt_run))
  
  ## coordinates site ---
    ### Mayotte ---
    ,tar_target(coord_site_may, get_coord_site_may())
    ,tar_target(coord_site_run, get_coord_site_run())
  
  ## reef type ---
    ### Mayotte ---
    ,tar_target(reef_type_may, get_reef_type_mayotte())
    ,tar_target(reef_type_run, get_reef_type_run())
  
  ## wrangle data ---
    ### Mayotte ---
    ,tar_target(mayotte_bd, wrangle_mayotte_shp(mayotte_shp))
    ,tar_target(may_reef, wrangle_may_reef(millenium_reef_shp))
    ,tar_target(pit_may, get_data_pit_may(data_pit_may, reef_type_may))
    ,tar_target(fish_may, get_data_fish_may(data_fish_may, reef_type_may))
    ,tar_target(invert_may, get_data_invert_may(data_invert_may, reef_type_may))
    ### Réunion ---
    ,tar_target(reunion_bd, wrangle_reunion_shp(reunion_shp))
    ,tar_target(run_reef, wrangle_reunion_reef_gpkg(reunion_reef_gpkg))
    ,tar_target(bati_run, wrangle_bati_run_shp(bati_run_shp))
    ,tar_target(pit_run, get_data_pit_run(data_pit_run, reef_type_run))
  
  ## coral cover timeline ---
    ### Mayotte ---
    ,tar_target(pourc_cc_may, get_cc_timeline_may(pit_may))
    ,tar_target(graph_cc_may, get_graph_cc_may(pourc_cc_may))
    ,tar_target(final_cc_timeline_may, get_final_cc_timeline_may(graph_cc_may, nb_survey_cc_may))
    ### Réunion ---
    ,tar_target(pourc_cc_run, get_cc_timeline_run(pit_run))
    ,tar_target(graph_cc_run, get_graph_cc_run(pourc_cc_run))
    ,tar_target(final_cc_timeline_run, get_final_cc_timeline_run(graph_cc_run, nb_survey_cc_run))
  
  ## n survey timeline ---
    ### Mayotte ---
    ,tar_target(nb_survey_cc_may, get_nb_survey_cc_may(pourc_cc_may))
    ,tar_target(nb_survey_fish_may, get_nb_survey_fish_may(fish_abondance_may))
    ### Réunion ---
    ,tar_target(nb_survey_cc_run, get_nb_survey_cc_run(pourc_cc_run))

  ## map coral cover evolution ---
    ### Mayotte ---
    ,tar_target(cc_evol_may, get_cc_evol_may(pit_may, reef_type_may, coord_site_may, mayotte_bd, may_reef))
    ### Réunion ---
    ,tar_target(cc_evol_run, get_cc_evol_run(pit_run, reef_type_run, coord_site_run, reunion_bd, run_reef, bati_run))
  
  ## fish abondance timeline ---
    ### Mayotte ---
    ,tar_target(fish_abondance_may, get_fish_abondance_may(fish_may))
    ,tar_target(graph_fish_may, get_graph_fish_may(fish_abondance_may))
    ,tar_target(final_fish_timeline_may,get_final_fish_timeline_may(graph_fish_may, nb_survey_fish_may))
  
  ## station survey ---
    ### Mayotte --- 
    ,tar_target(substrat_survey_may, substrat_station_may(data_pit_may))
    ,tar_target(fish_survey_may, fish_station_may(data_fish_may))
    ,tar_target(invert_survey_may, invert_station_may(data_invert_may))
  
  ## export figure ---
  #,tar_target(export_may, get_export_figure(line_may, fish_may, invert_may,
                                            #cc_evol_may, 
                                            #final_cc_timeline_may, final_fish_timeline_may,
                                            #substrat_survey_may, fish_survey_may, invert_survey_may))
  
  #,tar_quarto(report, "rc_report.qmd")
  
)
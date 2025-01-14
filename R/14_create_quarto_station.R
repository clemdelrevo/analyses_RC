
create_fist_page_carto <- function(mayotte_bd, may_reef, pit_may, reef_type_may, coord_site_may) {
  
  may_reef <- sf::st_union(may_reef)
  
  rc_station <- get_cc_evol(
    pit_region = pit_may, 
    coord_site = coord_site_may,
    fringing   = reef_type_may$fringing_may,
    barrier    = reef_type_may$barrier_may,
    intern     = reef_type_may$intern_may,
    slope      = NULL,
    flat       = NULL
  ) |>
    dplyr::select(site, reef_type)
  
  rc_station$site <- stringr::str_to_title(rc_station$site)
  rc_station$site <- stringr::str_replace(rc_station$site, 'Pes.deux', 'Passe en S bouée 2')
  rc_station$site <- stringr::str_replace(rc_station$site, 'Pes.onze', 'Passe en S bouée 11')
  rc_station$site <- stringr::str_replace(rc_station$site, 'Bandrele', 'Bandrélé')
  
  coord <- sf::st_coordinates(rc_station)
  rc_point_text <- cbind(sf::st_drop_geometry(rc_station), coord)
  
  g <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = mayotte_bd, fill = '#9dd1a7', color = '#9dd1a7') +
    ggplot2::geom_sf(data = may_reef, fill = '#fcefc0', linewidth = 0.1) +
    ggplot2::geom_sf(data = rc_station, ggplot2::aes(color = reef_type), size = 2.5) +
    ggplot2::scale_color_manual(values = c('#3edc77', '#0da3e7', '#e7770d'), labels = c("Barrière", "Frangeant", "Interne")) +
    ggplot2::labs(
      title = 'Carte des stations Reef Check de Mayotte', 
      color = 'Complexe récifal',
      caption = 'Récifs: Andrefouët et al. 2008\nTerres: BD Topo\nDatum: UTM zone 38S') +
    ggrepel::geom_text_repel(
      data = rc_point_text, 
      ggplot2::aes(x = X, y = Y, label = site), 
      nudge_x = c(0, -0.02, -0.02, 0, 0.005, 0.02, 0.02, -0.04, -0.02, 0.1, 0.1, 0.04, 0, -0.04, -0.02),
      nudge_y = c(0, 0.01, 0.01, 0.03, 0.02, 0.02, 0.02, -0.02, -0.02, 0.02, -0.04, -0.04, -0.02, 0.01, 0.02),
      size = 3.5) +
    ggspatial::annotation_scale() +
    ggspatial::annotation_north_arrow(
      location = 'tr', 
      height = ggplot2::unit(0.7, 'cm'), 
      width = ggplot2::unit(0.7, 'cm')
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#FFFFFF"),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = 'bold'),
      plot.caption = ggplot2::element_text(vjust = 60, hjust = 0.95, size = 8),
      legend.key = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = 'bold'),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(hjust = 0.5),
      legend.title.position = "bottom"
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(title.hjust = 0.5))
  
  g
  
  ggplot2::ggsave(filename = "outputs/mayotte/first_page_carto.jpg", plot = g, dpi = 400, width = 8, height = 9)
  
}

write_quarto <- function(bilan_station, data_pit_may, fist_page_carto, warning = TRUE) {

  #targets::tar_load(bilan_station)
  #targets::tar_load(data_pit_may)
  
  file.create("rc_report.qmd")
  
  site <- c(
    "bandrele", "boa", "boueni", "dzoumogne", "longoni", "majikavo", "mbouzi",       
    "mtsangamouji", "ngouja", "pes.deux", "pes.onze", "sakouli", "saziley",      
    "tanaraki", "tzoundzou"
    )
  
  type <- c(
    "Frangeant", "Interne", "Barrière", "Frangeant", "Interne", "Frangeant", "Frangeant",
    "Frangeant", "Frangeant", "Barrière", "Barrière", "Frangeant", "Frangeant",
    "Frangeant", "Frangeant"
  )
  
  pression <- c(
    "Moyenne", "Forte", "Faible", "Faible", "Moyenne", "Moyenne", "Très forte",
    "Moyenne", "Faible", "Faible", "Faible", "Faible", "Très Faible", 
    "Faible", "Faible"
  )
  
  correct_name <- stringr::str_to_upper(site)
  correct_name <- stringr::str_replace(correct_name, "PES.DEUX", "PASSE EN S BOUÉE 2")
  correct_name <- stringr::str_replace(correct_name, "PES.ONZE", "PASSE EN S BOUÉE 11")
  correct_name <- stringr::str_replace(correct_name, "MBOUZI", "M'BOUZI")
  correct_name <- stringr::str_replace(correct_name, "MTSANGAMOUJI", "M'TSANGAMOUJI")
  correct_name <- stringr::str_replace(correct_name, "BANDRELE", "BANDRÉLÉ")
  correct_name <- stringr::str_replace(correct_name, "NGOUJA", "N'GOUJA")
  
  names(correct_name) <- site
  names(type)         <- site
  names(pression)     <- site
  
  
  yaml <- glue::glue(
  "---
  title: 'Bilan des stations Reef Check'
  subtitle: 'Mayotte 2023-2024'
  format:
    pdf: default
  echo: FALSE
  geometry: 
    - top=16mm
    - left=15mm
    - right=15mm
    - bottom=20mm
  
  ---

  # [Auteurs]{.underline}
  
  Clément Delamare: Service de Plongée Scientifique  
  Sebastien Quaglietti: Service de Plongée Scientifique
  
  # [Contributeurs]{.underline}
  
  Flavien Foncin, Francois-Elie Paute, Léa Bernagou, Anna Roger, Charles Le Bozec, Annabelle Lapostolle, Dimitri Theuerkauff, Clémentine Cardon.
  
  # [Financeur]{.underline}
  
  **Direction de l’Environnement, de l’Aménagement, du Logement et de la Mer de Mayotte (DEALM)**  
  Terre-plein de M'tsapéré  
  97600 Mamoudzou  
  <deal-mayotte@developpement-durable.gouv.fr>
  
  # [Accès aux données]{.underline}
  
  Le code reproduisant les analyses est disponible à l'adresse suivante: <https://github.com/clemdelrevo/analyses_RC.git>. PDF généré avec [Quarto](https://quarto.org).
  
  ::: {layout-ncol='3'}
  ![](rc_mayotte.jpeg){width='100' fig-align='left'}
  
  ![](rc_france.jpeg){width='200' fig-align='center'}
  
  ![](deal.png){width='100' fig-align='right'}
  :::
  
  {{< pagebreak >}}
  
  ![](outputs/mayotte/first_page_carto.jpg){fig-align='center' width=110% heigth=110%}
  
  {{< pagebreak >}}

  ",
  .open = "<<", .close = ">>"
  )
  
  final_bilan <- glue::glue_collapse(unlist(lapply(names(correct_name), function(i) {
  
    #i = "bandrele"
    dates <- unique(data_pit_may$standard_date[data_pit_may$site == i])
    last_date   <- dates[length(dates)]
    before_date <- dates[length(dates) - 1]
    cn <- correct_name[names(correct_name) %in% i]
    t  <- type[names(type) %in% i]
    p  <- pression[names(pression) %in% i]
    bilan <- bilan_station[[i]]
    
    survey_sheet <- (
    "
  
    # <<cn>>
    
    :::{layout-ncol=2}
  
    #
  
    - Date du suivi: <<last_date>>
    - Suivi précédant: <<before_date>>
  
    # 
  
    - Type de récif: <<t>>
    - Pressions issues du bassin versant: <<p>>
    
    :::
    
    ## Composition du substrat en 2024 et évolution du recouvrement en corail dur
    
    :::: {layout='[30, -2, 30]' fig-pos='H'}
    ::: {#first-column}
    <<bilan$cc>>
    :::
    
    ::: {#fig-sub-<<i>>}
    ![](outputs/mayotte/substrat/<<i>>_substrat_survey.png){width=90%}
    
    Composition du substrat en 2024 (% moyen)
    :::
    ::::
    
    \
    
    ::: {#fig-cc-<<i>> fig-pos='H'}
    ![](outputs/mayotte/coral_cover/<<i>>_coral_cover.png){width=80%}
    
    Évolution du recouvrement en corail dur (%). Les points représentent le % de recouvrement en corail dur de chaque transect.
    :::

    {{< pagebreak >}}
    
    :::: {layout='[[10, -1, 20], [10, -1, 20]]' fig-pos='H'}
    
    ## Communautés ichtyologiques en 2024 et évolution
    
    ::: {#first-column}
    <<bilan$fish>>
    :::
    
    ::: {#fig-fish-<<i>>}
    ![](outputs/mayotte/fish/<<i>>_fish_survey.png)
    
    Évolution de la densité de poissons indicateurs. Barres d'erreurs = erreur standard.
    :::

    ## Communautés benthiques en 2024 et évolution
    ::: {#first-column}
    <<bilan$invert>>
    :::
    
    ::: {#fig-invert-<<i>>}
    ![](outputs/mayotte/invert/<<i>>_invert_survey.png)
    
    Évolution de la densité d'invertébrés indicateurs. Barres d'erreurs = erreur standard.
    :::
    ::::
    
    {{< pagebreak >}}
    
    "
    )
  
    glue_survey_sheet <- glue::glue(survey_sheet, .open = "<<", .close = ">>")
    
  })))
  
  if (warning) {
  
    warning_callout <- glue::glue(
      "
      
      # Remarques
      
      ::: {.callout-warning}
      
      ## Alerte prolifération d'algues
      
      La campagne 2023-2024 du suivi des stations Reef Check met en avant une augmentation
      importante du taux de recouvrement en macroalgues sur certains sites. L'augmentation de la
      part d'algues dréssées est souvent un marqueur d'eutrophisation et indique la présence d'apports azotés
      et phosphatés dans les eaux de ruissellement venant du bassin versant. Les sites concernés 
      doivent faire l'attention d'un suivi attentif et continus afin d'observer 
      la trajectoire environnementale que prend l'écosystème récifal.
      
      :::
      
      <br>
      
      ![Évolution du recouvrement en macroalgues sur les stations Reef Check de Mayotte. Les boxplot rouges montrent le recouvrement en algues recensé lors de la dernière campagne de suivis.](outputs/mayotte/nia_cover.png){#fig-warning-alga}
      
      ",
      .open = "<<", .close = ">>"
      )
  
  } else warning_callout <- NULL 
    
  final_sheet <- glue::glue_collapse(c(yaml, final_bilan, warning_callout))
  
  writeLines(final_sheet, "rc_report.qmd")
  
}
